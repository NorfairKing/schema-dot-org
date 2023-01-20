{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module SchemaDotOrg.Generator (schemaDotOrgGenerator) where

import Data.Aeson as JSON (eitherDecode')
import qualified Data.ByteString as SB
import qualified Data.ByteString.Lazy as LB
import Data.Char
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as S
import Data.String
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import GHC (runGhc)
import GHC.Driver.Session (getDynFlags)
import qualified GHC.Paths as GHC (libdir)
import GHC.SourceGen
import SchemaDotOrg.Generator.Schema
import System.Environment (getArgs)
import System.Exit
import System.Process

schemaDotOrgGenerator :: IO ()
schemaDotOrgGenerator = do
  args <- getArgs
  case args of
    [] -> putStrLn "Supply the schema file."
    (schemaFile : _) -> do
      putStrLn $ unwords ["Parsing schemas from schema file:", schemaFile]
      schemaFileContents <- SB.readFile schemaFile
      case JSON.eitherDecode' (LB.fromStrict schemaFileContents) of
        Left err -> die err
        Right allSchemas -> do
          let schemaMap =
                -- Filter out superseded schemas because of naming conflicts
                M.filter (null . schemaSupersededBy) $
                  -- Filter out pending schemas because they don't have consensus
                  M.filter (not . (SchemaRef "https://pending.schema.org" `elem`) . schemaIsPartOf) $
                    M.fromList (map (\s -> (schemaId s, s)) (allSchemasGraph allSchemas))

          -- Generate the source
          dynFlags <- runGhc (Just GHC.libdir) getDynFlags
          let code =
                showPpr dynFlags $
                  module'
                    (Just "SchemaDotOrg")
                    Nothing
                    [ import' "GHC.Generics" `exposing` [var "Generic"],
                      import' "Data.Aeson",
                      import' "Data.Text" `exposing` [var "Text"]
                    ]
                    $ concatMap (declsFor schemaMap) (M.elems schemaMap)

          let moduleHeader =
                unlines
                  [ "{-# LANGUAGE DeriveGeneric #-}",
                    "{-# LANGUAGE EmptyDataDeriving #-}",
                    "{-# LANGUAGE OverloadedStrings #-}",
                    "{-# LANGUAGE LambdaCase #-}"
                  ]
          let moduleCodec = unlines [moduleHeader, code]

          let moduleFile = "schema-dot-org/src/SchemaDotOrg.hs"
          SB.writeFile moduleFile $ TE.encodeUtf8 $ T.pack moduleCodec
          callProcess "ormolu" ["-i", moduleFile]

declsFor :: Map Text Schema -> Schema -> [HsDecl']
declsFor schemaMap s@Schema {..}
  | "schema:Enumeration" `elem` schemaSubclassOf = declsForEnumeration schemaMap s
  | "rdfs:Class" `elem` schemaType = declsForClass schemaMap s
  | otherwise = []

toCamelCase :: String -> String
toCamelCase = \case
  [] -> []
  [c] -> [toLower c]
  (c1 : c2 : cs)
    | isUpper c1 && isUpper c2 -> toLower c1 : toCamelCase (c2 : cs)
    | isUpper c1 -> toLower c1 : c2 : cs
    | otherwise -> c1 : c2 : cs

toPascalCase :: String -> String
toPascalCase = \case
  [] -> []
  (c : cs) -> toUpper c : cs

directClassProperties :: Map Text Schema -> Schema -> Set Schema
directClassProperties schemaMap schema =
  S.fromList $ M.elems (M.filter ((SchemaRef (schemaId schema) `elem`) . schemaDomainIncludes) schemaMap)

allClassProperties :: Map Text Schema -> Schema -> Set Schema
allClassProperties schemaMap schema =
  let superClassRefs = schemaSubclassOf schema
      superClasses = mapMaybe ((`M.lookup` schemaMap) . unSchemaRef) superClassRefs
   in S.unions $
        directClassProperties schemaMap schema : map (allClassProperties schemaMap) superClasses

declsForClass :: Map Text Schema -> Schema -> [HsDecl']
declsForClass schemaMap schema =
  let classProperties = S.toList $ allClassProperties schemaMap schema
      propertyFieldType s = case schemaRangeIncludes s of
        ["schema:Text"] -> var "Text"
        ["schema:Boolean"] -> var "Bool"
        ["schema:Number"] -> var "Double" -- TODO Double?
        [sr] -> case T.stripPrefix "schema:" (unSchemaRef sr) of
          Nothing -> error $ "Unknown type for schema: " <> show s
          Just schemaId -> var "Text" -- TODO:  var $ fromString (T.unpack schemaId)
        _ -> var "Text" -- TODO we have to make our own extra sum type schema.
      propertyFieldName s = toCamelCase classTypeNameString <> toPascalCase (schemaTypeNameString s) -- TODO prefix naming
      propertyField s =
        (fromString (propertyFieldName s), field (propertyFieldType s))
      classTypeName = fromString classTypeNameString
      classTypeNameString = schemaTypeNameString schema
   in if null classProperties
        then []
        else
          [ data'
              classTypeName
              []
              [recordCon (fromString (schemaTypeNameString schema)) (map propertyField classProperties)]
              [ deriving'
                  [ var "Show",
                    var "Eq",
                    var "Ord",
                    var "Generic"
                  ]
              ]
          ]

declsForEnumeration :: Map Text Schema -> Schema -> [HsDecl']
declsForEnumeration schemaMap schema =
  let enumerationElems = M.elems $ M.filter ((schemaId schema `elem`) . schemaType) schemaMap

      -- Use this as soon as comments work:
      -- {con_doc = schemaDocString s}
      elemConstructor s = prefixCon (schemaTypeName s) []

      enumLiteral s = string ("https://schema.org/" <> schemaTypeNameString s)

      enumTypeName = schemaTypeName schema

      enumOtherTypeNameString = "Other" <> schemaTypeNameString schema
      enumOtherTypeName = fromString enumOtherTypeNameString
   in if null enumerationElems
        then []
        else
          [ data'
              enumTypeName
              []
              (map elemConstructor enumerationElems ++ [prefixCon enumOtherTypeName [field $ var "Text"]])
              [ deriving'
                  [ var "Show",
                    var "Eq",
                    var "Ord",
                    var "Generic"
                  ]
              ],
            instance'
              (var "FromJSON" @@ bvar enumTypeName)
              [ funBinds
                  "parseJSON"
                  [ match [] $
                      (var "withText" @@ string (schemaTypeNameString schema))
                        @@ op
                          (var "pure")
                          "."
                          ( lambdaCase $
                              map
                                ( \s ->
                                    match
                                      [enumLiteral s]
                                      (bvar (schemaTypeName s))
                                )
                                enumerationElems
                                ++ [match [bvar "t"] (bvar enumOtherTypeName @@ bvar "t")]
                          )
                  ]
              ],
            instance'
              (var "ToJSON" @@ bvar enumTypeName)
              [ funBinds
                  "toJSON"
                  [ match [] $
                      op (var "toJSON") "." $
                        lambdaCase $
                          map
                            (\s -> match [bvar (schemaTypeName s)] (enumLiteral s))
                            enumerationElems
                            ++ [match [conP (fromString enumOtherTypeNameString) [bvar "t"]] (bvar "t")]
                  ]
              ]
          ]

schemaTypeName :: Schema -> OccNameStr
schemaTypeName = fromString . schemaTypeNameString

schemaTypeNameString :: Schema -> String
schemaTypeNameString = T.unpack . commentText . schemaLabel

commentText :: Comment -> Text
commentText = \case
  CommentText t -> t
  CommentTextInLang _ t -> t
