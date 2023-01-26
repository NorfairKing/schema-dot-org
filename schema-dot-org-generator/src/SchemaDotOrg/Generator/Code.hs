{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module SchemaDotOrg.Generator.Code (generateCodeFor) where

import qualified Data.ByteString as SB
import Data.Char
import Data.List
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
import System.Process.Typed

generateCodeFor :: Map Text Schema -> IO ()
generateCodeFor schemaMap = do
  dynFlags <- runGhc (Just GHC.libdir) getDynFlags
  let code =
        showPpr dynFlags $
          module'
            (Just "SchemaDotOrg.Generated")
            Nothing
            [ import' "GHC.Generics" `exposing` [var "Generic"],
              import' "Data.Text" `exposing` [var "Text"],
              import' "Data.Aeson" `exposing` [thingAll "FromJSON", thingAll "ToJSON", bvar "withText", bvar "Value"],
              import' "SchemaDotOrg.Schema"
            ]
            $ concatMap (declsFor schemaMap) (M.elems schemaMap)

  let moduleHeader =
        unlines
          [ "{-# LANGUAGE DeriveGeneric #-}",
            "{-# LANGUAGE EmptyDataDeriving #-}",
            "{-# LANGUAGE OverloadedStrings #-}",
            "{-# LANGUAGE DataKinds #-}",
            "{-# LANGUAGE LambdaCase #-}"
          ]
  let moduleCodec = unlines [moduleHeader, code]

  let moduleFile = "schema-dot-org/src/SchemaDotOrg/Generated.hs"
  SB.writeFile moduleFile $ TE.encodeUtf8 $ T.pack moduleCodec
  runProcess_ $ proc "ormolu" ["-i", moduleFile]

declsFor :: Map Text Schema -> Schema -> [HsDecl']
declsFor schemaMap s@Schema {..}
  | subclassOfEnumeration schemaMap s = declsForEnumeration schemaMap s
  | "rdfs:Class" `elem` schemaType = declsForClass schemaMap s
  | "rdf:Property" `elem` schemaType = declsForProperty schemaMap s
  | otherwise = []

subclassOfEnumeration :: Map Text Schema -> Schema -> Bool
subclassOfEnumeration schemaMap s = "schema:Enumeration" `elem` transitiveSuperclasses schemaMap s

transitiveSuperclasses :: Map Text Schema -> Schema -> [Text]
transitiveSuperclasses schemaMap schema = go (map unSchemaRef (schemaSubclassOf schema))
  where
    go :: [Text] -> [Text]
    go superClasses =
      superClasses
        ++ let superSuperClasses t = case M.lookup t schemaMap of
                 Nothing -> []
                 Just schema -> go (map unSchemaRef (schemaSubclassOf schema))
            in concatMap superSuperClasses superClasses

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
  let classTypeNameString = schemaTypeNameString schema
      classTypeName = fromString classTypeNameString
      valueTypeName = fromString $ "class" <> classTypeNameString
      superClasses = transitiveSuperclasses schemaMap schema
      superClassTypeNames = mapMaybe (fmap (toTypeName . T.unpack) . T.stripPrefix "schema:") superClasses
      primitiveTypes = ["Text"]
   in case find (\primitiveType -> schemaSubclassOf schema == [SchemaRef ("schema:" <> primitiveType)]) primitiveTypes of
        Just primitiveType -> [type' classTypeName [] (var (fromString (T.unpack primitiveType)))]
        Nothing ->
          if "schema:DataType" `elem` schemaType schema
            || commentText (schemaLabel schema) `elem` ["Class", "Property", "Integer", "Float"]
            then []
            else
              [ data' classTypeName [] [] [],
                typeSig valueTypeName (bvar "Class" @@ bvar classTypeName @@ listPromotedTy (map (bvar . fromString) superClassTypeNames)),
                funBind valueTypeName (match [] (var "Class" @@ string (T.unpack (commentText (schemaLabel schema)))))
              ]

declsForProperty :: Map Text Schema -> Schema -> [HsDecl']
declsForProperty schemaMap schema =
  let classTypeNameString = schemaTypeNameString schema
      classTypeName = fromString classTypeNameString
      propertyLabel = T.unpack (commentText (schemaLabel schema))
      valueTypeName className = fromString $ "property" <> toPascalCase className <> toPascalCase propertyLabel
   in if SchemaRef "https://meta.schema.org" `elem` schemaIsPartOf schema
        then []
        else
          concatMap
            ( \(SchemaRef ref) -> case T.unpack <$> T.stripPrefix "schema:" ref of
                Nothing -> []
                Just className ->
                  let typeName = valueTypeName className
                      rangeTypeNames = mapMaybe (fmap (toTypeName . T.unpack) . T.stripPrefix "schema:" . unSchemaRef) (schemaRangeIncludes schema)
                   in [ typeSig typeName (bvar "Property" @@ bvar (fromString (toTypeName className)) @@ listPromotedTy (map (bvar . fromString) rangeTypeNames)),
                        funBind typeName (match [] (bvar "Property" @@ string propertyLabel))
                      ]
            )
            (schemaDomainIncludes schema)

declsForEnumeration :: Map Text Schema -> Schema -> [HsDecl']
declsForEnumeration schemaMap schema =
  let enumerationElems = M.elems $ M.filter ((schemaId schema `elem`) . schemaType) schemaMap

      elemConstructorName s = schemaTypeNameString schema <> schemaTypeNameString s
      elemConstructor s = prefixCon (fromString (elemConstructorName s)) []

      enumLiteral s = string ("https://schema.org/" <> schemaTypeNameString s)

      enumTypeName = schemaTypeName schema
   in if null enumerationElems
        then [type' enumTypeName [] (var "Text")]
        else
          [ data'
              enumTypeName
              []
              (map elemConstructor enumerationElems)
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
                        @@ lambdaCase
                          ( map
                              ( \s ->
                                  match
                                    [enumLiteral s]
                                    (var "pure" @@ bvar (fromString (elemConstructorName s)))
                              )
                              enumerationElems
                              ++ [ match
                                     [bvar "t"]
                                     ( var "fail"
                                         @@ op
                                           ( string
                                               ( "Failed to parse "
                                                   ++ schemaTypeNameString schema
                                                   ++ ": "
                                               )
                                           )
                                           "<>"
                                           (var "show" @@ bvar "t")
                                     )
                                 ]
                          )
                  ]
              ],
            instance'
              (var "ToJSON" @@ bvar enumTypeName)
              [ funBinds
                  "toJSON"
                  [ match [] $
                      op (var "toJSON" @::@ (var "Text" GHC.SourceGen.--> var "Value")) "." $
                        lambdaCase $
                          map
                            (\s -> match [bvar (fromString (elemConstructorName s))] (enumLiteral s))
                            enumerationElems
                  ]
              ]
          ]

schemaTypeName :: Schema -> OccNameStr
schemaTypeName = fromString . schemaTypeNameString

schemaTypeNameString :: Schema -> String
schemaTypeNameString = toTypeName . T.unpack . commentText . schemaLabel

commentText :: Comment -> Text
commentText = \case
  CommentText t -> t
  CommentTextInLang _ t -> t

toTypeName :: String -> String
toTypeName = dropWhile isDigit
