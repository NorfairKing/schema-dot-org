{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module SchemaDotOrg.Generator (schemaDotOrgGenerator) where

import Data.Aeson as JSON (eitherDecode')
import qualified Data.ByteString as SB
import qualified Data.ByteString.Lazy as LB
import Data.Map (Map)
import qualified Data.Map as M
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
          let schemaMap = M.fromList (map (\s -> (schemaId s, s)) (allSchemasGraph allSchemas))

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
declsFor schemaMap s@Schema {..} =
  if "schema:Enumeration" `elem` schemaSubclassOf
    then declsForEnumeration schemaMap s
    else []

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
