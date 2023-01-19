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
                      import' "Data.Aeson"
                    ]
                    $ concatMap (declsFor schemaMap) (M.elems schemaMap)

          let moduleHeader =
                unlines
                  [ "{-# LANGUAGE DeriveGeneric #-}",
                    "{-# LANGUAGE EmptyDataDeriving #-}"
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

      enumTypeName = schemaTypeName schema
   in if null enumerationElems
        then []
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
                  [ match [] $ op (var "withText" @@ string (T.unpack $ commentText (schemaLabel schema))) "$" (string "TODO")
                  ]
              ]
          ]

schemaTypeName :: Schema -> OccNameStr
schemaTypeName = fromString . T.unpack . commentText . schemaLabel

commentText :: Comment -> Text
commentText = \case
  CommentText t -> t
  CommentTextInLang _ t -> t
