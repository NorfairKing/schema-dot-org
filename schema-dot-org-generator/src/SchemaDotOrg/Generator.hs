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
import GHC (LHsDocString, con_doc, mkHsDocString, noLoc, runGhc)
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

          mapM_ print $ M.keys $ M.filter (\s -> "schema:Event" `elem` schemaDomainIncludes s) schemaMap

          putStrLn "Enumerations"
          mapM_ print $ M.filter (\s -> "schema:Enumeration" `elem` schemaSubclassOf s) schemaMap

          -- Generate the source
          dynFlags <- runGhc (Just GHC.libdir) getDynFlags
          let code =
                showPpr dynFlags $
                  module'
                    (Just "SchemaDotOrg")
                    Nothing
                    [ import' "GHC.Generics" `exposing` [var "Generic"]
                    ]
                    $ concatMap (declsFor schemaMap) (M.elems schemaMap)
          putStrLn "code"

          let moduleHeader =
                unlines
                  [ "{-# LANGUAGE DeriveGeneric #-}",
                    "{-# LANGUAGE EmptyDataDeriving #-}"
                  ]
          let moduleCodec = unlines [moduleHeader, code]

          let moduleFile = "schema-dot-org/src/SchemaDotOrg.hs"
          writeFile moduleFile moduleCodec
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
   in [ data'
          (schemaTypeName schema)
          []
          (map elemConstructor enumerationElems)
          [deriving' [var "Show", var "Eq", var "Generic"]]
      ]

schemaTypeName :: Schema -> OccNameStr
schemaTypeName = fromString . T.unpack . commentText . schemaLabel

schemaDocString :: Schema -> Maybe LHsDocString
schemaDocString = Just . commentDocString . schemaComment

commentDocString :: Comment -> LHsDocString
commentDocString = noLoc . mkHsDocString . T.unpack . commentText

commentText :: Comment -> Text
commentText = \case
  CommentText t -> t
  CommentTextInLang _ t -> t
