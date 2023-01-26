{-# LANGUAGE OverloadedStrings #-}

module SchemaDotOrg.Generator (schemaDotOrgGenerator) where

import Data.Aeson as JSON (eitherDecode')
import qualified Data.ByteString as SB
import qualified Data.ByteString.Lazy as LB
import qualified Data.Map as M
import Path
import SchemaDotOrg.Generator.Code
import SchemaDotOrg.Generator.Graph
import SchemaDotOrg.Generator.OptParse
import SchemaDotOrg.Generator.Schema
import System.Exit

schemaDotOrgGenerator :: IO ()
schemaDotOrgGenerator = do
  Instructions dispatch settings <- getInstructions

  let schemaFile = settingSchemasFile settings

  putStrLn $ unwords ["Parsing schemas from schema file:", fromAbsFile schemaFile]
  schemaFileContents <- SB.readFile (fromAbsFile schemaFile)
  schemaMap <- case JSON.eitherDecode' (LB.fromStrict schemaFileContents) of
    Left err -> die err
    Right allSchemas -> do
      let schemaMap = M.fromList (map (\s -> (schemaId s, s)) (allSchemasGraph allSchemas))

      pure schemaMap

  case dispatch of
    DispatchGenerate -> generateCodeFor schemaMap
    DispatchGraph -> generateGraphFor schemaMap
