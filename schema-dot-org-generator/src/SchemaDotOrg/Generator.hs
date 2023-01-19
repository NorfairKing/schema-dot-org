{-# LANGUAGE OverloadedStrings #-}

module SchemaDotOrg.Generator (schemaDotOrgGenerator) where

import Data.Aeson as JSON (eitherDecode')
import qualified Data.ByteString as SB
import qualified Data.ByteString.Lazy as LB
import GHC (runGhc)
import GHC.Driver.Session (getDynFlags)
import qualified GHC.Paths as GHC (libdir)
import GHC.SourceGen
import SchemaDotOrg.Generator.Schema
import System.Environment (getArgs)
import System.Exit

schemaDotOrgGenerator :: IO ()
schemaDotOrgGenerator = do
  dynFlags <- runGhc (Just GHC.libdir) getDynFlags
  putStrLn $ showPpr dynFlags $ module' (Just "SchemaDotOrg") Nothing [] []

  args <- getArgs
  case args of
    [] -> putStrLn "Supply the schema file."
    (schemaFile : _) -> do
      print schemaFile
      schemaFileContents <- SB.readFile schemaFile
      case JSON.eitherDecode' (LB.fromStrict schemaFileContents) of
        Left err -> die err
        Right allSchemas -> do
          mapM_ print (allSchemasGraph allSchemas)
