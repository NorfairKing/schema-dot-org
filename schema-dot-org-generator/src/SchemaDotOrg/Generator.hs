{-# LANGUAGE OverloadedStrings #-}

module SchemaDotOrg.Generator (schemaDotOrgGenerator) where

import GHC (noLoc, runGhc)
import GHC.Driver.Session (getDynFlags)
import GHC.Hs (hsmodHaddockModHeader, mkHsDocString)
import qualified GHC.Paths as GHC (libdir)
import GHC.SourceGen

schemaDotOrgGenerator :: IO ()
schemaDotOrgGenerator = do
  dynFlags <- runGhc (Just GHC.libdir) getDynFlags
  putStrLn $ showPpr dynFlags $ module' (Just "SchemaDotOrg") Nothing [] []
