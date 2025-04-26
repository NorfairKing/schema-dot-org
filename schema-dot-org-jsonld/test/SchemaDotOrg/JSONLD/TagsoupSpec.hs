{-# LANGUAGE OverloadedStrings #-}

module SchemaDotOrg.JSONLD.TagsoupSpec (spec) where

import Control.Monad
import Data.Aeson as JSON
import qualified Data.ByteString as SB
import qualified Data.ByteString.Lazy as LB
import Data.List (isSuffixOf)
import SchemaDotOrg.JSONLD.Tagsoup
import Test.Syd
import Test.Syd.Aeson

spec :: Spec
spec = do
  describe "findStructuredData" $
    scenarioDir "test_resources" $ \fp ->
      when (".html" `isSuffixOf` fp) $ do
        it (unwords ["can parse the structured data in", show fp]) $ do
          goldenJSONFile (fp <> ".structured") $ do
            contents <- SB.readFile fp
            pure $ findStructuredDataValues (LB.fromStrict contents)
