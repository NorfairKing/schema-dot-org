{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module SchemaDotOrg.JSONLD.TagsoupSpec (spec) where

import Control.Monad
import Data.Aeson as JSON
import qualified Data.ByteString as SB
import qualified Data.ByteString.Lazy as LB
import Path (fileExtension, fromRelFile, reldir, (</>))
import SchemaDotOrg.JSONLD.Tagsoup
import Test.Syd
import Test.Syd.Aeson

spec :: Spec
spec = do
  describe "findStructuredDataValues" $
    it "extracts ld+json nested inside a microdata itemscope (regression)" $ do
      -- Many CMSs wrap <body> in itemscope/WebPage microdata; the nested
      -- ld+json must still be extracted, not swallowed by the microdata item.
      -- Before the fix, only the WebPage microdata was returned (1 value); the
      -- nested Event ld+json must also be extracted (2 values).
      let html :: LB.ByteString
          html =
            mconcat
              [ "<html><body itemscope itemtype=\"https://schema.org/WebPage\">",
                "<script type=\"application/ld+json\">{\"@type\":\"Event\",\"name\":\"Nested\"}</script>",
                "</body></html>"
              ]
      length (findStructuredDataValues html) `shouldBe` 2

  let resourcesDir = [reldir|test_resources|]
  describe "findStructuredData" $
    scenarioDir resourcesDir $ \file ->
      when (fileExtension file == Just ".html") $ do
        let fp = fromRelFile (resourcesDir </> file)
        it (unwords ["can parse the structured data in", show fp]) $ do
          goldenJSONFile (fp <> ".structured") $ do
            contents <- SB.readFile fp
            pure $ toJSON $ findStructuredDataValues (LB.fromStrict contents)
