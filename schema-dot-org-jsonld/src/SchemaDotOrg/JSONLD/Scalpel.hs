{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module SchemaDotOrg.JSONLD.Scalpel where

import Data.Aeson as JSON
import qualified Data.ByteString.Lazy as LB
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text.Encoding as TE
import Text.HTML.Scalpel

-- | Scrape all JSONLD values on the pages.
scrapeJSONLDValues :: Monad m => ScraperT Text m [JSON.Value]
scrapeJSONLDValues =
  mapMaybe (JSON.decode . LB.fromStrict . TE.encodeUtf8) <$> scrapeJSONLDText

-- | Scrape the raw JSONLD 'Text's on a page.
scrapeJSONLDText :: Monad m => ScraperT Text m [Text]
scrapeJSONLDText = texts ("script" @: ["type" @= "application/ld+json"])
