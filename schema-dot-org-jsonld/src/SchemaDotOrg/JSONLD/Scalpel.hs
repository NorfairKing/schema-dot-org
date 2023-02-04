{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module SchemaDotOrg.JSONLD.Scalpel where

import Data.Aeson as JSON
import qualified Data.ByteString.Lazy as LB
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text.Encoding as TE
import SchemaDotOrg.JSONLD.Parse
import Text.HTML.Scalpel
import qualified Text.HTML.TagSoup as TagSoup

scrapeJSONLDValues :: Monad m => ParserOf classes a -> ScraperT Text m [[Either [String] a]]
scrapeJSONLDValues parser = map (parseValues parser) <$> scrapeRawJSONLDValues

scrapeJSONLDValue :: Monad m => ParserOf classes a -> ScraperT Text m [Either [String] a]
scrapeJSONLDValue parser = map (parseValue parser) <$> scrapeRawJSONLDValues

-- | Scrape all JSONLD values on the pages.
scrapeRawJSONLDValues :: Monad m => ScraperT Text m [JSON.Value]
scrapeRawJSONLDValues =
  mapMaybe (JSON.decode . LB.fromStrict . TE.encodeUtf8) <$> scrapeJSONLDText

-- | Scrape the raw JSONLD 'Text's on a page.
scrapeJSONLDText :: Monad m => ScraperT Text m [Text]
scrapeJSONLDText = map unHTMLText <$> texts ("script" @: ["type" @= "application/ld+json"])

-- | Decode HTML entities
unHTMLText :: Text -> Text
unHTMLText = TagSoup.innerText . TagSoup.parseTags
