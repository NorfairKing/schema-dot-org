{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module SchemaDotOrg.JSONLD.Tagsoup
  ( Structured (..),
    findStructuredData,
    findRawJSONLDValues,
    findJSONLDTexts,
    findJSONLDTextsInTags,
  )
where

import Data.Aeson as JSON
import qualified Data.ByteString.Lazy as LB
import Data.Maybe
import qualified Data.Text.Encoding.Error as EE
import qualified Data.Text.Lazy as Lazy
import qualified Data.Text.Lazy.Encoding as LTE
import Text.HTML.TagSoup as TagSoup
import Text.HTML.TagSoup.Match as TagSoup

data Structured
  = Microdata JSON.Object
  | JSONLD JSON.Value
  deriving (Show, Eq)

findStructuredData :: LB.ByteString -> [Structured]
findStructuredData = findStructuredDataInTags . TagSoup.parseTagsOptions parseOptionsFast

findStructuredDataInTags :: [Tag LB.ByteString] -> [Structured]
findStructuredDataInTags = go
  where
    go :: [Tag LB.ByteString] -> [Structured]
    go = \case
      [] -> []
      (t : ts)
        | tagOpenLit "script" (anyAttrLit ("type", "application/ld+json")) t -> goLD id ts
        | otherwise -> go ts

    goLD :: ([Tag LB.ByteString] -> [Tag LB.ByteString]) -> [Tag LB.ByteString] -> [Structured]
    goLD acc = \case
      [] -> []
      (t : ts)
        | tagCloseNameLit "script" t -> maybeToList (JSONLD <$> JSON.decode (innerText (acc []))) ++ go ts
        | otherwise -> goLD (acc . (t :)) ts

-- | Scrape all JSONLD values on the pages.
findRawJSONLDValues :: LB.ByteString -> [JSON.Value]
findRawJSONLDValues = mapMaybe (JSON.decode . LTE.encodeUtf8) . findJSONLDTexts

-- | Scrape the raw JSONLD 'Text's on a page.
findJSONLDTexts :: LB.ByteString -> [Lazy.Text]
findJSONLDTexts = findJSONLDTextsInTags . TagSoup.parseTagsOptions parseOptionsFast

findJSONLDTextsInTags :: [Tag LB.ByteString] -> [Lazy.Text]
findJSONLDTextsInTags =
  map
    ( innerText
        -- We have to decode in order for the tagsoup library to deal with
        -- multi-byte characters correctly.
        . map (fmap (LTE.decodeUtf8With EE.lenientDecode))
    )
    . goOpen -- getTagContent "script" ()
  where
    goOpen :: [Tag LB.ByteString] -> [[Tag LB.ByteString]]
    goOpen = \case
      [] -> []
      (t : ts)
        | tagOpenLit "script" (anyAttrLit ("type", "application/ld+json")) t -> goClose id ts
        | otherwise -> goOpen ts
    goClose :: ([Tag LB.ByteString] -> [Tag LB.ByteString]) -> [Tag LB.ByteString] -> [[Tag LB.ByteString]]
    goClose acc = \case
      [] -> []
      (t : ts)
        | tagCloseNameLit "script" t -> acc [] : goOpen ts
        | otherwise -> goClose (acc . (t :)) ts
