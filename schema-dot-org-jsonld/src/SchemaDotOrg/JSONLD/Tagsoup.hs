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

import Control.Applicative
import Data.Aeson as JSON
import Data.Aeson.Key (Key)
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KM
import qualified Data.Aeson.Types as JSON
import qualified Data.ByteString.Lazy as LB
import Data.List.NonEmpty (NonEmpty (..), (<|))
import qualified Data.List.NonEmpty as NE
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Encoding.Error as EE
import qualified Data.Text.Lazy as Lazy
import qualified Data.Text.Lazy.Encoding as LTE
import Debug.Trace
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
    dec = TE.decodeUtf8With EE.lenientDecode . LB.toStrict

    go :: [Tag LB.ByteString] -> [Structured]
    go = \case
      [] -> []
      (t : ts) -> case t of
        TagOpen "script" attrs | anyAttrLit ("type", "application/ld+json") attrs -> goLD id ts
        TagOpen tagName attrs
          | anyAttrNameLit "itemscope" attrs -> goMicrodata (Ctx True tagName Nothing (typeAndIdProps attrs) :| []) ts
        _ -> go ts

    goLD :: ([Tag LB.ByteString] -> [Tag LB.ByteString]) -> [Tag LB.ByteString] -> [Structured]
    goLD acc = \case
      [] -> []
      (t : ts)
        | tagCloseNameLit "script" t -> maybeToList (JSONLD <$> JSON.decode (innerText (acc []))) ++ go ts
        | otherwise -> goLD (acc . (t :)) ts

    goMicrodata :: NonEmpty Ctx -> [Tag LB.ByteString] -> [Structured]
    goMicrodata stack@(Ctx isItem open mProp obj :| rest) = \case
      [] -> []
      (t : ts) -> traceShow stack $ traceShow ("go", t) $ case t of
        TagOpen tagName attrs ->
          traceShow ("open", tagName) $
            -- Open tag, definitely push a new context onto the stack.
            let newMProp = Key.fromText . dec <$> lookup "itemprop" attrs
                newIsItem = any ((== "itemscope") . fst) attrs
             in case newMProp of
                  Nothing ->
                    -- Not a property, just new open that needs to be closed later.
                    goMicrodata (Ctx newIsItem tagName Nothing KM.empty <| stack) ts
                  Just propKey ->
                    -- Definitely a property
                    case lookup "itemscope" attrs of
                      Just _ ->
                        -- Its own type
                        let newCtx = Ctx True tagName (Just propKey) (typeAndIdProps attrs)
                            newStack = newCtx <| stack
                         in goMicrodata newStack ts
                      Nothing -> case attrsFromSingleTag propKey tagName attrs of
                        Just (k, v) ->
                          -- Data in attrs
                          let newCtx = Ctx True tagName (Just propKey) (KM.singleton k v)
                              newStack = newCtx <| stack
                           in goMicrodata newStack ts
                        Nothing ->
                          -- Textual data
                          -- We don't build a whole new context and continue.
                          -- We just find the text and put it in the current context immediately.
                          let (text, restTags) = goTextual id [] ts
                              newObj' = KM.insert propKey (toJSON text) (typeAndIdProps attrs)
                              newCtx' = Ctx isItem open mProp newObj'
                              newStack' = newCtx' :| rest
                           in goMicrodata newStack' restTags
        TagClose tagName ->
          traceShow ("close", tagName) $
            -- We don't check if the tag name matches here, yet because:
            -- It can happen that we close a tag that wasn't open
            -- This can happen in situations like this:
            --
            -- <span>
            --  Hello
            --  <br>
            --  World
            -- </span>
            --
            -- The ctx stack would then look like this:
            --
            -- span > br >
            -- and we find a closing "span"
            --
            -- so we want to collapse the contexts upward until we find the "span" context.
            let (ctx'@(Ctx isItem open' _ obj') :| rest') =
                  (\col -> traceShow ("Collapsed:", open, col) col) $
                    collapseUpward tagName stack
             in if open' == tagName
                  then -- Current context is closed.
                  case NE.nonEmpty rest' of
                    Nothing ->
                      -- The object is complete.
                      Microdata obj' : go ts
                    Just (outerCtx@(Ctx outerIsItem outerTag mOuterProp outerObj) :| outerRest) ->
                      if isItem
                        then -- The object is complete, but it was part of the context before, add it there and continue.

                          let newOuterObj = case mOuterProp of
                                Nothing -> outerObj
                                Just prop -> KM.insert prop (toJSON obj') outerObj
                              newOuter = Ctx outerIsItem outerTag mOuterProp newOuterObj
                              newStack = newOuter :| outerRest
                           in goMicrodata newStack ts
                        else -- The outer was not an item, collapse the state with it

                          let newOuter = collapseCtx ctx' outerCtx
                              newStack = newOuter :| outerRest
                           in goMicrodata newStack ts
                  else -- If the tag name still doesn't match after collapsing, ignore the closing tag.
                    goMicrodata stack ts
        -- Ignore any other tags
        _ -> goMicrodata stack ts

    collapseUpward expectedOpen stack@(ctx@(Ctx _ open _ _) :| rest)
      | expectedOpen == open = stack
      | otherwise =
          case NE.nonEmpty rest of
            Nothing ->
              -- Nothing to collapse
              stack
            Just restNE ->
              let (ctx' :| rest') = collapseUpward expectedOpen restNE
                  collapsed = collapseCtx ctx ctx'
               in collapsed :| rest'

    collapseCtx (Ctx isItem open mProp obj) (Ctx isItem' open' mProp' obj') =
      Ctx (isItem' || isItem) open' (mProp' <|> mProp) (KM.union obj' obj)

    goTextual :: ([Tag LB.ByteString] -> [Tag LB.ByteString]) -> [LB.ByteString] -> [Tag LB.ByteString] -> (Text, [Tag LB.ByteString])
    goTextual acc tags = \case
      [] -> (dec (innerText (acc [])), [])
      tss@(t : ts) -> case t of
        TagText {} -> goTextual (acc . (t :)) tags ts
        TagOpen name _ -> goTextual acc (name : tags) ts
        TagClose name -> case dropWhile (/= name) tags of
          [] -> (dec (innerText (acc [])), ts)
          (name' : rest)
            | name' == name -> goTextual acc rest ts
            | otherwise -> goTextual acc tags ts
        _ -> goTextual acc tags ts

    attrsFromSingleTag :: JSON.Key -> LB.ByteString -> [(LB.ByteString, LB.ByteString)] -> Maybe JSON.Pair
    attrsFromSingleTag key tg attrs =
      -- https://html.spec.whatwg.org/multipage/microdata.html#values
      let fromAttr k =
            -- Use fromMaybe instead of a missing property because the tag was
            -- still there even if the value wasn't.
            Just $ key .= dec (fromMaybe "" (lookup k attrs))
       in case tg of
            "meta" -> fromAttr "content"
            "audio" -> fromAttr "src"
            "embed" -> fromAttr "src"
            "iframe" -> fromAttr "src"
            "img" -> fromAttr "src"
            "source" -> fromAttr "src"
            "track" -> fromAttr "src"
            "a" -> fromAttr "href"
            "area" -> fromAttr "href"
            "link" -> fromAttr "href"
            "object" -> fromAttr "data"
            "data" -> fromAttr "value"
            "meter" -> fromAttr "value"
            "time" -> fromAttr "datetime"
            _ -> Nothing

    typeAndIdProps attrs =
      KM.fromList $
        concat
          [ ["@type" .= dec typ | typ <- maybeToList (lookup "itemtype" attrs)],
            ["@id" .= dec i | i <- maybeToList (lookup "itemid" attrs)]
          ]

-- Maybe make the context a json object already?
data Ctx = Ctx
  { -- If the context represents an 'itemscope'
    ctxScope :: !Bool,
    -- Name of the open tag
    ctxOpen :: !LB.ByteString,
    -- Name of the itemprop, if there was one
    ctxProp :: !(Maybe JSON.Key),
    -- Object that's being built
    ctxObj :: !JSON.Object
  }
  deriving (Show)

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
