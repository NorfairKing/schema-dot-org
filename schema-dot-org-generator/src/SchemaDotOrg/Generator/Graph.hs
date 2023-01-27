{-# LANGUAGE OverloadedStrings #-}

module SchemaDotOrg.Generator.Graph (generateGraphFor) where

import Control.Monad
import qualified Data.ByteString as SB
import Data.GraphViz
import Data.GraphViz.Attributes.Complete
import Data.GraphViz.Types.Generalised as Generalised
import Data.GraphViz.Types.Monadic
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Lazy as LT
import SchemaDotOrg.Generator.Schema

generateGraphFor :: SchemaMap -> IO ()
generateGraphFor schemaMap = do
  let originalDoubleEdgesMap :: DoubleEdgesMap
      originalDoubleEdgesMap = makeDoubleEdgesMap $ makeEdgesMap schemaMap
  let relevantEdgeTypes :: Set SchemaEdgeType
      relevantEdgeTypes = S.delete TypeOf $ S.delete SubPropertyOf allSchemaEdgeTypes
  let doubleEdgesMap :: DoubleEdgesMap
      doubleEdgesMap = filterDoubleEdgesMapByRelevantSchemaEdgeTypes originalDoubleEdgesMap relevantEdgeTypes
  let relevantToEvent :: Set Text
      relevantToEvent = nodesRelevantTo doubleEdgesMap "schema:Event"
  let filteredDoubleEdgesMap :: DoubleEdgesMap
      filteredDoubleEdgesMap = filterDoubleEdgesMapByRelevantNodes doubleEdgesMap relevantToEvent
  -- print doubleEdgesMap
  SB.writeFile "schemas.dot" $
    TE.encodeUtf8 $
      LT.toStrict $
        printDotGraph $
          graphFor filteredDoubleEdgesMap

graphFor :: DoubleEdgesMap -> Generalised.DotGraph Text
graphFor schemaMap = digraph (Str "schema.org") $ do
  graphAttrs [RankDir FromLeft]
  forM_ (M.toList schemaMap) $ \(i, (_, s, _)) -> do
    node i (schemaNodeAttributes s)
  forM_ (M.toList schemaMap) $ \(i, (_, _, edgesOut)) -> do
    forM_ edgesOut $ \(et, j) ->
      edge i j [textLabel $ LT.fromStrict $ T.pack $ show et]

schemaNodeAttributes :: Schema -> Attributes
schemaNodeAttributes s = case schemaType s of
  ["rdfs:Class"] -> [color Orange]
  ["rdf:Property"] -> [color Blue]
  _ -> []
