{-# LANGUAGE OverloadedStrings #-}

module SchemaDotOrg.Generator.Graph (generateGraphFor) where

import Control.Monad
import Data.Aeson as JSON (eitherDecode')
import qualified Data.ByteString as SB
import qualified Data.ByteString.Lazy as LB
import Data.Char
import Data.GraphViz
import Data.GraphViz.Attributes.Complete
import Data.GraphViz.Printing (renderDot)
import Data.GraphViz.Types.Generalised as Generalised
import Data.GraphViz.Types.Monadic
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as S
import Data.String
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Lazy as LT
import GHC (runGhc)
import GHC.Driver.Session (getDynFlags)
import qualified GHC.Paths as GHC (libdir)
import GHC.SourceGen
import Path
import SchemaDotOrg.Generator.OptParse
import SchemaDotOrg.Generator.Schema
import System.Environment (getArgs)
import System.Exit
import System.Process.Typed

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
  forM_ (M.toList schemaMap) $ \(i, (_, s, edgesOut)) -> do
    forM_ edgesOut $ \(et, j) ->
      edge i j [textLabel $ LT.fromStrict $ T.pack $ show et]

schemaNodeAttributes :: Schema -> Attributes
schemaNodeAttributes s = case schemaType s of
  ["rdfs:Class"] -> [color Orange]
  ["rdf:Property"] -> [color Blue]
  _ -> []
