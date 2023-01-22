{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module SchemaDotOrg.Generator.Schema where

import Autodocodec
import Data.Aeson as JSON (FromJSON, ToJSON, Value)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as S
import Data.String
import Data.Text (Text)
import GHC.Generics (Generic)

data AllSchemas = AllSchemas
  { allSchemasContext :: JSON.Value,
    allSchemasGraph :: [Schema]
  }
  deriving stock (Show, Eq, Generic)
  deriving (FromJSON, ToJSON) via (Autodocodec AllSchemas)

instance HasCodec AllSchemas where
  codec =
    object "AllSchemas" $
      AllSchemas
        <$> requiredField' "@context" .= allSchemasContext
        <*> requiredField' "@graph" .= allSchemasGraph

filterDoubleEdgesMapByRelevantSchemaEdgeTypes :: DoubleEdgesMap -> Set SchemaEdgeType -> DoubleEdgesMap
filterDoubleEdgesMapByRelevantSchemaEdgeTypes dem relevantSchemaEdgeTypes =
  M.map
    ( \(fromEdges, schema, toEdges) ->
        ( S.filter ((`S.member` relevantSchemaEdgeTypes) . snd) fromEdges,
          schema,
          S.filter ((`S.member` relevantSchemaEdgeTypes) . fst) toEdges
        )
    )
    dem

filterDoubleEdgesMapByRelevantNodes :: DoubleEdgesMap -> Set Text -> DoubleEdgesMap
filterDoubleEdgesMapByRelevantNodes dem relevantIds =
  M.map fromJust
    . M.filter isJust
    $ M.mapWithKey
      ( \sid (fromEdges, schema, toEdges) ->
          if sid `S.member` relevantIds
            then
              Just
                ( S.filter ((`S.member` relevantIds) . fst) fromEdges,
                  schema,
                  S.filter ((`S.member` relevantIds) . snd) toEdges
                )
            else Nothing
      )
      dem

nodesRelevantTo :: DoubleEdgesMap -> Text -> Set Text
nodesRelevantTo dem sid = case M.lookup sid dem of
  Nothing -> S.empty
  Just (edgesTo, _, edgesFrom) ->
    S.insert sid $
      S.union
        (eventuallyReachableFrom dem sid)
        (eventuallyReachingTo dem sid)

eventuallyReachableFrom :: DoubleEdgesMap -> Text -> Set Text
eventuallyReachableFrom dem sid =
  let reachableInOneStep = directlyReachableFrom dem sid
   in S.union reachableInOneStep (S.unions (map (eventuallyReachableFrom dem) (S.toList reachableInOneStep)))

eventuallyReachingTo :: DoubleEdgesMap -> Text -> Set Text
eventuallyReachingTo dem sid =
  let reachingInOneStep = directlyReachingTo dem sid
   in S.union reachingInOneStep (S.unions (map (eventuallyReachingTo dem) (S.toList reachingInOneStep)))

directlyReachableFrom :: DoubleEdgesMap -> Text -> Set Text
directlyReachableFrom dem sid = case M.lookup sid dem of
  Nothing -> S.empty
  Just (_, _, es) -> S.map snd es

directlyReachingTo :: DoubleEdgesMap -> Text -> Set Text
directlyReachingTo dem sid = case M.lookup sid dem of
  Nothing -> S.empty
  Just (es, _, _) -> S.map fst es

-- Schemas, which schemas point to them, and which schemas they point to.
type DoubleEdgesMap = Map Text (Set (Text, SchemaEdgeType), Schema, Set (SchemaEdgeType, Text))

makeDoubleEdgesMap :: EdgesMap -> DoubleEdgesMap
makeDoubleEdgesMap em =
  M.mapWithKey
    ( \i (s, outgoingEdges) ->
        let go :: Text -> (Schema, Set (SchemaEdgeType, Text)) -> Set (Text, SchemaEdgeType)
            go j (_, es) = S.fromList $ map ((,) j . fst) $ filter ((== i) . snd) $ S.toList es
            arrivingEdges = foldMap (uncurry go) $ M.toList em
         in (arrivingEdges, s, outgoingEdges)
    )
    em

-- Schemas and which other schemas they point to.
type EdgesMap = Map Text (Schema, Set (SchemaEdgeType, Text))

makeEdgesMap :: SchemaMap -> EdgesMap
makeEdgesMap = M.map (\s -> (s, edgesFrom s))

data SchemaEdgeType
  = TypeOf
  | SubclassOf
  | SubPropertyOf
  | DomainIncludes
  | RangeIncludes
  | IsPartOf
  | IsSupersededBy
  deriving (Show, Eq, Ord, Bounded, Enum)

allSchemaEdgeTypes :: Set SchemaEdgeType
allSchemaEdgeTypes = S.fromList [minBound .. maxBound]

edgesFrom :: Schema -> Set (SchemaEdgeType, Text)
edgesFrom s =
  S.fromList $
    concat
      [ map ((,) TypeOf) (schemaType s),
        map ((,) SubclassOf . unSchemaRef) (schemaSubclassOf s),
        map ((,) SubPropertyOf . unSchemaRef) (schemaSubPropertyOf s),
        map ((,) DomainIncludes . unSchemaRef) (schemaDomainIncludes s),
        map ((,) RangeIncludes . unSchemaRef) (schemaRangeIncludes s),
        map ((,) IsPartOf . unSchemaRef) (schemaIsPartOf s),
        map ((,) IsSupersededBy . unSchemaRef) (schemaSupersededBy s)
      ]

-- Map of schemas keyed by their id
type SchemaMap = Map Text Schema

data Schema = Schema
  { schemaId :: Text,
    schemaType :: [Text],
    schemaComment :: Comment,
    schemaLabel :: Comment,
    schemaSubclassOf :: [SchemaRef],
    schemaSubPropertyOf :: [SchemaRef],
    schemaDomainIncludes :: [SchemaRef],
    schemaRangeIncludes :: [SchemaRef],
    schemaIsPartOf :: [SchemaRef],
    schemaSupersededBy :: [SchemaRef]
  }
  deriving stock (Show, Eq, Ord, Generic)

instance HasCodec Schema where
  codec =
    object "Schema" $
      Schema
        <$> requiredField' "@id" .= schemaId
        <*> requiredFieldWith' "@type" (singleOrListCodec codec) .= schemaType
        <*> requiredField' "rdfs:comment" .= schemaComment
        <*> requiredField' "rdfs:label" .= schemaLabel
        <*> optionalFieldWithOmittedDefaultWith' "rdfs:subClassOf" (singleOrListCodec codec) [] .= schemaSubclassOf
        <*> optionalFieldWithOmittedDefaultWith' "rdfs:subPropertyOf" (singleOrListCodec codec) [] .= schemaSubPropertyOf
        <*> optionalFieldWithOmittedDefaultWith' "schema:domainIncludes" (singleOrListCodec codec) [] .= schemaDomainIncludes
        <*> optionalFieldWithOmittedDefaultWith' "schema:rangeIncludes" (singleOrListCodec codec) [] .= schemaRangeIncludes
        <*> optionalFieldWithOmittedDefaultWith' "schema:isPartOf" (singleOrListCodec codec) [] .= schemaIsPartOf
        <*> optionalFieldWithOmittedDefaultWith' "schema:supersededBy" (singleOrListCodec codec) [] .= schemaSupersededBy

data Comment
  = CommentText Text
  | CommentTextInLang
      Text
      -- ^ Lang
      Text
      -- ^ Value
  deriving stock (Show, Eq, Ord, Generic)

instance HasCodec Comment where
  codec =
    dimapCodec f g $
      eitherCodec codec $
        object "CommentTextInLang" $
          (,)
            <$> requiredField' "@language" .= fst
            <*> requiredField' "@value" .= snd
    where
      f = \case
        Left t -> CommentText t
        Right (l, v) -> CommentTextInLang l v
      g = \case
        CommentText t -> Left t
        CommentTextInLang l v -> Right (l, v)

newtype SchemaRef = SchemaRef {unSchemaRef :: Text}
  deriving stock (Show, Eq, Ord, Generic)

instance HasCodec SchemaRef where
  codec = object "SchemaRef" $ SchemaRef <$> requiredField' "@id" .= unSchemaRef

instance IsString SchemaRef where
  fromString = SchemaRef . fromString
