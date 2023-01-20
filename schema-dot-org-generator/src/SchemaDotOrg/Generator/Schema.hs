{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module SchemaDotOrg.Generator.Schema where

import Autodocodec
import Data.Aeson as JSON (FromJSON, ToJSON, Value)
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
