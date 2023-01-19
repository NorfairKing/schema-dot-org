{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module SchemaDotOrg.Generator.Schema where

import Autodocodec
import Data.Aeson as JSON (FromJSON, ToJSON, Value)
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
    schemaLabel :: Comment
    -- schemaDomainIncludes :: [Text]
    -- schemaRangeIncludes :: [Text]
  }
  deriving stock (Show, Eq, Generic)

instance HasCodec Schema where
  codec =
    object "Schema" $
      Schema
        <$> requiredField' "@id" .= schemaId
        <*> requiredFieldWith' "@type" (singleOrListCodec codec) .= schemaType
        <*> requiredField' "rdfs:comment" .= schemaComment
        <*> requiredField' "rdfs:label" .= schemaLabel

data Comment
  = CommentText Text
  | CommentTextInLang
      Text
      -- ^ Lang
      Text
      -- ^ Value
  deriving stock (Show, Eq, Generic)

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
