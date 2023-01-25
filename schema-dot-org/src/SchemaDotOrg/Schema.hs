{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module SchemaDotOrg.Schema
  ( -- * Schema declaration
    Class (..),
    Property (..),

    -- ** Data types
    Boolean,
    Number,
    Date,
    Time,
    DateTime,

    -- ** Parsing
    ParserOf (..),
    Options (..),
    ParseableOptions (..),
    Inherits,
    parseClass,
    lookupProperty,
    requireProperty,
    lookupPropertyClass,
    requirePropertyClass,

    -- ** Rendering
    RenderOf (..),
    renderClass,
    IsExpectedType,
    renderProperty,
    renderSimpleProperty,
    renderPropertyClass,
  )
where

import Control.Applicative
import Data.Aeson hiding (Options)
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.Aeson.Types as JSON
import Data.Foldable
import Data.Kind
import Data.Scientific (Scientific)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time as Time

type Boolean = Bool

type Date = Time.Day -- TODO

type Time = Time.TimeOfDay -- TODO

type DateTime = Time.LocalTime -- TODO

type Number = Scientific

-- | A class schema.
--
-- Existence of a value of this type (in this library) implies that there is a
-- class schema on schema.org called `clazz` with superclasses `superClasses`.
data Class clazz superClasses = Class {className :: Text}

-- | A class schema.
--
-- Existence of a value of this type (in this library) implies that there is a
-- property schema on schema.org for class `clazz` with expected types `expectedTypes`.
data Property clazz expectedTypes = Property {propertyName :: Text}

-- | A parser for a given class. The type-level list `classes` contains the
-- class itself, and its transitive superclasses.
newtype ParserOf (classes :: [Type]) (a :: Type) = ParserOf {unParser :: JSON.Object -> Either String a}

instance Functor (ParserOf classes) where
  fmap f (ParserOf func) = ParserOf $ \o ->
    f <$> func o

instance Applicative (ParserOf classes) where
  pure a = ParserOf $ \_ -> pure a
  (ParserOf ff) <*> (ParserOf fa) = ParserOf $ \o -> do
    f <- ff o
    a <- fa o
    pure (f a)

instance Alternative (ParserOf classes) where
  empty = ParserOf $ \_ -> Left "empty"
  (ParserOf fa) <|> (ParserOf fb) =
    ParserOf $ \o -> fa o <|> fb o

instance Monad (ParserOf classes) where
  (ParserOf fa) >>= func = ParserOf $ \o -> do
    a <- fa o
    let (ParserOf fb) = func a
    fb o

instance MonadFail (ParserOf classes) where
  fail err = ParserOf $ \_ -> Left err

-- | All possible interpretations of a given 'Value' as a value of any of the
-- `expectedTypes` types.
--
-- We keep the options in the order that they appear in the property.
-- Even if one of the options succeeds to parse, we still allow evaluating the
-- other parse-interpretations.
-- I.e. something can be both a valid Text and a valid 'DateTime'.
-- The consumer decides which to use and how.
data Options expectedTypes where
  -- | The consumer can always access the raw 'Value'.
  UnexpectedOption :: Value -> Options '[]
  -- | The parse result of the first `expectedType` in the list
  ExpectedOption :: Either String a -> Options otherExpectedType -> Options (a ': otherExpectedType)

-- | Whether a list of 'expectedTypes' is parseable.
--
-- This just means they all have 'FromJSON' instances.
class ParseableOptions expectedTypes where
  parseOptions :: JSON.Value -> Options expectedTypes

instance ParseableOptions '[] where
  parseOptions = UnexpectedOption

instance
  (FromJSON firstExpectedType, ParseableOptions otherExpectedTypes) =>
  ParseableOptions (firstExpectedType ': otherExpectedTypes)
  where
  parseOptions v = ExpectedOption (JSON.parseEither parseJSON v) (parseOptions v)

-- | Whether a class is in the given inheritance hierarchy
class Inherits classes clazz

instance {-# OVERLAPS #-} Inherits (clazz ': otherClasses) clazz

instance Inherits otherClasses clazz => Inherits (otherClass ': otherClasses) clazz

-- | Parse a value of a given class, from a 'JSON.Value'.
parseClass ::
  Class clazz superClasses ->
  -- | How to parse the value
  ParserOf (clazz : superClasses) a ->
  JSON.Value ->
  Either String a
parseClass clazz (ParserOf parseFunc) value =
  JSON.parseEither (withObject (T.unpack (className clazz)) (either fail pure . parseFunc)) value

-- | Lookup a property in a 'Class'.
lookupProperty ::
  (Inherits classes propertyClass, ParseableOptions expectedTypes) =>
  Property propertyClass expectedTypes ->
  ParserOf classes (Maybe (Options expectedTypes))
lookupProperty property = ParserOf $ \o ->
  case KeyMap.lookup (Key.fromText (propertyName property)) o of
    Nothing -> pure Nothing
    Just v -> pure $ Just $ parseOptions v

-- | Lookup a property in a 'Class', fail if it wasn't there.
--
-- All properties are optional, so you may want to use 'lookupProperty' instead.
requireProperty ::
  (Inherits classes propertyClass, ParseableOptions expectedTypes) =>
  Property propertyClass expectedTypes ->
  ParserOf classes (Options expectedTypes)
requireProperty property = do
  mProperty <- lookupProperty property
  case mProperty of
    Nothing -> fail $ unwords ["Property not found: ", show (propertyName property)]
    Just options -> pure options

-- | Lookup a property in a 'Class', that is itself a class.
lookupPropertyClass ::
  Inherits classes propertyClass =>
  Property propertyClass expectedTypes ->
  Class clazz superClasses ->
  ParserOf (clazz ': superClasses) a ->
  ParserOf classes (Maybe a)
lookupPropertyClass property clazz classParserFunc = ParserOf $ \o -> do
  case KeyMap.lookup (Key.fromText (propertyName property)) o of
    Nothing -> pure Nothing
    Just v -> Just <$> parseClass clazz classParserFunc v

-- | Lookup a property in a 'Class', that is itself a class.
--
-- All properties are optional, so you may want to use 'lookupPropertyClass' instead.
requirePropertyClass ::
  Inherits classes propertyClass =>
  Property propertyClass expectedTypes ->
  Class clazz superClasses ->
  ParserOf (clazz ': superClasses) a ->
  ParserOf classes a
requirePropertyClass property clazz classParserFunc = ParserOf $ \o -> do
  case KeyMap.lookup (Key.fromText (propertyName property)) o of
    Nothing -> Left $ unwords ["Property not found: ", show (propertyName property)]
    Just v -> parseClass clazz classParserFunc v

-- | Render a value of a class in the `classes` hierarchy.
newtype RenderOf (classes :: [Type]) = RenderOf {unRenderOf :: JSON.Object}
  deriving (Semigroup, Monoid)

-- | Render a class using a given renderer for that class.
renderClass ::
  Foldable f =>
  Class clazz superClasses ->
  f (RenderOf (clazz : superClasses)) ->
  JSON.Value
renderClass clazz renderers =
  let (RenderOf render) = fold renderers
   in Object $ setContext $ setClassType clazz render

setContext :: JSON.Object -> JSON.Object
setContext = KeyMap.insert "@context" (toJSON ("https://schema.org" :: Text))

setClassType :: Class clazz superClasses -> JSON.Object -> JSON.Object
setClassType clazz o = KeyMap.insert "@type" (toJSON (className clazz)) o

-- Whether the given 'actualType' is in the 'expectedTypes' list.
class IsExpectedType expectedTypes actualType

instance {-# OVERLAPS #-} IsExpectedType (actualType ': otherTypes) actualType

instance IsExpectedType otherTypes actualType => IsExpectedType (otherType ': otherTypes) actualType

-- | Render a property
renderProperty ::
  ( Inherits classes propertyClass,
    IsExpectedType expectedTypes actualType,
    ToJSON actualType
  ) =>
  Property propertyClass expectedTypes ->
  actualType ->
  RenderOf classes
renderProperty property actualValue =
  RenderOf $
    KeyMap.singleton
      (Key.fromText (propertyName property))
      (toJSON actualValue)

-- | Render a property that has only one possible expected type
renderSimpleProperty ::
  ( Inherits classes propertyClass,
    ToJSON propertyType
  ) =>
  Property propertyClass '[propertyType] ->
  propertyType ->
  RenderOf classes
renderSimpleProperty = renderProperty

-- | Render a property that is a class
renderPropertyClass ::
  (Inherits classes propertyClass, Foldable f) =>
  Property propertyClass expectedTypes ->
  Class clazz superClasses ->
  f (RenderOf (clazz : superClasses)) ->
  RenderOf classes
renderPropertyClass property clazz renderers =
  RenderOf $
    KeyMap.singleton
      (Key.fromText (propertyName property))
      (renderClass clazz renderers)
