{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
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
    Inherits,
    parseClass,
    checkClass,
    lookupProperty,
    requireProperty,
    Interpretation (..),
    lookupPropertyInterpretation,
    lookupPropertyMInterpretation,
    requirePropertyInterpretation,
    requirePropertyEInterpretation,
    lookupPropertyText,
    requirePropertyText,
    lookupPropertySingleValue,
    interpretText,
    interpretNumber,
    interpretSingleValue,
    lookupPropertyClass,
    requirePropertyClass,

    -- ** Rendering
    RenderOf (..),
    renderClass,
    IsExpectedType,
    renderProperty,
    renderPropertyClass,
    renderTextProperty,
    renderSimpleProperty,
    renderUnspecifiedProperty,
  )
where

import Control.Applicative
import Control.Monad
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

-- | Whether a class is in the given inheritance hierarchy
class Inherits classes clazz

instance {-# OVERLAPS #-} Inherits (clazz ': otherClasses) clazz

instance Inherits otherClasses clazz => Inherits (otherClass ': otherClasses) clazz

-- | Parse a value of a given class, from a 'JSON.Value'.
parseClass ::
  Class clazz superClasses ->
  -- | How to parse the value
  ParserOf (clazz ': superClasses) a ->
  JSON.Value ->
  Either String a
parseClass clazz parser value =
  JSON.parseEither (withObject (T.unpack (className clazz)) (either fail pure . runParserOf parser)) value

checkClass ::
  Inherits classes clazz =>
  Class clazz superclasses ->
  ParserOf classes ()
checkClass clazz = ParserOf $ \o -> case KeyMap.lookup "@type" o of
  Nothing -> Left "Key '@type' not found."
  Just (String t) ->
    if t == className clazz
      then pure ()
      else Left "Type mismatch."
  Just _ -> Left "Key '@type' found but was not a String"

runParserOf :: ParserOf (clazz ': superClasses) a -> JSON.Object -> Either String a
runParserOf (ParserOf parseFunc) o = parseFunc o

-- | Lookup a property in a 'Class'.
--
-- TODO consider removing this because it doesn't force you to teal with every option.
lookupProperty ::
  ( Inherits classes propertyClass,
    IsExpectedType expectedTypes actualType,
    FromJSON actualType
  ) =>
  Property propertyClass expectedTypes ->
  ParserOf classes (Maybe actualType)
lookupProperty property = ParserOf $ \o ->
  case KeyMap.lookup (Key.fromText (propertyName property)) o of
    Nothing -> pure Nothing
    Just v -> Just <$> JSON.parseEither parseJSON v

-- | Lookup a property in a 'Class', fail if it wasn't there.
--
-- All properties are optional, so you may want to use 'lookupProperty' instead.
requireProperty ::
  ( Inherits classes propertyClass,
    IsExpectedType expectedTypes actualType,
    FromJSON actualType
  ) =>
  Property propertyClass expectedTypes ->
  ParserOf classes actualType
requireProperty property = do
  mProperty <- lookupProperty property
  case mProperty of
    Nothing -> fail $ unwords ["Property not found: ", show (propertyName property)]
    Just v -> pure v

-- | Lookup a property and interpret every possible value that is expected.
--
-- This forces the consumer to deal with every possible value.
lookupPropertyInterpretation ::
  Inherits classes propertyClass =>
  Property propertyClass expectedTypes ->
  Interpretation expectedTypes interpretation ->
  ParserOf classes (Maybe [interpretation])
lookupPropertyInterpretation property interpretation = ParserOf $ \o ->
  case KeyMap.lookup (Key.fromText (propertyName property)) o of
    Nothing -> pure Nothing
    Just value -> pure $ Just $ runInterpretation value interpretation

requirePropertyInterpretation ::
  Inherits classes propertyClass =>
  Property propertyClass expectedTypes ->
  Interpretation expectedTypes interpretation ->
  ParserOf classes [interpretation]
requirePropertyInterpretation property interpretation = do
  mInterpretations <- lookupPropertyInterpretation property interpretation
  case mInterpretations of
    Nothing -> fail $ unwords ["Property not found: ", show (propertyName property)]
    Just is -> pure is

requirePropertyEInterpretation ::
  Inherits classes propertyClass =>
  Property propertyClass expectedTypes ->
  Interpretation expectedTypes (Either String interpretation) ->
  ParserOf classes interpretation
requirePropertyEInterpretation property interpretation = do
  interpretations <- requirePropertyInterpretation property interpretation
  require $ msum interpretations

-- | Lookup a property and try to interpret every possible value, then choose the first 'Just'.
lookupPropertyMInterpretation ::
  Inherits classes propertyClass =>
  Property propertyClass expectedTypes ->
  Interpretation expectedTypes (Maybe interpretation) ->
  ParserOf classes (Maybe interpretation)
lookupPropertyMInterpretation property interpretation = (msum =<<) <$> lookupPropertyInterpretation property interpretation

-- | Lookup a property that may only be 'Text'.
--
-- Interpret literal text as text, and anything else as Nothing.
lookupPropertyText ::
  Inherits classes propertyClass =>
  Property propertyClass '[Text] ->
  ParserOf classes (Maybe Text)
lookupPropertyText property =
  lookupPropertyMInterpretation
    property
    ( InterpretProperty
        interpretText
        (EmptyInterpretation Nothing)
    )

-- | Require a property that may only be 'Text'.
--
-- Interpret literal text as text, and anything else as Nothing.
requirePropertyText ::
  Inherits classes propertyClass =>
  Property propertyClass '[Text] ->
  ParserOf classes Text
requirePropertyText property =
  requirePropertyEInterpretation
    property
    ( InterpretProperty
        (\ee ees -> msum (ee : either (const []) id ees))
        (EmptyInterpretation (Left "unused"))
    )

require :: Either String a -> ParserOf classes a
require errOrRes = ParserOf $ \_ -> errOrRes

-- | Lookup a property that may only be of a given single value.
--
-- This is for enumerations, for example.
lookupPropertySingleValue ::
  FromJSON a =>
  Inherits classes propertyClass =>
  Property propertyClass '[a] ->
  ParserOf classes (Maybe a)
lookupPropertySingleValue property =
  lookupPropertyMInterpretation
    property
    ( InterpretProperty
        interpretSingleValue
        (EmptyInterpretation Nothing)
    )

interpretText :: Either String Text -> Either String [Either String Text] -> Maybe Text
interpretText = interpretSingleValue

interpretNumber :: Either String Number -> Either String [Either String Number] -> Maybe Number
interpretNumber = interpretSingleValue

interpretSingleValue :: Either String a -> Either String [Either String a] -> Maybe a
interpretSingleValue errOrVal _ = either (const Nothing) Just errOrVal

runInterpretation :: JSON.Value -> Interpretation types a -> [a]
runInterpretation v = go
  where
    go :: Interpretation ts a -> [a]
    go = \case
      EmptyInterpretation defaultValue -> [defaultValue]
      InterpretProperty combiner i ->
        combiner
          (JSON.parseEither parseJSON v)
          (map (JSON.parseEither parseJSON) <$> JSON.parseEither parseJSON v) :
        go i
      InterpretClass parser combiner i ->
        combiner
          (runParserOf parser <$> JSON.parseEither parseJSON v)
          (map (runParserOf parser) <$> JSON.parseEither parseJSON v) :
        go i

data Interpretation (expectedTypes :: [Type]) a where
  EmptyInterpretation :: a -> Interpretation '[] a
  InterpretProperty ::
    FromJSON expectedType =>
    (Either String expectedType -> Either String [Either String expectedType] -> a) ->
    Interpretation otherExpectedTypes a ->
    Interpretation (expectedType ': otherExpectedTypes) a
  InterpretClass ::
    ParserOf (clazz ': superClasses) e ->
    (Either String (Either String e) -> Either String [Either String e] -> a) ->
    Interpretation otherExpectedTypes a ->
    Interpretation (clazz ': otherExpectedTypes) a

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
renderTextProperty ::
  ( Inherits classes propertyClass,
    IsExpectedType expectedTypes Text
  ) =>
  Property propertyClass expectedTypes ->
  Text ->
  RenderOf classes
renderTextProperty = renderProperty

-- | Render a property
renderSimpleProperty ::
  ( Inherits classes propertyClass,
    ToJSON actualType
  ) =>
  Property propertyClass '[actualType] ->
  actualType ->
  RenderOf classes
renderSimpleProperty = renderProperty

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
  renderUnspecifiedProperty
    (Key.fromText (propertyName property))
    (toJSON actualValue)

-- | Render a property that is a class
renderPropertyClass ::
  (Inherits classes propertyClass, Foldable f) =>
  Property propertyClass expectedTypes ->
  Class clazz superClasses ->
  f (RenderOf (clazz : superClasses)) ->
  RenderOf classes
renderPropertyClass property clazz renderers =
  renderUnspecifiedProperty
    (Key.fromText (propertyName property))
    (renderClass clazz renderers)

renderUnspecifiedProperty ::
  JSON.Key ->
  JSON.Value ->
  RenderOf classes
renderUnspecifiedProperty key value =
  RenderOf $ KeyMap.singleton key value
