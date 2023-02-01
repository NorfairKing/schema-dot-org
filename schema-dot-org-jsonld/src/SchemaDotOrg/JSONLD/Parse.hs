{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module SchemaDotOrg.JSONLD.Parse
  ( -- ** Parsing
    parseValues,
    parseValue,
    ParserOf (..),
    runParserOf,
    parseClass,
    checkContext,
    checkClass,
    lookupProperty,
    requireProperty,
    Interpretation (..),
    Interpret (..),
    Actual (..),
    runInterpretation,
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
    require,
    forgiveError,
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
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import SchemaDotOrg

-- | Try to use the parser where possible.
--
-- This will try to:
-- 1. Parse an object directly
-- 2. Parse a list of objects
-- 3. Parse an object containing an @@graph@ key, if there is one.
--  a) If there is an object in the @@graph@ attribute, try to parse it
--  b) If there is a listo fo objects in the @@graph@ attribute, try to parse each one.
parseValues :: ParserOf classes a -> Value -> [Either String a]
parseValues (ParserOf parserFunc) = \case
  Object o ->
    parserFunc o : case KeyMap.lookup "@graph" o of
      Nothing -> []
      Just (Object o') -> [parserFunc o']
      Just (Array as) -> flip mapMaybe (toList as) $ \case
        Object o' -> Just $ parserFunc o'
        _ -> Nothing
      Just _ -> []
  Array as -> flip mapMaybe (toList as) $ \case
    Object o -> Just $ parserFunc o
    _ -> Nothing
  _ -> []

-- | Like 'parseValues', but return the first match or the first error.
parseValue :: ParserOf classes a -> Value -> Either String a
parseValue parser = msum . parseValues parser

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

-- | Run a 'ParserOf' on a JSON 'Object' directly.
--
-- You probably want to use 'parseClass' instead.
runParserOf :: ParserOf (clazz ': superClasses) a -> JSON.Object -> Either String a
runParserOf (ParserOf parseFunc) o = parseFunc o

-- | Parse a value of a given class, from a 'JSON.Value'.
parseClass ::
  Class clazz superClasses ->
  -- | How to parse the value
  ParserOf (clazz ': superClasses) a ->
  JSON.Value ->
  Either String a
parseClass clazz parser =
  JSON.parseEither $
    withObject
      (T.unpack (className clazz))
      (either fail pure . runParserOf parser)

-- | Check that "https://schema.org" is mentioned in the @@context@ property
checkContext :: ParserOf classes ()
checkContext = undefined

-- | Check that the given class is mentioned in the @@type@ property
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

-- | Lookup a property in a 'Class'.
--
-- Note that this function does not force you to deal with every possible 'expectedType'.
-- Consider using 'lookupPropertyInterpretation' instead.
lookupProperty ::
  ( Inherits classes propertyClass,
    IsExpectedType expectedTypes actualType,
    FromJSON actualType
  ) =>
  -- | Property
  Property propertyClass expectedTypes ->
  ParserOf classes (Maybe actualType)
lookupProperty property = ParserOf $ \o ->
  case KeyMap.lookup (Key.fromText (propertyName property)) o of
    Nothing -> pure Nothing
    Just v -> Just <$> JSON.parseEither parseJSON v

-- | Lookup a property in a 'Class', fail if it wasn't there.
--
-- Note that this function does not force you to deal with every possible 'expectedType'.
-- Consider using 'requirePropertyInterpretation' instead.
--
-- All properties are optional, so you may want to use 'lookupProperty' instead.
requireProperty ::
  ( Inherits classes propertyClass,
    IsExpectedType expectedTypes actualType,
    FromJSON actualType
  ) =>
  -- | Property
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
  -- | Property
  Property propertyClass expectedTypes ->
  Interpretation expectedTypes interpretation ->
  ParserOf classes (Maybe [interpretation])
lookupPropertyInterpretation property interpretation = ParserOf $ \o ->
  case KeyMap.lookup (Key.fromText (propertyName property)) o of
    Nothing -> pure Nothing
    Just value -> pure $ Just $ runInterpretation value interpretation

-- | Lookup a property, require that it's there, and interpret every possible
-- value that is expected.
--
-- This forces the consumer to deal with every possible value.
requirePropertyInterpretation ::
  Inherits classes propertyClass =>
  -- | Property
  Property propertyClass expectedTypes ->
  Interpretation expectedTypes interpretation ->
  ParserOf classes [interpretation]
requirePropertyInterpretation property interpretation = do
  mInterpretations <- lookupPropertyInterpretation property interpretation
  case mInterpretations of
    Nothing -> fail $ unwords ["Property not found: ", show (propertyName property)]
    Just is -> pure is

-- | Lookup a property, require that it's there, interpret every possible value
-- that is expected, and then choose the first 'Right'.
--
-- This forces the consumer to deal with every possible value.
requirePropertyEInterpretation ::
  Inherits classes propertyClass =>
  -- | Property
  Property propertyClass expectedTypes ->
  Interpretation expectedTypes (Either String interpretation) ->
  ParserOf classes interpretation
requirePropertyEInterpretation property interpretation = do
  interpretations <- requirePropertyInterpretation property interpretation
  require $ msum interpretations

-- | Lookup a property and try to interpret every possible value, then choose
-- the first 'Just'.
lookupPropertyMInterpretation ::
  Inherits classes propertyClass =>
  -- | Property
  Property propertyClass expectedTypes ->
  Interpretation expectedTypes (Maybe interpretation) ->
  ParserOf classes (Maybe interpretation)
lookupPropertyMInterpretation property interpretation = (msum =<<) <$> lookupPropertyInterpretation property interpretation

-- | Lookup a property that may only be 'Text'.
--
-- Interpret literal text as text, and anything else as Nothing.
lookupPropertyText ::
  Inherits classes propertyClass =>
  -- | Property
  Property propertyClass '[Text] ->
  ParserOf classes (Maybe Text)
lookupPropertyText property =
  lookupPropertyMInterpretation
    property
    ( InterpretProperty
        (forgiveError <$> interpretText)
        (EmptyInterpretation (const Nothing))
    )

-- | Require a property that may only be 'Text'.
--
-- Interpret literal text as text, and anything else as Nothing.
requirePropertyText ::
  Inherits classes propertyClass =>
  -- | Property
  Property propertyClass '[Text] ->
  ParserOf classes Text
requirePropertyText property =
  requirePropertyEInterpretation
    property
    ( InterpretProperty
        interpretText
        (EmptyInterpretation (const (Left "unused")))
    )

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
        (forgiveError <$> interpretSingleValue)
        (EmptyInterpretation (const Nothing))
    )

-- Run an 'Interpretation' on a given JSON 'Value'.
runInterpretation :: JSON.Value -> Interpretation types a -> [a]
runInterpretation v = go
  where
    go :: Interpretation ts a -> [a]
    go = \case
      EmptyInterpretation mkDefaultValue -> [mkDefaultValue v]
      InterpretProperty (Interpret func) i ->
        let actual = case v of
              Array a -> ActualList (map (JSON.parseEither parseJSON) (toList a))
              _ -> ActualSingle (JSON.parseEither parseJSON v)
         in func actual : go i
      InterpretClass parser (Interpret func) i ->
        let actual = case v of
              Array a -> ActualList (map (JSON.parseEither parseJSON >=> runParserOf parser) (toList a))
              _ -> ActualSingle ((JSON.parseEither parseJSON >=> runParserOf parser) v)
         in func actual : go i

-- | An 'Interpretation' of a property
--
-- An interpretation of a property that can have the given list of types, to
-- produce a list of interpretations of type 'a'.
data Interpretation (expectedTypes :: [Type]) a where
  -- | The default value, if all else fails.
  EmptyInterpretation :: (JSON.Value -> a) -> Interpretation '[] a
  -- | An interpretation of a property that has a given 'expectedType' that has a 'FromJSON' instance.
  InterpretProperty ::
    FromJSON expectedType =>
    Interpret expectedType a ->
    Interpretation otherExpectedTypes a ->
    Interpretation (expectedType ': otherExpectedTypes) a
  -- | An interpretation of a property that is a class.
  InterpretClass ::
    ParserOf (clazz ': superClasses) e ->
    Interpret e a ->
    Interpretation otherExpectedTypes a ->
    Interpretation (clazz ': otherExpectedTypes) a

-- | The value found at a 'Property'
--
-- The schema.org specifications do not specify whether a property will have 0,
-- 1, or a list of values for a given property, so we deal with each option.
data Actual expectedType
  = ActualSingle (Either String expectedType)
  | ActualList [Either String expectedType]

data Interpret (expectedType :: Type) (result :: Type) = Interpret
  { interpretActual :: Actual expectedType -> result
  }

instance Functor (Interpret expectedType) where
  fmap f (Interpret func) = Interpret (f . func)

instance Applicative (Interpret expectedType) where
  pure a = Interpret (const a)
  (Interpret ff) <*> (Interpret fa) = Interpret (\a -> ff a (fa a))

instance Monad (Interpret expectedType) where
  Interpret fa >>= f = Interpret $ \a ->
    let Interpret fb = f (fa a)
     in fb a

-- | Interpret a single 'Text'
interpretText :: Interpret Text (Either String Text)
interpretText = interpretSingleValue

-- | Interpret a single 'Number'
interpretNumber :: Interpret Number (Either String Number)
interpretNumber = interpretSingleValue

-- | Interpret only a single value, not a list.
interpretSingleValue :: Interpret a (Either String a)
interpretSingleValue = Interpret $ \case
  ActualSingle ee -> ee
  ActualList _ -> Left "Lists of values are not interpreted."

-- | Lookup a property in a 'Class', that is itself a class.
lookupPropertyClass ::
  Inherits classes propertyClass =>
  -- | Property
  Property propertyClass expectedTypes ->
  -- | Property class
  Class clazz superClasses ->
  -- | Property class parser
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
  -- | Property
  Property propertyClass expectedTypes ->
  -- | Property class
  Class clazz superClasses ->
  -- | Property class parser
  ParserOf (clazz ': superClasses) a ->
  ParserOf classes a
requirePropertyClass property clazz classParserFunc = ParserOf $ \o -> do
  case KeyMap.lookup (Key.fromText (propertyName property)) o of
    Nothing -> Left $ unwords ["Property not found: ", show (propertyName property)]
    Just v -> parseClass clazz classParserFunc v

-- | Require a value to be 'Right', fail to parse otherwise.
require :: Either String a -> ParserOf classes a
require errOrRes = ParserOf $ \_ -> errOrRes

-- | A convenience function for modifiying interpreters.
--
-- > forgiveError = either (const Nothing) Just
forgiveError :: Either String a -> Maybe a
forgiveError = either (const Nothing) Just
