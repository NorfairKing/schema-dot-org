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
    lookupPropertyTextLenient,
    requirePropertyText,
    requirePropertyTextLenient,
    lookupPropertySingleValue,
    interpretText,
    interpretTextLenient,
    interpretNumber,
    interpretSingleValue,
    interpretFirstValue,
    interpretAnyListValues,
    interpretAllListValues,
    interpretNothing,
    interpretLeft,
    lookupPropertyClass,
    requirePropertyClass,
    require,
    forgiveError,
    listError,
    eitherChoices,
  )
where

import Control.Applicative
import Control.Arrow (left, second)
import Control.Monad
import Data.Aeson hiding (Options)
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.Aeson.Types as JSON
import Data.Either
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
parseValues :: ParserOf classes a -> Value -> [Either [String] a]
parseValues parser = \case
  Object o ->
    runParserOf parser o : case KeyMap.lookup "@graph" o of
      Nothing -> []
      Just (Object o') -> [runParserOf parser o']
      Just (Array as) -> flip mapMaybe (toList as) $ \case
        Object o' -> Just $ runParserOf parser o'
        _ -> Nothing
      Just _ -> []
  Array as -> flip mapMaybe (toList as) $ \case
    Object o -> Just $ runParserOf parser o
    _ -> Nothing
  _ -> []

-- | Like 'parseValues', but return the first match or the first error.
parseValue :: ParserOf classes a -> Value -> Either [String] a
parseValue parser = eitherChoices' . parseValues parser

-- | A parser for a given class. The type-level list `classes` contains the
-- class itself, and its transitive superclasses.
newtype ParserOf (classes :: [Type]) (a :: Type) = ParserOf {unParser :: JSON.Object -> Either [String] a}

instance Functor (ParserOf classes) where
  fmap f (ParserOf func) = ParserOf $ \o ->
    f <$> func o

instance Applicative (ParserOf classes) where
  pure a = ParserOf $ \_ -> pure a
  (ParserOf ff) <*> (ParserOf fa) = ParserOf $ \o ->
    case (ff o, fa o) of
      (Right f, Right a) -> Right (f a)
      (Left errs, Right _) -> Left errs
      (Right _, Left errs) -> Left errs
      (Left errs1, Left errs2) -> Left (errs1 ++ errs2)

instance Alternative (ParserOf classes) where
  empty = ParserOf $ \_ -> Left ["empty"]
  (ParserOf fa) <|> (ParserOf fb) =
    -- Take the first result or the last error
    ParserOf $ \o -> case (fa o, fb o) of
      (Right a, _) -> Right a
      (Left _, Right b) -> Right b
      (Left errs1, Left errs2) -> Left (errs1 ++ errs2)

instance Monad (ParserOf classes) where
  (ParserOf fa) >>= func = ParserOf $ \o -> do
    a <- fa o
    let (ParserOf fb) = func a
    fb o

instance MonadFail (ParserOf classes) where
  fail err = ParserOf $ \_ -> Left [err]

-- | Run a 'ParserOf' on a JSON 'Object' directly.
--
-- You probably want to use 'parseClass' instead.
runParserOf :: ParserOf classes a -> JSON.Object -> Either [String] a
runParserOf (ParserOf parseFunc) o = parseFunc o

-- | Parse a value of a given class, from a 'JSON.Value'.
parseClass ::
  Class clazz superClasses ->
  -- | How to parse the value
  ParserOf (clazz ': superClasses) a ->
  JSON.Value ->
  Either [String] a
parseClass clazz parser = \case
  Object o -> runParserOf parser o
  _ -> Left ["Not a JSON Object while parsing class: " <> show (className clazz)]

-- | Check that "https://schema.org" is mentioned in the @@context@ property
checkContext :: ParserOf classes ()
checkContext = ParserOf $ \o -> case KeyMap.lookup "@context" o of
  Nothing -> Left ["Key '@context' not found."]
  Just (String "https://schema.org") -> pure ()
  Just v -> Left ["Key '@context' was not 'https://schema.org': " <> show v]

-- | Check that the given class is mentioned in the @@type@ property
checkClass ::
  (Inherits classes clazz) =>
  Class clazz superclasses ->
  ParserOf classes ()
checkClass clazz = ParserOf $ \o -> case KeyMap.lookup "@type" o of
  Nothing -> Left ["Key '@type' not found."]
  Just (String t) ->
    if t == className clazz
      then pure ()
      else
        Left
          [ unwords
              [ "Type mismatch; actual:",
                show t,
                ", expected: ",
                show (className clazz)
              ]
          ]
  Just _ -> Left ["Key '@type' found but was not a String"]

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
    Just v -> Just <$> listError (JSON.parseEither parseJSON v)

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
  (Inherits classes propertyClass) =>
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
  (Inherits classes propertyClass) =>
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
  (Show interpretation) =>
  (Inherits classes propertyClass) =>
  -- | Property
  Property propertyClass expectedTypes ->
  Interpretation expectedTypes (Either [String] interpretation) ->
  ParserOf classes interpretation
requirePropertyEInterpretation property interpretation = do
  interpretations <- requirePropertyInterpretation property interpretation
  require' $ eitherChoices' interpretations

-- | Lookup a property and try to interpret every possible value, then choose
-- the first 'Just'.
--
-- You may want to just use 'requirePropertyEInterpretation' with 'optional' instead.
lookupPropertyMInterpretation ::
  (Inherits classes propertyClass) =>
  -- | Property
  Property propertyClass expectedTypes ->
  Interpretation expectedTypes (Maybe interpretation) ->
  ParserOf classes (Maybe interpretation)
lookupPropertyMInterpretation property interpretation = (asum =<<) <$> lookupPropertyInterpretation property interpretation

-- | Lookup a property that may only be 'Text'.
--
-- Interpret literal text as text, and anything else as Nothing.
lookupPropertyText ::
  (Inherits classes propertyClass) =>
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

-- | Lookup a property that may only be 'Text' using 'interpretTextLenient'.
lookupPropertyTextLenient ::
  (Inherits classes propertyClass) =>
  (Text -> Bool) ->
  -- | Property
  Property propertyClass '[Text] ->
  ParserOf classes (Maybe Text)
lookupPropertyTextLenient languagePredicate property =
  lookupPropertyMInterpretation
    property
    ( InterpretProperty
        (forgiveError <$> interpretTextLenient languagePredicate)
        (EmptyInterpretation (const Nothing))
    )

-- | Require a property that may only be 'Text'.
--
-- Interpret literal text as text, and anything else as Nothing.
requirePropertyText ::
  (Inherits classes propertyClass) =>
  -- | Property
  Property propertyClass '[Text] ->
  ParserOf classes Text
requirePropertyText property =
  requirePropertyEInterpretation
    property
    ( InterpretProperty
        interpretText
        (EmptyInterpretation (const (Left ["Could not parse value as Text value."])))
    )

-- | Require a property that may only be 'Text' using 'interpretTextLenient'.
requirePropertyTextLenient ::
  (Inherits classes propertyClass) =>
  (Text -> Bool) ->
  -- | Property
  Property propertyClass '[Text] ->
  ParserOf classes Text
requirePropertyTextLenient languagePredicate property =
  requirePropertyEInterpretation
    property
    ( InterpretProperty
        (interpretTextLenient languagePredicate)
        (EmptyInterpretation (const (Left ["Could not parse value as Text value."])))
    )

-- | Lookup a property that may only be of a given single value.
--
-- This is for enumerations, for example.
lookupPropertySingleValue ::
  (FromJSON a) =>
  (Inherits classes propertyClass) =>
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
              Array a ->
                let l = toList a
                 in case parseI18N l of
                      Just i18n -> ActualI18N (map (second (parseEither' parseJSON)) i18n)
                      _ -> ActualList (map (parseEither' parseJSON) l)
              _ -> ActualSingle (parseEither' parseJSON v)
         in func actual : go i
      InterpretClass parser (Interpret func) i ->
        let actual =
              case v of
                Array a ->
                  let l = toList a
                   in case parseI18N l of
                        Just i18n -> ActualI18N (map (second (parseEither' parseJSON >=> runParserOf parser)) i18n)
                        _ -> ActualList (map (parseEither' parseJSON >=> runParserOf parser) l)
                _ -> ActualSingle ((parseEither' parseJSON >=> runParserOf parser) v)
         in func actual : go i

parseI18N :: [JSON.Value] -> Maybe [(Text, JSON.Value)]
parseI18N vs = mapM parseSingle vs
  where
    parseSingle :: JSON.Value -> Maybe (Text, JSON.Value)
    parseSingle v = case v of
      Object o -> do
        lang <- KeyMap.lookup "@language" o
        case lang of
          String t -> do
            val <- KeyMap.lookup "@value" o
            pure (t, val)
          _ -> Nothing
      _ -> Nothing

-- | An 'Interpretation' of a property
--
-- An interpretation of a property that can have the given list of types, to
-- produce a list of interpretations of type 'a'.
data Interpretation (expectedTypes :: [Type]) a where
  -- | The default value, if all else fails.
  EmptyInterpretation :: (JSON.Value -> a) -> Interpretation '[] a
  -- | An interpretation of a property that has a given 'expectedType' that has a 'FromJSON' instance.
  InterpretProperty ::
    (FromJSON expectedType) =>
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
  = ActualSingle (Either [String] expectedType)
  | ActualList [Either [String] expectedType]
  | ActualI18N [(Text, Either [String] expectedType)]

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
interpretText :: Interpret Text (Either [String] Text)
interpretText = interpretSingleValue

-- | Interpret a single 'Text' with a lenient parser.
-- This uses "T.unlines" on lists of text, and chooses the text in the given
-- language matching the given predicate or the first one if no such language
-- is found.
interpretTextLenient :: (Text -> Bool) -> Interpret Text (Either [String] Text)
interpretTextLenient langPredicate = Interpret $ \case
  ActualSingle t -> t
  ActualList ts -> T.unlines <$> sequence ts
  ActualI18N ts -> case find (langPredicate . fst) ts of
    Nothing -> case ts of
      [] -> Left ["No I18N value found."]
      ((_, t) : _) -> t
    Just (_, t) -> t

-- | Interpret a single 'Number'
interpretNumber :: Interpret Number (Either [String] Number)
interpretNumber = interpretSingleValue

-- | Interpret only a single value, not a list.
interpretSingleValue :: Interpret a (Either [String] a)
interpretSingleValue = Interpret $ \case
  ActualSingle ee -> ee
  ActualList _ -> Left ["Lists of values are not interpreted in interpretSingleValue."]
  ActualI18N _ -> Left ["I18N values are not interpreted in interpretSingleValue."]

-- | Intepret the first 'Right' in either a single value or a list.
interpretFirstValue :: Interpret a (Either [String] a)
interpretFirstValue = Interpret $ \case
  ActualSingle ee -> ee
  ActualList ees -> eitherChoices' ees
  ActualI18N ees -> eitherChoices' $ map snd ees

-- | Intepret any parseable list values
interpretAnyListValues :: Interpret a [a]
interpretAnyListValues = Interpret $ \case
  ActualSingle ee -> case ee of
    Left _ -> []
    Right a -> [a]
  ActualList ees -> snd $ partitionEithers ees
  ActualI18N ees -> snd $ partitionEithers $ map snd ees

-- | Intepret values as a list, but only if they're all parseable.
interpretAllListValues :: Interpret a [a]
interpretAllListValues = Interpret $ \a ->
  let errOrList = case a of
        ActualSingle ee -> (: []) <$> ee
        ActualList ees -> sequence ees
        ActualI18N ees -> mapM snd ees
   in case errOrList of
        Left _ -> []
        Right as -> as

-- | Don't try to interpret anything, and return 'Nothing'.
interpretNothing :: Interpret a (Maybe b)
interpretNothing = pure Nothing

-- | Don't try to interpret anything, and return 'Left' with the given 'e'.
interpretLeft :: e -> Interpret a (Either e b)
interpretLeft e = pure (Left e)

-- | Lookup a property in a 'Class', that is itself a class.
lookupPropertyClass ::
  (Inherits classes propertyClass) =>
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
  (Inherits classes propertyClass) =>
  -- | Property
  Property propertyClass expectedTypes ->
  -- | Property class
  Class clazz superClasses ->
  -- | Property class parser
  ParserOf (clazz ': superClasses) a ->
  ParserOf classes a
requirePropertyClass property clazz classParserFunc = ParserOf $ \o -> do
  case KeyMap.lookup (Key.fromText (propertyName property)) o of
    Nothing -> Left [unwords ["Property not found: ", show (propertyName property)]]
    Just v -> parseClass clazz classParserFunc v

-- | Require a value to be 'Right', fail to parse otherwise.
require :: Either String a -> ParserOf classes a
require errOrRes = ParserOf $ \_ -> listError errOrRes

require' :: Either [String] a -> ParserOf classes a
require' errsOrRes = ParserOf $ \_ -> errsOrRes

parseEither' :: (a -> JSON.Parser b) -> a -> Either [String] b
parseEither' func = listError . JSON.parseEither func

-- | A convenience function for modifiying interpreters.
--
-- > forgiveError = either (const Nothing) Just
forgiveError :: Either e a -> Maybe a
forgiveError = either (const Nothing) Just

eitherChoices :: [Either e a] -> Either [e] a
eitherChoices = go
  where
    go :: [Either e a] -> Either [e] a
    go = \case
      [] -> Left []
      (Right a : _) -> Right a
      (Left err' : rest) -> case go rest of
        Left errs -> Left (err' : errs)
        Right a -> Right a

eitherChoices' :: [Either [e] a] -> Either [e] a
eitherChoices' = left concat . eitherChoices

listError :: Either e a -> Either [e] a
listError = left (: [])
