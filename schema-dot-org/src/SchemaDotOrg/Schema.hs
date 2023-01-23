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

module SchemaDotOrg.Schema where

import Data.Aeson hiding (Options)
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.Aeson.Types as JSON
import Data.Kind
import Data.Proxy
import Data.Text (Text)
import qualified Data.Text as T

-- Imagine this hyrarchy
--
-- A -> B -\
--  \       v
--   -> C -> D
data A

classA :: Class A '[B, C, D]
classA = Class "A"

data B

classB :: Class B '[D]
classB = Class "B"

data C

classC :: Class C '[D]
classC = Class "C"

data D

classD :: Class D '[]
classD = Class "D"

propertyDName :: Property D '[Text]
propertyDName = Property "name"

propertyBE :: Property B '[E]
propertyBE = Property "B"

data E

classE :: Class E '[]
classE = Class "E"

propertyEAddress :: Property E '[Text]
propertyEAddress = Property "address"

data Class clazz superClasses = Class Text

data Property clazz expectedTypes = Property Text

data Options expectedTypes where
  UnexpectedOption :: Value -> Options '[]
  ExpectedOption :: Either String a -> Options otherExpectedType -> Options (a ': otherExpectedType)

class ParseableOptions expectedTypes where
  parseOptions :: JSON.Value -> Options expectedTypes

instance ParseableOptions '[] where
  parseOptions = UnexpectedOption

instance
  (FromJSON firstExpectedType, ParseableOptions otherExpectedTypes) =>
  ParseableOptions (firstExpectedType ': otherExpectedTypes)
  where
  parseOptions v = ExpectedOption (JSON.parseEither parseJSON v) (parseOptions v)

lookupProperty ::
  (Inherits classes propertyClass, ParseableOptions expectedTypes) =>
  Property propertyClass expectedTypes ->
  ParserOf classes (Maybe (Options expectedTypes))
lookupProperty (Property propertyName) = ParserOf $ \o ->
  case KeyMap.lookup (Key.fromText propertyName) o of
    Nothing -> pure Nothing
    Just v -> pure $ Just $ parseOptions v

requireProperty ::
  (Inherits classes propertyClass, ParseableOptions expectedTypes) =>
  Property propertyClass expectedTypes ->
  ParserOf classes (Options expectedTypes)
requireProperty property@(Property propertyName) = do
  mProperty <- lookupProperty property
  case mProperty of
    Nothing -> fail $ unwords ["Property not found: ", show propertyName]
    Just options -> pure options

class Inherits classes clazz

instance {-# OVERLAPS #-} Inherits (clazz ': otherClasses) clazz

instance Inherits otherClasses clazz => Inherits (otherClass ': otherClasses) clazz

newtype ParserOf (classes :: [Type]) a = ParserOf {unParser :: JSON.Object -> Either String a}

instance Functor (ParserOf classes) where
  fmap f (ParserOf func) = ParserOf $ \o ->
    f <$> func o

instance Applicative (ParserOf classes) where
  pure a = ParserOf $ \_ -> pure a
  (ParserOf ff) <*> (ParserOf fa) = ParserOf $ \o -> do
    f <- ff o
    a <- fa o
    pure (f a)

instance Monad (ParserOf classes) where
  (ParserOf fa) >>= func = ParserOf $ \o -> do
    a <- fa o
    let (ParserOf fb) = func a
    fb o

instance MonadFail (ParserOf classes) where
  fail err = ParserOf $ \_ -> Left err

parseClass :: Class clazz superClasses -> ParserOf (clazz : superClasses) a -> JSON.Value -> Either String a
parseClass (Class className) (ParserOf parseFunc) value =
  JSON.parseEither (withObject (T.unpack className) (either fail pure . parseFunc)) value

newtype RenderOf (classes :: [Type]) = RenderOf {unRenderOf :: JSON.Object}
  deriving (Semigroup, Monoid)

renderClass :: Class clazz superClasses -> RenderOf (clazz : superClasses) -> JSON.Value
renderClass (Class _) (RenderOf render) = Object render

class IsExpectedType expectedTypes actualType

instance {-# OVERLAPS #-} IsExpectedType (actualType ': otherTypes) actualType

instance IsExpectedType otherTypes actualType => IsExpectedType (otherType ': otherTypes) actualType

renderProperty ::
  ( Inherits classes propertyClass,
    IsExpectedType expectedTypes actualType,
    ToJSON actualType
  ) =>
  Property propertyClass expectedTypes ->
  actualType ->
  RenderOf classes
renderProperty (Property propertyName) actualValue =
  RenderOf $ KeyMap.singleton (Key.fromText propertyName) (toJSON actualValue)

renderPropertyClass ::
  (Inherits classes propertyClass) =>
  Property propertyClass expectedTypes ->
  Class clazz superClasses ->
  RenderOf (clazz : superClasses) ->
  RenderOf classes
renderPropertyClass (Property propertyName) (Class className) (RenderOf object) =
  RenderOf $ KeyMap.singleton (Key.fromText propertyName) (toJSON object)

exampleValue :: JSON.Value
exampleValue =
  object
    [ ("@type", "A"),
      ("name", "foobar")
    ]

exampleNameParsing :: Either String Text
exampleNameParsing = flip (parseClass classA) exampleValue $ do
  ExpectedOption errOrResult (UnexpectedOption _) <- requireProperty propertyDName
  case errOrResult of
    Left err -> fail err
    Right result -> pure result

exampleARendering :: JSON.Value
exampleARendering =
  renderClass
    classA
    ( mconcat
        [ renderProperty propertyDName ("quux" :: Text),
          renderPropertyClass propertyBE classB $
            mconcat
              [ renderProperty propertyDName ("quux" :: Text)
              ]
        ]
    )
