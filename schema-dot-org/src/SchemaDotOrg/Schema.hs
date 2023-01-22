{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeOperators #-}

module SchemaDotOrg.Schema where

import Data.Aeson hiding (Options)
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.Types as JSON
import Data.Kind
import Data.Proxy
import Data.Text (Text)

-- Imagine this hyrarchy
--
-- A -> B -\
--  \       v
--   -> C -> D
data A

classA :: Class A '[B, C]
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

data Class clazz superClasses = Class Text

data Property clazz expectedTypes = Property Text

data Options expectedTypes where
  UnexpectedOption :: Value -> Options '[]
  ExpectedOption :: Either String a -> Options otherExpectedType -> Options (a ': otherExpectedType)

class ParseableOptions expectedTypes where
  parseOptions :: JSON.Value -> Options expectedTypes

instance ParseableOptions '[] where
  parseOptions = UnexpectedOption

instance (FromJSON firstExpectedType, ParseableOptions otherExpectedTypes) => ParseableOptions (firstExpectedType ': otherExpectedTypes) where
  parseOptions v = ExpectedOption (JSON.parseEither parseJSON v) (parseOptions v)

parseProperty ::
  (Inherits (clazz ': superClasses) propertyClass, ParseableOptions expectedTypes) =>
  Class clazz superClasses ->
  Property propertyClass expectedTypes ->
  (JSON.Object -> JSON.Parser (Maybe (Options expectedTypes)))
parseProperty (Class _) (Property propertyName) o = do
  mValue <- o .:? Key.fromText propertyName
  pure $ parseOptions <$> mValue

class Inherits classes clazz

instance Inherits '[clazz] clazz
