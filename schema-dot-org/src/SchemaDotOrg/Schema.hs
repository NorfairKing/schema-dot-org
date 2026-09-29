{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module SchemaDotOrg.Schema
  ( -- * Schema declaration
    Class (..),
    Property (..),
    Inherits,
    IsExpectedType,

    -- ** Data types
    Boolean,
    Number,
    Date,
    Time,
    DateTime,
  )
where

import Data.Kind
import Data.Scientific (Scientific)
import Data.Text (Text)
import Data.Time as Time

-- [tag:PrimitiveTypes] The generator must not generate a type for any
-- schema.org type that already has a Haskell one, so it carries its own list of
-- their schema.org names.  The ones below are here; @Text@ comes from
-- "Data.Text" and @Integer@ and @Float@ from the Prelude.  Adding or dropping
-- one of them means changing that list too.
type Boolean = Bool

type Date = Time.Day -- TODO

type Time = Time.TimeOfDay -- TODO

type DateTime = Time.LocalTime -- TODO

type Number = Scientific

-- | A class schema.
--
-- Existence of a value of this type (in this library) implies that there is a
-- class schema on schema.org called `clazz` with superclasses `superClasses`.
data Class (clazz :: Type) (superClasses :: [Type]) = Class {className :: Text}

-- | A class schema.
--
-- Existence of a value of this type (in this library) implies that there is a
-- property schema on schema.org for class `clazz` with expected types `expectedTypes`.
data Property (clazz :: Type) (expectedTypes :: [Type]) = Property {propertyName :: Text}

-- | Whether a class is in the given inheritance hierarchy
class Inherits classes clazz

instance {-# OVERLAPS #-} Inherits (clazz ': otherClasses) clazz

instance (Inherits otherClasses clazz) => Inherits (otherClass ': otherClasses) clazz

-- Whether the given 'actualType' is in the 'expectedTypes' list.
class IsExpectedType expectedTypes actualType

instance {-# OVERLAPS #-} IsExpectedType (actualType ': otherTypes) actualType

instance (IsExpectedType otherTypes actualType) => IsExpectedType (otherType ': otherTypes) actualType
