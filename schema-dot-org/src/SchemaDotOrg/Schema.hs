{-# LANGUAGE PolyKinds #-}

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
  )
where

import Data.Scientific (Scientific)
import Data.Text (Text)
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
