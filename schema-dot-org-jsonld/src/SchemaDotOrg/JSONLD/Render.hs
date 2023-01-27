{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module SchemaDotOrg.JSONLD.Render
  ( -- ** Rendering
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

import Data.Aeson hiding (Options)
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.Aeson.Types as JSON
import Data.Foldable
import Data.Kind
import Data.Text (Text)
import SchemaDotOrg

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
