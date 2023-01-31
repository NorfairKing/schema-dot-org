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
    renderClass,
    RenderOf (..),
    setContext,
    setClassType,
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
  -- | Renderers
  --
  -- This is a 'Foldable' in case you want to use something more performant
  -- than a list, but usually you'll want to use a list.
  -- 'RenderOf classes' is a 'Monoid' so these renderers are just 'fold'-ed,
  -- but they're still a list so that you don't have to use 'fold' yourself.
  f (RenderOf (clazz : superClasses)) ->
  -- | The resulting JSON 'Value'.
  JSON.Value
renderClass clazz renderers =
  let (RenderOf render) = fold renderers
   in Object $ setContext $ setClassType clazz render

-- | Set the @@context@ property to @"https://schema.org"@.
setContext :: JSON.Object -> JSON.Object
setContext = KeyMap.insert "@context" (toJSON ("https://schema.org" :: Text))

-- | Set the @@class@ property to the name of the given class.
setClassType :: Class clazz superClasses -> JSON.Object -> JSON.Object
setClassType clazz o = KeyMap.insert "@type" (toJSON (className clazz)) o

-- | Render a property
--
-- > renderTextProperty = renderProperty
renderTextProperty ::
  ( Inherits classes propertyClass,
    IsExpectedType expectedTypes Text
  ) =>
  -- | Property
  Property propertyClass expectedTypes ->
  -- | Value
  Text ->
  RenderOf classes
renderTextProperty = renderProperty

-- | Render a property for which there is only one possible value.
--
-- > renderTextProperty = renderProperty
renderSimpleProperty ::
  ( Inherits classes propertyClass,
    ToJSON actualType
  ) =>
  -- | Property
  Property propertyClass '[actualType] ->
  -- | Value
  actualType ->
  RenderOf classes
renderSimpleProperty = renderProperty

-- | Render a property.
renderProperty ::
  ( Inherits classes propertyClass,
    IsExpectedType expectedTypes actualType,
    ToJSON actualType
  ) =>
  -- | Property
  Property propertyClass expectedTypes ->
  -- | Value
  --
  -- The value can be of any type that the property allows.
  actualType ->
  RenderOf classes
renderProperty property actualValue =
  renderUnspecifiedProperty
    (Key.fromText (propertyName property))
    (toJSON actualValue)

-- | Render a property that is a class.
renderPropertyClass ::
  (Inherits classes propertyClass, Foldable f) =>
  -- | Property
  Property propertyClass expectedTypes ->
  -- | Class of the value
  Class clazz superClasses ->
  -- | Renderers for the value
  f (RenderOf (clazz : superClasses)) ->
  RenderOf classes
renderPropertyClass property clazz renderers =
  renderUnspecifiedProperty
    (Key.fromText (propertyName property))
    (renderClass clazz renderers)

-- | Render a property that has not been specified.
--
-- Note that this means that consumers may not try to consume this value.
renderUnspecifiedProperty ::
  -- | JSON Key
  JSON.Key ->
  -- | JSON Value
  JSON.Value ->
  RenderOf classes
renderUnspecifiedProperty key value =
  RenderOf $ KeyMap.singleton key value
