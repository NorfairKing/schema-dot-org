{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}

module SchemaDotOrg.Example where

import Control.Applicative
import Data.Aeson hiding (Options)
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.Aeson.Types as JSON
import Data.Kind
import Data.Proxy
import Data.Scientific (Scientific)
import Data.Text (Text)
import qualified Data.Text as T
import SchemaDotOrg.Generated
import SchemaDotOrg.Schema

exampleEvent :: JSON.Value
exampleEvent =
  renderClass
    classEvent
    [ renderSimpleProperty propertyThingName "Example event",
      renderProperty propertyEventLocation ("Example location" :: Text),
      renderProperty propertyEventEventAttendanceMode OfflineEventAttendanceMode,
      renderPropertyClass
        propertyEventLocation
        classPlace
        [ renderPropertyClass
            propertyPlaceAddress
            classPostalAddress
            [ renderProperty propertyPostalAddressAddressCountry ("Example country" :: Text),
              renderSimpleProperty propertyPostalAddressAddressLocality "Example locality",
              renderSimpleProperty propertyPostalAddressAddressRegion "Example region",
              renderSimpleProperty propertyPostalAddressPostOfficeBoxNumber "Example post office box number",
              renderSimpleProperty propertyPostalAddressPostalCode "Example postal code",
              renderSimpleProperty propertyPostalAddressStreetAddress "example street address"
            ]
        ]
    ]
