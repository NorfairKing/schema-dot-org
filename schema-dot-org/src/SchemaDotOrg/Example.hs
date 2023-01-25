{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}

module SchemaDotOrg.Example where

import qualified Data.Aeson.Types as JSON
import Data.Text (Text)
import SchemaDotOrg.Generated
import SchemaDotOrg.Schema

exampleEvent :: JSON.Value
exampleEvent =
  renderClass
    classEvent
    [ renderSimpleProperty propertyThingName "Example event",
      renderProperty propertyEventLocation ("Example location" :: Text),
      renderProperty propertyEventEventAttendanceMode EventAttendanceModeOfflineEventAttendanceMode,
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
