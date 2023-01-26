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
    [ renderProperty propertyThingName ("Example event" :: Text),
      renderProperty propertyEventLocation ("Example location" :: Text),
      renderProperty propertyEventEventAttendanceMode EventAttendanceModeEnumerationOfflineEventAttendanceMode,
      renderPropertyClass
        propertyEventLocation
        classPlace
        [ renderPropertyClass
            propertyPlaceAddress
            classPostalAddress
            [ renderProperty propertyPostalAddressAddressCountry ("Example country" :: Text),
              renderProperty propertyPostalAddressAddressLocality ("Example locality" :: Text),
              renderProperty propertyPostalAddressAddressRegion ("Example region" :: Text),
              renderProperty propertyPostalAddressPostOfficeBoxNumber ("Example post office box number" :: Text),
              renderProperty propertyPostalAddressPostalCode ("Example postal code" :: Text),
              renderProperty propertyPostalAddressStreetAddress ("example street address" :: Text)
            ]
        ]
    ]
