{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE EmptyDataDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module SchemaDotOrg.Generated where

import Data.Aeson (FromJSON (..), ToJSON (..), Value, withText)
import Data.Text (Text)
import GHC.Generics (Generic)
import SchemaDotOrg.Schema

data DModel

classDModel :: Class DModel '[MediaObject, CreativeWork, Thing]
classDModel = Class "DModel"

data AMRadioChannel

classAMRadioChannel ::
  Class
    AMRadioChannel
    '[ RadioChannel,
       BroadcastChannel,
       Intangible,
       Thing
     ]
classAMRadioChannel = Class "AMRadioChannel"

data APIReference

classAPIReference ::
  Class APIReference '[TechArticle, Article, CreativeWork, Thing]
classAPIReference = Class "APIReference"

data AboutPage

classAboutPage :: Class AboutPage '[WebPage, CreativeWork, Thing]
classAboutPage = Class "AboutPage"

data AcceptAction

classAcceptAction ::
  Class AcceptAction '[AllocateAction, OrganizeAction, Action, Thing]
classAcceptAction = Class "AcceptAction"

data Accommodation

classAccommodation :: Class Accommodation '[Place, Thing]
classAccommodation = Class "Accommodation"

data AccountingService

classAccountingService ::
  Class
    AccountingService
    '[ FinancialService,
       LocalBusiness,
       Place,
       Organization,
       Thing,
       Thing
     ]
classAccountingService = Class "AccountingService"

data AchieveAction

classAchieveAction :: Class AchieveAction '[Action, Thing]
classAchieveAction = Class "AchieveAction"

data Action

classAction :: Class Action '[Thing]
classAction = Class "Action"

data ActionAccessSpecification

classActionAccessSpecification ::
  Class ActionAccessSpecification '[Intangible, Thing]
classActionAccessSpecification = Class "ActionAccessSpecification"

data ActionStatusType
  = ActionStatusTypeActiveActionStatus
  | ActionStatusTypeCompletedActionStatus
  | ActionStatusTypeFailedActionStatus
  | ActionStatusTypePotentialActionStatus
  deriving (Show, Eq, Ord, Generic)

instance FromJSON ActionStatusType where
  parseJSON =
    withText
      "ActionStatusType"
      ( \case
          "https://schema.org/ActiveActionStatus" ->
            pure ActionStatusTypeActiveActionStatus
          "https://schema.org/CompletedActionStatus" ->
            pure ActionStatusTypeCompletedActionStatus
          "https://schema.org/FailedActionStatus" ->
            pure ActionStatusTypeFailedActionStatus
          "https://schema.org/PotentialActionStatus" ->
            pure ActionStatusTypePotentialActionStatus
          t -> fail ("Failed to parse ActionStatusType: " <> show t)
      )

instance ToJSON ActionStatusType where
  toJSON =
    (toJSON :: Text -> Value)
      . ( \case
            ActionStatusTypeActiveActionStatus ->
              "https://schema.org/ActiveActionStatus"
            ActionStatusTypeCompletedActionStatus ->
              "https://schema.org/CompletedActionStatus"
            ActionStatusTypeFailedActionStatus ->
              "https://schema.org/FailedActionStatus"
            ActionStatusTypePotentialActionStatus ->
              "https://schema.org/PotentialActionStatus"
        )

data ActivateAction

classActivateAction ::
  Class ActivateAction '[ControlAction, Action, Thing]
classActivateAction = Class "ActivateAction"

data AddAction

classAddAction :: Class AddAction '[UpdateAction, Action, Thing]
classAddAction = Class "AddAction"

data AdministrativeArea

classAdministrativeArea :: Class AdministrativeArea '[Place, Thing]
classAdministrativeArea = Class "AdministrativeArea"

data AdultEntertainment

classAdultEntertainment ::
  Class
    AdultEntertainment
    '[ EntertainmentBusiness,
       LocalBusiness,
       Place,
       Organization,
       Thing,
       Thing
     ]
classAdultEntertainment = Class "AdultEntertainment"

data AdultOrientedEnumeration
  = AdultOrientedEnumerationAlcoholConsideration
  | AdultOrientedEnumerationDangerousGoodConsideration
  | AdultOrientedEnumerationHealthcareConsideration
  | AdultOrientedEnumerationNarcoticConsideration
  | AdultOrientedEnumerationReducedRelevanceForChildrenConsideration
  | AdultOrientedEnumerationSexualContentConsideration
  | AdultOrientedEnumerationTobaccoNicotineConsideration
  | AdultOrientedEnumerationUnclassifiedAdultConsideration
  | AdultOrientedEnumerationViolenceConsideration
  | AdultOrientedEnumerationWeaponConsideration
  deriving (Show, Eq, Ord, Generic)

instance FromJSON AdultOrientedEnumeration where
  parseJSON =
    withText
      "AdultOrientedEnumeration"
      ( \case
          "https://schema.org/AlcoholConsideration" ->
            pure AdultOrientedEnumerationAlcoholConsideration
          "https://schema.org/DangerousGoodConsideration" ->
            pure AdultOrientedEnumerationDangerousGoodConsideration
          "https://schema.org/HealthcareConsideration" ->
            pure AdultOrientedEnumerationHealthcareConsideration
          "https://schema.org/NarcoticConsideration" ->
            pure AdultOrientedEnumerationNarcoticConsideration
          "https://schema.org/ReducedRelevanceForChildrenConsideration" ->
            pure
              AdultOrientedEnumerationReducedRelevanceForChildrenConsideration
          "https://schema.org/SexualContentConsideration" ->
            pure AdultOrientedEnumerationSexualContentConsideration
          "https://schema.org/TobaccoNicotineConsideration" ->
            pure AdultOrientedEnumerationTobaccoNicotineConsideration
          "https://schema.org/UnclassifiedAdultConsideration" ->
            pure AdultOrientedEnumerationUnclassifiedAdultConsideration
          "https://schema.org/ViolenceConsideration" ->
            pure AdultOrientedEnumerationViolenceConsideration
          "https://schema.org/WeaponConsideration" ->
            pure AdultOrientedEnumerationWeaponConsideration
          t -> fail ("Failed to parse AdultOrientedEnumeration: " <> show t)
      )

instance ToJSON AdultOrientedEnumeration where
  toJSON =
    (toJSON :: Text -> Value)
      . ( \case
            AdultOrientedEnumerationAlcoholConsideration ->
              "https://schema.org/AlcoholConsideration"
            AdultOrientedEnumerationDangerousGoodConsideration ->
              "https://schema.org/DangerousGoodConsideration"
            AdultOrientedEnumerationHealthcareConsideration ->
              "https://schema.org/HealthcareConsideration"
            AdultOrientedEnumerationNarcoticConsideration ->
              "https://schema.org/NarcoticConsideration"
            AdultOrientedEnumerationReducedRelevanceForChildrenConsideration ->
              "https://schema.org/ReducedRelevanceForChildrenConsideration"
            AdultOrientedEnumerationSexualContentConsideration ->
              "https://schema.org/SexualContentConsideration"
            AdultOrientedEnumerationTobaccoNicotineConsideration ->
              "https://schema.org/TobaccoNicotineConsideration"
            AdultOrientedEnumerationUnclassifiedAdultConsideration ->
              "https://schema.org/UnclassifiedAdultConsideration"
            AdultOrientedEnumerationViolenceConsideration ->
              "https://schema.org/ViolenceConsideration"
            AdultOrientedEnumerationWeaponConsideration ->
              "https://schema.org/WeaponConsideration"
        )

data AdvertiserContentArticle

classAdvertiserContentArticle ::
  Class AdvertiserContentArticle '[Article, CreativeWork, Thing]
classAdvertiserContentArticle = Class "AdvertiserContentArticle"

data AggregateOffer

classAggregateOffer ::
  Class AggregateOffer '[Offer, Intangible, Thing]
classAggregateOffer = Class "AggregateOffer"

data AggregateRating

classAggregateRating ::
  Class AggregateRating '[Rating, Intangible, Thing]
classAggregateRating = Class "AggregateRating"

data AgreeAction

classAgreeAction ::
  Class AgreeAction '[ReactAction, AssessAction, Action, Thing]
classAgreeAction = Class "AgreeAction"

data Airline

classAirline :: Class Airline '[Organization, Thing]
classAirline = Class "Airline"

data Airport

classAirport :: Class Airport '[CivicStructure, Place, Thing]
classAirport = Class "Airport"

data AlignmentObject

classAlignmentObject :: Class AlignmentObject '[Intangible, Thing]
classAlignmentObject = Class "AlignmentObject"

data AllocateAction

classAllocateAction ::
  Class AllocateAction '[OrganizeAction, Action, Thing]
classAllocateAction = Class "AllocateAction"

data AmpStory

classAmpStory ::
  Class
    AmpStory
    '[ MediaObject,
       CreativeWork,
       CreativeWork,
       Thing,
       Thing
     ]
classAmpStory = Class "AmpStory"

data AmusementPark

classAmusementPark ::
  Class
    AmusementPark
    '[ EntertainmentBusiness,
       LocalBusiness,
       Place,
       Organization,
       Thing,
       Thing
     ]
classAmusementPark = Class "AmusementPark"

data AnalysisNewsArticle

classAnalysisNewsArticle ::
  Class
    AnalysisNewsArticle
    '[ NewsArticle,
       Article,
       CreativeWork,
       Thing
     ]
classAnalysisNewsArticle = Class "AnalysisNewsArticle"

data AnatomicalStructure

classAnatomicalStructure ::
  Class AnatomicalStructure '[MedicalEntity, Thing]
classAnatomicalStructure = Class "AnatomicalStructure"

data AnatomicalSystem

classAnatomicalSystem ::
  Class AnatomicalSystem '[MedicalEntity, Thing]
classAnatomicalSystem = Class "AnatomicalSystem"

data AnimalShelter

classAnimalShelter ::
  Class
    AnimalShelter
    '[ LocalBusiness,
       Place,
       Organization,
       Thing,
       Thing
     ]
classAnimalShelter = Class "AnimalShelter"

data Answer

classAnswer :: Class Answer '[Comment, CreativeWork, Thing]
classAnswer = Class "Answer"

data Apartment

classApartment :: Class Apartment '[Accommodation, Place, Thing]
classApartment = Class "Apartment"

data ApartmentComplex

classApartmentComplex ::
  Class ApartmentComplex '[Residence, Place, Thing]
classApartmentComplex = Class "ApartmentComplex"

data AppendAction

classAppendAction ::
  Class
    AppendAction
    '[ InsertAction,
       AddAction,
       UpdateAction,
       Action,
       Thing
     ]
classAppendAction = Class "AppendAction"

data ApplyAction

classApplyAction ::
  Class ApplyAction '[OrganizeAction, Action, Thing]
classApplyAction = Class "ApplyAction"

data ApprovedIndication

classApprovedIndication ::
  Class ApprovedIndication '[MedicalIndication, MedicalEntity, Thing]
classApprovedIndication = Class "ApprovedIndication"

data Aquarium

classAquarium :: Class Aquarium '[CivicStructure, Place, Thing]
classAquarium = Class "Aquarium"

data ArchiveComponent

classArchiveComponent ::
  Class ArchiveComponent '[CreativeWork, Thing]
classArchiveComponent = Class "ArchiveComponent"

data ArchiveOrganization

classArchiveOrganization ::
  Class
    ArchiveOrganization
    '[ LocalBusiness,
       Place,
       Organization,
       Thing,
       Thing
     ]
classArchiveOrganization = Class "ArchiveOrganization"

data ArriveAction

classArriveAction ::
  Class ArriveAction '[MoveAction, Action, Thing]
classArriveAction = Class "ArriveAction"

data ArtGallery

classArtGallery ::
  Class
    ArtGallery
    '[ EntertainmentBusiness,
       LocalBusiness,
       Place,
       Organization,
       Thing,
       Thing
     ]
classArtGallery = Class "ArtGallery"

data Artery

classArtery ::
  Class Artery '[Vessel, AnatomicalStructure, MedicalEntity, Thing]
classArtery = Class "Artery"

data Article

classArticle :: Class Article '[CreativeWork, Thing]
classArticle = Class "Article"

data AskAction

classAskAction ::
  Class AskAction '[CommunicateAction, InteractAction, Action, Thing]
classAskAction = Class "AskAction"

data AskPublicNewsArticle

classAskPublicNewsArticle ::
  Class
    AskPublicNewsArticle
    '[ NewsArticle,
       Article,
       CreativeWork,
       Thing
     ]
classAskPublicNewsArticle = Class "AskPublicNewsArticle"

data AssessAction

classAssessAction :: Class AssessAction '[Action, Thing]
classAssessAction = Class "AssessAction"

data AssignAction

classAssignAction ::
  Class AssignAction '[AllocateAction, OrganizeAction, Action, Thing]
classAssignAction = Class "AssignAction"

data Atlas

classAtlas :: Class Atlas '[CreativeWork, Thing]
classAtlas = Class "Atlas"

data Attorney

classAttorney ::
  Class
    Attorney
    '[ LegalService,
       LocalBusiness,
       Place,
       Organization,
       Thing,
       Thing
     ]
classAttorney = Class "Attorney"

data Audience

classAudience :: Class Audience '[Intangible, Thing]
classAudience = Class "Audience"

data AudioObject

classAudioObject ::
  Class AudioObject '[MediaObject, CreativeWork, Thing]
classAudioObject = Class "AudioObject"

data AudioObjectSnapshot

classAudioObjectSnapshot ::
  Class
    AudioObjectSnapshot
    '[ AudioObject,
       MediaObject,
       CreativeWork,
       Thing
     ]
classAudioObjectSnapshot = Class "AudioObjectSnapshot"

data Audiobook

classAudiobook ::
  Class
    Audiobook
    '[ AudioObject,
       Book,
       MediaObject,
       CreativeWork,
       Thing,
       CreativeWork,
       Thing
     ]
classAudiobook = Class "Audiobook"

data AuthorizeAction

classAuthorizeAction ::
  Class
    AuthorizeAction
    '[ AllocateAction,
       OrganizeAction,
       Action,
       Thing
     ]
classAuthorizeAction = Class "AuthorizeAction"

data AutoBodyShop

classAutoBodyShop ::
  Class
    AutoBodyShop
    '[ AutomotiveBusiness,
       LocalBusiness,
       Place,
       Organization,
       Thing,
       Thing
     ]
classAutoBodyShop = Class "AutoBodyShop"

data AutoDealer

classAutoDealer ::
  Class
    AutoDealer
    '[ AutomotiveBusiness,
       LocalBusiness,
       Place,
       Organization,
       Thing,
       Thing
     ]
classAutoDealer = Class "AutoDealer"

data AutoPartsStore

classAutoPartsStore ::
  Class
    AutoPartsStore
    '[ Store,
       AutomotiveBusiness,
       LocalBusiness,
       Place,
       Organization,
       Thing,
       Thing,
       LocalBusiness,
       Place,
       Organization,
       Thing,
       Thing
     ]
classAutoPartsStore = Class "AutoPartsStore"

data AutoRental

classAutoRental ::
  Class
    AutoRental
    '[ AutomotiveBusiness,
       LocalBusiness,
       Place,
       Organization,
       Thing,
       Thing
     ]
classAutoRental = Class "AutoRental"

data AutoRepair

classAutoRepair ::
  Class
    AutoRepair
    '[ AutomotiveBusiness,
       LocalBusiness,
       Place,
       Organization,
       Thing,
       Thing
     ]
classAutoRepair = Class "AutoRepair"

data AutoWash

classAutoWash ::
  Class
    AutoWash
    '[ AutomotiveBusiness,
       LocalBusiness,
       Place,
       Organization,
       Thing,
       Thing
     ]
classAutoWash = Class "AutoWash"

data AutomatedTeller

classAutomatedTeller ::
  Class
    AutomatedTeller
    '[ FinancialService,
       LocalBusiness,
       Place,
       Organization,
       Thing,
       Thing
     ]
classAutomatedTeller = Class "AutomatedTeller"

data AutomotiveBusiness

classAutomotiveBusiness ::
  Class
    AutomotiveBusiness
    '[ LocalBusiness,
       Place,
       Organization,
       Thing,
       Thing
     ]
classAutomotiveBusiness = Class "AutomotiveBusiness"

data BackgroundNewsArticle

classBackgroundNewsArticle ::
  Class
    BackgroundNewsArticle
    '[ NewsArticle,
       Article,
       CreativeWork,
       Thing
     ]
classBackgroundNewsArticle = Class "BackgroundNewsArticle"

data Bakery

classBakery ::
  Class
    Bakery
    '[ FoodEstablishment,
       LocalBusiness,
       Place,
       Organization,
       Thing,
       Thing
     ]
classBakery = Class "Bakery"

data BankAccount

classBankAccount ::
  Class BankAccount '[FinancialProduct, Service, Intangible, Thing]
classBankAccount = Class "BankAccount"

data BankOrCreditUnion

classBankOrCreditUnion ::
  Class
    BankOrCreditUnion
    '[ FinancialService,
       LocalBusiness,
       Place,
       Organization,
       Thing,
       Thing
     ]
classBankOrCreditUnion = Class "BankOrCreditUnion"

data BarOrPub

classBarOrPub ::
  Class
    BarOrPub
    '[ FoodEstablishment,
       LocalBusiness,
       Place,
       Organization,
       Thing,
       Thing
     ]
classBarOrPub = Class "BarOrPub"

data Barcode

classBarcode ::
  Class Barcode '[ImageObject, MediaObject, CreativeWork, Thing]
classBarcode = Class "Barcode"

data Beach

classBeach :: Class Beach '[CivicStructure, Place, Thing]
classBeach = Class "Beach"

data BeautySalon

classBeautySalon ::
  Class
    BeautySalon
    '[ HealthAndBeautyBusiness,
       LocalBusiness,
       Place,
       Organization,
       Thing,
       Thing
     ]
classBeautySalon = Class "BeautySalon"

data BedAndBreakfast

classBedAndBreakfast ::
  Class
    BedAndBreakfast
    '[ LodgingBusiness,
       LocalBusiness,
       Place,
       Organization,
       Thing,
       Thing
     ]
classBedAndBreakfast = Class "BedAndBreakfast"

data BedDetails

classBedDetails :: Class BedDetails '[Intangible, Thing]
classBedDetails = Class "BedDetails"

type BedType = Text

data BefriendAction

classBefriendAction ::
  Class BefriendAction '[InteractAction, Action, Thing]
classBefriendAction = Class "BefriendAction"

data BikeStore

classBikeStore ::
  Class
    BikeStore
    '[ Store,
       LocalBusiness,
       Place,
       Organization,
       Thing,
       Thing
     ]
classBikeStore = Class "BikeStore"

data BioChemEntity

classBioChemEntity :: Class BioChemEntity '[Thing]
classBioChemEntity = Class "BioChemEntity"

data Blog

classBlog :: Class Blog '[CreativeWork, Thing]
classBlog = Class "Blog"

data BlogPosting

classBlogPosting ::
  Class
    BlogPosting
    '[ SocialMediaPosting,
       Article,
       CreativeWork,
       Thing
     ]
classBlogPosting = Class "BlogPosting"

data BloodTest

classBloodTest ::
  Class BloodTest '[MedicalTest, MedicalEntity, Thing]
classBloodTest = Class "BloodTest"

data BoardingPolicyType
  = BoardingPolicyTypeGroupBoardingPolicy
  | BoardingPolicyTypeZoneBoardingPolicy
  deriving (Show, Eq, Ord, Generic)

instance FromJSON BoardingPolicyType where
  parseJSON =
    withText
      "BoardingPolicyType"
      ( \case
          "https://schema.org/GroupBoardingPolicy" ->
            pure BoardingPolicyTypeGroupBoardingPolicy
          "https://schema.org/ZoneBoardingPolicy" ->
            pure BoardingPolicyTypeZoneBoardingPolicy
          t -> fail ("Failed to parse BoardingPolicyType: " <> show t)
      )

instance ToJSON BoardingPolicyType where
  toJSON =
    (toJSON :: Text -> Value)
      . ( \case
            BoardingPolicyTypeGroupBoardingPolicy ->
              "https://schema.org/GroupBoardingPolicy"
            BoardingPolicyTypeZoneBoardingPolicy ->
              "https://schema.org/ZoneBoardingPolicy"
        )

data BoatReservation

classBoatReservation ::
  Class BoatReservation '[Reservation, Intangible, Thing]
classBoatReservation = Class "BoatReservation"

data BoatTerminal

classBoatTerminal ::
  Class BoatTerminal '[CivicStructure, Place, Thing]
classBoatTerminal = Class "BoatTerminal"

data BoatTrip

classBoatTrip :: Class BoatTrip '[Trip, Intangible, Thing]
classBoatTrip = Class "BoatTrip"

data BodyMeasurementTypeEnumeration
  = BodyMeasurementTypeEnumerationBodyMeasurementArm
  | BodyMeasurementTypeEnumerationBodyMeasurementBust
  | BodyMeasurementTypeEnumerationBodyMeasurementChest
  | BodyMeasurementTypeEnumerationBodyMeasurementFoot
  | BodyMeasurementTypeEnumerationBodyMeasurementHand
  | BodyMeasurementTypeEnumerationBodyMeasurementHead
  | BodyMeasurementTypeEnumerationBodyMeasurementHeight
  | BodyMeasurementTypeEnumerationBodyMeasurementHips
  | BodyMeasurementTypeEnumerationBodyMeasurementInsideLeg
  | BodyMeasurementTypeEnumerationBodyMeasurementNeck
  | BodyMeasurementTypeEnumerationBodyMeasurementUnderbust
  | BodyMeasurementTypeEnumerationBodyMeasurementWaist
  | BodyMeasurementTypeEnumerationBodyMeasurementWeight
  deriving (Show, Eq, Ord, Generic)

instance FromJSON BodyMeasurementTypeEnumeration where
  parseJSON =
    withText
      "BodyMeasurementTypeEnumeration"
      ( \case
          "https://schema.org/BodyMeasurementArm" ->
            pure BodyMeasurementTypeEnumerationBodyMeasurementArm
          "https://schema.org/BodyMeasurementBust" ->
            pure BodyMeasurementTypeEnumerationBodyMeasurementBust
          "https://schema.org/BodyMeasurementChest" ->
            pure BodyMeasurementTypeEnumerationBodyMeasurementChest
          "https://schema.org/BodyMeasurementFoot" ->
            pure BodyMeasurementTypeEnumerationBodyMeasurementFoot
          "https://schema.org/BodyMeasurementHand" ->
            pure BodyMeasurementTypeEnumerationBodyMeasurementHand
          "https://schema.org/BodyMeasurementHead" ->
            pure BodyMeasurementTypeEnumerationBodyMeasurementHead
          "https://schema.org/BodyMeasurementHeight" ->
            pure BodyMeasurementTypeEnumerationBodyMeasurementHeight
          "https://schema.org/BodyMeasurementHips" ->
            pure BodyMeasurementTypeEnumerationBodyMeasurementHips
          "https://schema.org/BodyMeasurementInsideLeg" ->
            pure BodyMeasurementTypeEnumerationBodyMeasurementInsideLeg
          "https://schema.org/BodyMeasurementNeck" ->
            pure BodyMeasurementTypeEnumerationBodyMeasurementNeck
          "https://schema.org/BodyMeasurementUnderbust" ->
            pure BodyMeasurementTypeEnumerationBodyMeasurementUnderbust
          "https://schema.org/BodyMeasurementWaist" ->
            pure BodyMeasurementTypeEnumerationBodyMeasurementWaist
          "https://schema.org/BodyMeasurementWeight" ->
            pure BodyMeasurementTypeEnumerationBodyMeasurementWeight
          t ->
            fail
              ("Failed to parse BodyMeasurementTypeEnumeration: " <> show t)
      )

instance ToJSON BodyMeasurementTypeEnumeration where
  toJSON =
    (toJSON :: Text -> Value)
      . ( \case
            BodyMeasurementTypeEnumerationBodyMeasurementArm ->
              "https://schema.org/BodyMeasurementArm"
            BodyMeasurementTypeEnumerationBodyMeasurementBust ->
              "https://schema.org/BodyMeasurementBust"
            BodyMeasurementTypeEnumerationBodyMeasurementChest ->
              "https://schema.org/BodyMeasurementChest"
            BodyMeasurementTypeEnumerationBodyMeasurementFoot ->
              "https://schema.org/BodyMeasurementFoot"
            BodyMeasurementTypeEnumerationBodyMeasurementHand ->
              "https://schema.org/BodyMeasurementHand"
            BodyMeasurementTypeEnumerationBodyMeasurementHead ->
              "https://schema.org/BodyMeasurementHead"
            BodyMeasurementTypeEnumerationBodyMeasurementHeight ->
              "https://schema.org/BodyMeasurementHeight"
            BodyMeasurementTypeEnumerationBodyMeasurementHips ->
              "https://schema.org/BodyMeasurementHips"
            BodyMeasurementTypeEnumerationBodyMeasurementInsideLeg ->
              "https://schema.org/BodyMeasurementInsideLeg"
            BodyMeasurementTypeEnumerationBodyMeasurementNeck ->
              "https://schema.org/BodyMeasurementNeck"
            BodyMeasurementTypeEnumerationBodyMeasurementUnderbust ->
              "https://schema.org/BodyMeasurementUnderbust"
            BodyMeasurementTypeEnumerationBodyMeasurementWaist ->
              "https://schema.org/BodyMeasurementWaist"
            BodyMeasurementTypeEnumerationBodyMeasurementWeight ->
              "https://schema.org/BodyMeasurementWeight"
        )

data BodyOfWater

classBodyOfWater :: Class BodyOfWater '[Landform, Place, Thing]
classBodyOfWater = Class "BodyOfWater"

data Bone

classBone ::
  Class Bone '[AnatomicalStructure, MedicalEntity, Thing]
classBone = Class "Bone"

data Book

classBook :: Class Book '[CreativeWork, Thing]
classBook = Class "Book"

data BookFormatType
  = BookFormatTypeAudiobookFormat
  | BookFormatTypeEBook
  | BookFormatTypeGraphicNovel
  | BookFormatTypeHardcover
  | BookFormatTypePaperback
  deriving (Show, Eq, Ord, Generic)

instance FromJSON BookFormatType where
  parseJSON =
    withText
      "BookFormatType"
      ( \case
          "https://schema.org/AudiobookFormat" ->
            pure BookFormatTypeAudiobookFormat
          "https://schema.org/EBook" -> pure BookFormatTypeEBook
          "https://schema.org/GraphicNovel" ->
            pure BookFormatTypeGraphicNovel
          "https://schema.org/Hardcover" -> pure BookFormatTypeHardcover
          "https://schema.org/Paperback" -> pure BookFormatTypePaperback
          t -> fail ("Failed to parse BookFormatType: " <> show t)
      )

instance ToJSON BookFormatType where
  toJSON =
    (toJSON :: Text -> Value)
      . ( \case
            BookFormatTypeAudiobookFormat ->
              "https://schema.org/AudiobookFormat"
            BookFormatTypeEBook -> "https://schema.org/EBook"
            BookFormatTypeGraphicNovel -> "https://schema.org/GraphicNovel"
            BookFormatTypeHardcover -> "https://schema.org/Hardcover"
            BookFormatTypePaperback -> "https://schema.org/Paperback"
        )

data BookSeries

classBookSeries ::
  Class
    BookSeries
    '[ CreativeWorkSeries,
       Series,
       CreativeWork,
       Intangible,
       Thing,
       Thing
     ]
classBookSeries = Class "BookSeries"

data BookStore

classBookStore ::
  Class
    BookStore
    '[ Store,
       LocalBusiness,
       Place,
       Organization,
       Thing,
       Thing
     ]
classBookStore = Class "BookStore"

data BookmarkAction

classBookmarkAction ::
  Class BookmarkAction '[OrganizeAction, Action, Thing]
classBookmarkAction = Class "BookmarkAction"

data BorrowAction

classBorrowAction ::
  Class BorrowAction '[TransferAction, Action, Thing]
classBorrowAction = Class "BorrowAction"

data BowlingAlley

classBowlingAlley ::
  Class
    BowlingAlley
    '[ SportsActivityLocation,
       LocalBusiness,
       Place,
       Organization,
       Thing,
       Thing
     ]
classBowlingAlley = Class "BowlingAlley"

data BrainStructure

classBrainStructure ::
  Class BrainStructure '[AnatomicalStructure, MedicalEntity, Thing]
classBrainStructure = Class "BrainStructure"

data Brand

classBrand :: Class Brand '[Intangible, Thing]
classBrand = Class "Brand"

data BreadcrumbList

classBreadcrumbList ::
  Class BreadcrumbList '[ItemList, Intangible, Thing]
classBreadcrumbList = Class "BreadcrumbList"

data Brewery

classBrewery ::
  Class
    Brewery
    '[ FoodEstablishment,
       LocalBusiness,
       Place,
       Organization,
       Thing,
       Thing
     ]
classBrewery = Class "Brewery"

data Bridge

classBridge :: Class Bridge '[CivicStructure, Place, Thing]
classBridge = Class "Bridge"

data BroadcastChannel

classBroadcastChannel ::
  Class BroadcastChannel '[Intangible, Thing]
classBroadcastChannel = Class "BroadcastChannel"

data BroadcastEvent

classBroadcastEvent ::
  Class BroadcastEvent '[PublicationEvent, Event, Thing]
classBroadcastEvent = Class "BroadcastEvent"

data BroadcastFrequencySpecification

classBroadcastFrequencySpecification ::
  Class BroadcastFrequencySpecification '[Intangible, Thing]
classBroadcastFrequencySpecification =
  Class "BroadcastFrequencySpecification"

data BroadcastService

classBroadcastService ::
  Class BroadcastService '[Service, Intangible, Thing]
classBroadcastService = Class "BroadcastService"

data BrokerageAccount

classBrokerageAccount ::
  Class
    BrokerageAccount
    '[ InvestmentOrDeposit,
       FinancialProduct,
       Service,
       Intangible,
       Thing
     ]
classBrokerageAccount = Class "BrokerageAccount"

data BuddhistTemple

classBuddhistTemple ::
  Class
    BuddhistTemple
    '[ PlaceOfWorship,
       CivicStructure,
       Place,
       Thing
     ]
classBuddhistTemple = Class "BuddhistTemple"

data BusOrCoach

classBusOrCoach :: Class BusOrCoach '[Vehicle, Product, Thing]
classBusOrCoach = Class "BusOrCoach"

data BusReservation

classBusReservation ::
  Class BusReservation '[Reservation, Intangible, Thing]
classBusReservation = Class "BusReservation"

data BusStation

classBusStation :: Class BusStation '[CivicStructure, Place, Thing]
classBusStation = Class "BusStation"

data BusStop

classBusStop :: Class BusStop '[CivicStructure, Place, Thing]
classBusStop = Class "BusStop"

data BusTrip

classBusTrip :: Class BusTrip '[Trip, Intangible, Thing]
classBusTrip = Class "BusTrip"

data BusinessAudience

classBusinessAudience ::
  Class BusinessAudience '[Audience, Intangible, Thing]
classBusinessAudience = Class "BusinessAudience"

type BusinessEntityType = Text

data BusinessEvent

classBusinessEvent :: Class BusinessEvent '[Event, Thing]
classBusinessEvent = Class "BusinessEvent"

type BusinessFunction = Text

data BuyAction

classBuyAction :: Class BuyAction '[TradeAction, Action, Thing]
classBuyAction = Class "BuyAction"

data CDCPMDRecord

classCDCPMDRecord ::
  Class CDCPMDRecord '[StructuredValue, Intangible, Thing]
classCDCPMDRecord = Class "CDCPMDRecord"

data CableOrSatelliteService

classCableOrSatelliteService ::
  Class CableOrSatelliteService '[Service, Intangible, Thing]
classCableOrSatelliteService = Class "CableOrSatelliteService"

data CafeOrCoffeeShop

classCafeOrCoffeeShop ::
  Class
    CafeOrCoffeeShop
    '[ FoodEstablishment,
       LocalBusiness,
       Place,
       Organization,
       Thing,
       Thing
     ]
classCafeOrCoffeeShop = Class "CafeOrCoffeeShop"

data Campground

classCampground ::
  Class
    Campground
    '[ LodgingBusiness,
       CivicStructure,
       LocalBusiness,
       Place,
       Organization,
       Thing,
       Thing,
       Place,
       Thing
     ]
classCampground = Class "Campground"

data CampingPitch

classCampingPitch ::
  Class CampingPitch '[Accommodation, Place, Thing]
classCampingPitch = Class "CampingPitch"

data Canal

classCanal :: Class Canal '[BodyOfWater, Landform, Place, Thing]
classCanal = Class "Canal"

data CancelAction

classCancelAction ::
  Class CancelAction '[PlanAction, OrganizeAction, Action, Thing]
classCancelAction = Class "CancelAction"

data Car

classCar :: Class Car '[Vehicle, Product, Thing]
classCar = Class "Car"

data CarUsageType
  = CarUsageTypeDrivingSchoolVehicleUsage
  | CarUsageTypeRentalVehicleUsage
  | CarUsageTypeTaxiVehicleUsage
  deriving (Show, Eq, Ord, Generic)

instance FromJSON CarUsageType where
  parseJSON =
    withText
      "CarUsageType"
      ( \case
          "https://schema.org/DrivingSchoolVehicleUsage" ->
            pure CarUsageTypeDrivingSchoolVehicleUsage
          "https://schema.org/RentalVehicleUsage" ->
            pure CarUsageTypeRentalVehicleUsage
          "https://schema.org/TaxiVehicleUsage" ->
            pure CarUsageTypeTaxiVehicleUsage
          t -> fail ("Failed to parse CarUsageType: " <> show t)
      )

instance ToJSON CarUsageType where
  toJSON =
    (toJSON :: Text -> Value)
      . ( \case
            CarUsageTypeDrivingSchoolVehicleUsage ->
              "https://schema.org/DrivingSchoolVehicleUsage"
            CarUsageTypeRentalVehicleUsage ->
              "https://schema.org/RentalVehicleUsage"
            CarUsageTypeTaxiVehicleUsage ->
              "https://schema.org/TaxiVehicleUsage"
        )

data Casino

classCasino ::
  Class
    Casino
    '[ EntertainmentBusiness,
       LocalBusiness,
       Place,
       Organization,
       Thing,
       Thing
     ]
classCasino = Class "Casino"

data CategoryCode

classCategoryCode ::
  Class CategoryCode '[DefinedTerm, Intangible, Thing]
classCategoryCode = Class "CategoryCode"

data CategoryCodeSet

classCategoryCodeSet ::
  Class CategoryCodeSet '[DefinedTermSet, CreativeWork, Thing]
classCategoryCodeSet = Class "CategoryCodeSet"

data CatholicChurch

classCatholicChurch ::
  Class
    CatholicChurch
    '[ Church,
       PlaceOfWorship,
       CivicStructure,
       Place,
       Thing
     ]
classCatholicChurch = Class "CatholicChurch"

data Cemetery

classCemetery :: Class Cemetery '[CivicStructure, Place, Thing]
classCemetery = Class "Cemetery"

data Chapter

classChapter :: Class Chapter '[CreativeWork, Thing]
classChapter = Class "Chapter"

data CheckAction

classCheckAction :: Class CheckAction '[FindAction, Action, Thing]
classCheckAction = Class "CheckAction"

data CheckInAction

classCheckInAction ::
  Class
    CheckInAction
    '[ CommunicateAction,
       InteractAction,
       Action,
       Thing
     ]
classCheckInAction = Class "CheckInAction"

data CheckOutAction

classCheckOutAction ::
  Class
    CheckOutAction
    '[ CommunicateAction,
       InteractAction,
       Action,
       Thing
     ]
classCheckOutAction = Class "CheckOutAction"

data CheckoutPage

classCheckoutPage ::
  Class CheckoutPage '[WebPage, CreativeWork, Thing]
classCheckoutPage = Class "CheckoutPage"

data ChemicalSubstance

classChemicalSubstance ::
  Class ChemicalSubstance '[BioChemEntity, Thing]
classChemicalSubstance = Class "ChemicalSubstance"

data ChildCare

classChildCare ::
  Class ChildCare '[LocalBusiness, Place, Organization, Thing, Thing]
classChildCare = Class "ChildCare"

data ChildrensEvent

classChildrensEvent :: Class ChildrensEvent '[Event, Thing]
classChildrensEvent = Class "ChildrensEvent"

data ChooseAction

classChooseAction ::
  Class ChooseAction '[AssessAction, Action, Thing]
classChooseAction = Class "ChooseAction"

data Church

classChurch ::
  Class Church '[PlaceOfWorship, CivicStructure, Place, Thing]
classChurch = Class "Church"

data City

classCity :: Class City '[AdministrativeArea, Place, Thing]
classCity = Class "City"

data CityHall

classCityHall ::
  Class CityHall '[GovernmentBuilding, CivicStructure, Place, Thing]
classCityHall = Class "CityHall"

data CivicStructure

classCivicStructure :: Class CivicStructure '[Place, Thing]
classCivicStructure = Class "CivicStructure"

data Claim

classClaim :: Class Claim '[CreativeWork, Thing]
classClaim = Class "Claim"

data ClaimReview

classClaimReview ::
  Class ClaimReview '[Review, CreativeWork, Thing]
classClaimReview = Class "ClaimReview"

data Class_

classClass_ :: Class Class_ '[Intangible, Thing]
classClass_ = Class "Class"

data Clip

classClip :: Class Clip '[CreativeWork, Thing]
classClip = Class "Clip"

data ClothingStore

classClothingStore ::
  Class
    ClothingStore
    '[ Store,
       LocalBusiness,
       Place,
       Organization,
       Thing,
       Thing
     ]
classClothingStore = Class "ClothingStore"

data Code

classCode :: Class Code '[CreativeWork, Thing]
classCode = Class "Code"

data Collection

classCollection :: Class Collection '[CreativeWork, Thing]
classCollection = Class "Collection"

data CollectionPage

classCollectionPage ::
  Class CollectionPage '[WebPage, CreativeWork, Thing]
classCollectionPage = Class "CollectionPage"

data CollegeOrUniversity

classCollegeOrUniversity ::
  Class
    CollegeOrUniversity
    '[ EducationalOrganization,
       CivicStructure,
       Organization,
       Place,
       Thing,
       Thing
     ]
classCollegeOrUniversity = Class "CollegeOrUniversity"

data ComedyClub

classComedyClub ::
  Class
    ComedyClub
    '[ EntertainmentBusiness,
       LocalBusiness,
       Place,
       Organization,
       Thing,
       Thing
     ]
classComedyClub = Class "ComedyClub"

data ComedyEvent

classComedyEvent :: Class ComedyEvent '[Event, Thing]
classComedyEvent = Class "ComedyEvent"

data ComicCoverArt

classComicCoverArt ::
  Class
    ComicCoverArt
    '[ ComicStory,
       CoverArt,
       CreativeWork,
       Thing,
       VisualArtwork,
       CreativeWork,
       Thing
     ]
classComicCoverArt = Class "ComicCoverArt"

data ComicIssue

classComicIssue ::
  Class ComicIssue '[PublicationIssue, CreativeWork, Thing]
classComicIssue = Class "ComicIssue"

data ComicSeries

classComicSeries ::
  Class
    ComicSeries
    '[ Periodical,
       CreativeWorkSeries,
       Series,
       CreativeWork,
       Intangible,
       Thing,
       Thing
     ]
classComicSeries = Class "ComicSeries"

data ComicStory

classComicStory :: Class ComicStory '[CreativeWork, Thing]
classComicStory = Class "ComicStory"

data Comment

classComment :: Class Comment '[CreativeWork, Thing]
classComment = Class "Comment"

data CommentAction

classCommentAction ::
  Class
    CommentAction
    '[ CommunicateAction,
       InteractAction,
       Action,
       Thing
     ]
classCommentAction = Class "CommentAction"

data CommunicateAction

classCommunicateAction ::
  Class CommunicateAction '[InteractAction, Action, Thing]
classCommunicateAction = Class "CommunicateAction"

data CompleteDataFeed

classCompleteDataFeed ::
  Class CompleteDataFeed '[DataFeed, Dataset, CreativeWork, Thing]
classCompleteDataFeed = Class "CompleteDataFeed"

data CompoundPriceSpecification

classCompoundPriceSpecification ::
  Class
    CompoundPriceSpecification
    '[ PriceSpecification,
       StructuredValue,
       Intangible,
       Thing
     ]
classCompoundPriceSpecification =
  Class "CompoundPriceSpecification"

data ComputerLanguage

classComputerLanguage ::
  Class ComputerLanguage '[Intangible, Thing]
classComputerLanguage = Class "ComputerLanguage"

data ComputerStore

classComputerStore ::
  Class
    ComputerStore
    '[ Store,
       LocalBusiness,
       Place,
       Organization,
       Thing,
       Thing
     ]
classComputerStore = Class "ComputerStore"

data ConfirmAction

classConfirmAction ::
  Class
    ConfirmAction
    '[ InformAction,
       CommunicateAction,
       InteractAction,
       Action,
       Thing
     ]
classConfirmAction = Class "ConfirmAction"

data Consortium

classConsortium :: Class Consortium '[Organization, Thing]
classConsortium = Class "Consortium"

data ConsumeAction

classConsumeAction :: Class ConsumeAction '[Action, Thing]
classConsumeAction = Class "ConsumeAction"

data ContactPage

classContactPage ::
  Class ContactPage '[WebPage, CreativeWork, Thing]
classContactPage = Class "ContactPage"

data ContactPoint

classContactPoint ::
  Class ContactPoint '[StructuredValue, Intangible, Thing]
classContactPoint = Class "ContactPoint"

data ContactPointOption
  = ContactPointOptionHearingImpairedSupported
  | ContactPointOptionTollFree
  deriving (Show, Eq, Ord, Generic)

instance FromJSON ContactPointOption where
  parseJSON =
    withText
      "ContactPointOption"
      ( \case
          "https://schema.org/HearingImpairedSupported" ->
            pure ContactPointOptionHearingImpairedSupported
          "https://schema.org/TollFree" -> pure ContactPointOptionTollFree
          t -> fail ("Failed to parse ContactPointOption: " <> show t)
      )

instance ToJSON ContactPointOption where
  toJSON =
    (toJSON :: Text -> Value)
      . ( \case
            ContactPointOptionHearingImpairedSupported ->
              "https://schema.org/HearingImpairedSupported"
            ContactPointOptionTollFree -> "https://schema.org/TollFree"
        )

data Continent

classContinent :: Class Continent '[Landform, Place, Thing]
classContinent = Class "Continent"

data ControlAction

classControlAction :: Class ControlAction '[Action, Thing]
classControlAction = Class "ControlAction"

data ConvenienceStore

classConvenienceStore ::
  Class
    ConvenienceStore
    '[ Store,
       LocalBusiness,
       Place,
       Organization,
       Thing,
       Thing
     ]
classConvenienceStore = Class "ConvenienceStore"

data Conversation

classConversation :: Class Conversation '[CreativeWork, Thing]
classConversation = Class "Conversation"

data CookAction

classCookAction :: Class CookAction '[CreateAction, Action, Thing]
classCookAction = Class "CookAction"

data Corporation

classCorporation :: Class Corporation '[Organization, Thing]
classCorporation = Class "Corporation"

data CorrectionComment

classCorrectionComment ::
  Class CorrectionComment '[Comment, CreativeWork, Thing]
classCorrectionComment = Class "CorrectionComment"

data Country

classCountry :: Class Country '[AdministrativeArea, Place, Thing]
classCountry = Class "Country"

data Course

classCourse ::
  Class
    Course
    '[ LearningResource,
       CreativeWork,
       CreativeWork,
       Thing,
       Thing
     ]
classCourse = Class "Course"

data CourseInstance

classCourseInstance :: Class CourseInstance '[Event, Thing]
classCourseInstance = Class "CourseInstance"

data Courthouse

classCourthouse ::
  Class
    Courthouse
    '[ GovernmentBuilding,
       CivicStructure,
       Place,
       Thing
     ]
classCourthouse = Class "Courthouse"

data CoverArt

classCoverArt ::
  Class CoverArt '[VisualArtwork, CreativeWork, Thing]
classCoverArt = Class "CoverArt"

data CovidTestingFacility

classCovidTestingFacility ::
  Class
    CovidTestingFacility
    '[ MedicalClinic,
       MedicalBusiness,
       MedicalOrganization,
       LocalBusiness,
       Place,
       Organization,
       Thing,
       Thing,
       Organization,
       Thing
     ]
classCovidTestingFacility = Class "CovidTestingFacility"

data CreateAction

classCreateAction :: Class CreateAction '[Action, Thing]
classCreateAction = Class "CreateAction"

data CreativeWork

classCreativeWork :: Class CreativeWork '[Thing]
classCreativeWork = Class "CreativeWork"

data CreativeWorkSeason

classCreativeWorkSeason ::
  Class CreativeWorkSeason '[CreativeWork, Thing]
classCreativeWorkSeason = Class "CreativeWorkSeason"

data CreativeWorkSeries

classCreativeWorkSeries ::
  Class
    CreativeWorkSeries
    '[ Series,
       CreativeWork,
       Intangible,
       Thing,
       Thing
     ]
classCreativeWorkSeries = Class "CreativeWorkSeries"

type CreditCard = Text

data Crematorium

classCrematorium ::
  Class Crematorium '[CivicStructure, Place, Thing]
classCrematorium = Class "Crematorium"

data CriticReview

classCriticReview ::
  Class CriticReview '[Review, CreativeWork, Thing]
classCriticReview = Class "CriticReview"

type CssSelectorType = Text

data CurrencyConversionService

classCurrencyConversionService ::
  Class
    CurrencyConversionService
    '[ FinancialProduct,
       Service,
       Intangible,
       Thing
     ]
classCurrencyConversionService = Class "CurrencyConversionService"

data DDxElement

classDDxElement ::
  Class DDxElement '[MedicalIntangible, MedicalEntity, Thing]
classDDxElement = Class "DDxElement"

data DanceEvent

classDanceEvent :: Class DanceEvent '[Event, Thing]
classDanceEvent = Class "DanceEvent"

data DanceGroup

classDanceGroup ::
  Class DanceGroup '[PerformingGroup, Organization, Thing]
classDanceGroup = Class "DanceGroup"

data DataCatalog

classDataCatalog :: Class DataCatalog '[CreativeWork, Thing]
classDataCatalog = Class "DataCatalog"

data DataDownload

classDataDownload ::
  Class DataDownload '[MediaObject, CreativeWork, Thing]
classDataDownload = Class "DataDownload"

data DataFeed

classDataFeed :: Class DataFeed '[Dataset, CreativeWork, Thing]
classDataFeed = Class "DataFeed"

data DataFeedItem

classDataFeedItem :: Class DataFeedItem '[Intangible, Thing]
classDataFeedItem = Class "DataFeedItem"

data DataType

classDataType :: Class DataType '[]
classDataType = Class "DataType"

data Dataset

classDataset :: Class Dataset '[CreativeWork, Thing]
classDataset = Class "Dataset"

data DatedMoneySpecification

classDatedMoneySpecification ::
  Class DatedMoneySpecification '[StructuredValue, Intangible, Thing]
classDatedMoneySpecification = Class "DatedMoneySpecification"

data DayOfWeek
  = DayOfWeekFriday
  | DayOfWeekMonday
  | DayOfWeekPublicHolidays
  | DayOfWeekSaturday
  | DayOfWeekSunday
  | DayOfWeekThursday
  | DayOfWeekTuesday
  | DayOfWeekWednesday
  deriving (Show, Eq, Ord, Generic)

instance FromJSON DayOfWeek where
  parseJSON =
    withText
      "DayOfWeek"
      ( \case
          "https://schema.org/Friday" -> pure DayOfWeekFriday
          "https://schema.org/Monday" -> pure DayOfWeekMonday
          "https://schema.org/PublicHolidays" -> pure DayOfWeekPublicHolidays
          "https://schema.org/Saturday" -> pure DayOfWeekSaturday
          "https://schema.org/Sunday" -> pure DayOfWeekSunday
          "https://schema.org/Thursday" -> pure DayOfWeekThursday
          "https://schema.org/Tuesday" -> pure DayOfWeekTuesday
          "https://schema.org/Wednesday" -> pure DayOfWeekWednesday
          t -> fail ("Failed to parse DayOfWeek: " <> show t)
      )

instance ToJSON DayOfWeek where
  toJSON =
    (toJSON :: Text -> Value)
      . ( \case
            DayOfWeekFriday -> "https://schema.org/Friday"
            DayOfWeekMonday -> "https://schema.org/Monday"
            DayOfWeekPublicHolidays -> "https://schema.org/PublicHolidays"
            DayOfWeekSaturday -> "https://schema.org/Saturday"
            DayOfWeekSunday -> "https://schema.org/Sunday"
            DayOfWeekThursday -> "https://schema.org/Thursday"
            DayOfWeekTuesday -> "https://schema.org/Tuesday"
            DayOfWeekWednesday -> "https://schema.org/Wednesday"
        )

data DaySpa

classDaySpa ::
  Class
    DaySpa
    '[ HealthAndBeautyBusiness,
       LocalBusiness,
       Place,
       Organization,
       Thing,
       Thing
     ]
classDaySpa = Class "DaySpa"

data DeactivateAction

classDeactivateAction ::
  Class DeactivateAction '[ControlAction, Action, Thing]
classDeactivateAction = Class "DeactivateAction"

data DefenceEstablishment

classDefenceEstablishment ::
  Class
    DefenceEstablishment
    '[ GovernmentBuilding,
       CivicStructure,
       Place,
       Thing
     ]
classDefenceEstablishment = Class "DefenceEstablishment"

data DefinedRegion

classDefinedRegion ::
  Class DefinedRegion '[StructuredValue, Intangible, Thing]
classDefinedRegion = Class "DefinedRegion"

data DefinedTerm

classDefinedTerm :: Class DefinedTerm '[Intangible, Thing]
classDefinedTerm = Class "DefinedTerm"

data DefinedTermSet

classDefinedTermSet :: Class DefinedTermSet '[CreativeWork, Thing]
classDefinedTermSet = Class "DefinedTermSet"

data DeleteAction

classDeleteAction ::
  Class DeleteAction '[UpdateAction, Action, Thing]
classDeleteAction = Class "DeleteAction"

data DeliveryChargeSpecification

classDeliveryChargeSpecification ::
  Class
    DeliveryChargeSpecification
    '[ PriceSpecification,
       StructuredValue,
       Intangible,
       Thing
     ]
classDeliveryChargeSpecification =
  Class "DeliveryChargeSpecification"

data DeliveryEvent

classDeliveryEvent :: Class DeliveryEvent '[Event, Thing]
classDeliveryEvent = Class "DeliveryEvent"

data DeliveryMethod
  = DeliveryMethodLockerDelivery
  | DeliveryMethodOnSitePickup
  | DeliveryMethodParcelService
  deriving (Show, Eq, Ord, Generic)

instance FromJSON DeliveryMethod where
  parseJSON =
    withText
      "DeliveryMethod"
      ( \case
          "https://schema.org/LockerDelivery" ->
            pure DeliveryMethodLockerDelivery
          "https://schema.org/OnSitePickup" ->
            pure DeliveryMethodOnSitePickup
          "https://schema.org/ParcelService" ->
            pure DeliveryMethodParcelService
          t -> fail ("Failed to parse DeliveryMethod: " <> show t)
      )

instance ToJSON DeliveryMethod where
  toJSON =
    (toJSON :: Text -> Value)
      . ( \case
            DeliveryMethodLockerDelivery -> "https://schema.org/LockerDelivery"
            DeliveryMethodOnSitePickup -> "https://schema.org/OnSitePickup"
            DeliveryMethodParcelService -> "https://schema.org/ParcelService"
        )

data DeliveryTimeSettings

classDeliveryTimeSettings ::
  Class DeliveryTimeSettings '[StructuredValue, Intangible, Thing]
classDeliveryTimeSettings = Class "DeliveryTimeSettings"

data Demand

classDemand :: Class Demand '[Intangible, Thing]
classDemand = Class "Demand"

data Dentist

classDentist ::
  Class
    Dentist
    '[ LocalBusiness,
       MedicalOrganization,
       MedicalBusiness,
       Place,
       Organization,
       Thing,
       Thing,
       Organization,
       Thing,
       LocalBusiness,
       Place,
       Organization,
       Thing,
       Thing
     ]
classDentist = Class "Dentist"

data DepartAction

classDepartAction ::
  Class DepartAction '[MoveAction, Action, Thing]
classDepartAction = Class "DepartAction"

data DepartmentStore

classDepartmentStore ::
  Class
    DepartmentStore
    '[ Store,
       LocalBusiness,
       Place,
       Organization,
       Thing,
       Thing
     ]
classDepartmentStore = Class "DepartmentStore"

data DepositAccount

classDepositAccount ::
  Class
    DepositAccount
    '[ BankAccount,
       InvestmentOrDeposit,
       FinancialProduct,
       Service,
       Intangible,
       Thing,
       FinancialProduct,
       Service,
       Intangible,
       Thing
     ]
classDepositAccount = Class "DepositAccount"

data DiagnosticLab

classDiagnosticLab ::
  Class DiagnosticLab '[MedicalOrganization, Organization, Thing]
classDiagnosticLab = Class "DiagnosticLab"

data DiagnosticProcedure

classDiagnosticProcedure ::
  Class DiagnosticProcedure '[MedicalProcedure, MedicalEntity, Thing]
classDiagnosticProcedure = Class "DiagnosticProcedure"

data Diet

classDiet ::
  Class
    Diet
    '[ CreativeWork,
       LifestyleModification,
       Thing,
       MedicalEntity,
       Thing
     ]
classDiet = Class "Diet"

data DietarySupplement

classDietarySupplement ::
  Class
    DietarySupplement
    '[ Product,
       Substance,
       Thing,
       MedicalEntity,
       Thing
     ]
classDietarySupplement = Class "DietarySupplement"

data DigitalDocument

classDigitalDocument ::
  Class DigitalDocument '[CreativeWork, Thing]
classDigitalDocument = Class "DigitalDocument"

data DigitalDocumentPermission

classDigitalDocumentPermission ::
  Class DigitalDocumentPermission '[Intangible, Thing]
classDigitalDocumentPermission = Class "DigitalDocumentPermission"

data DigitalDocumentPermissionType
  = DigitalDocumentPermissionTypeCommentPermission
  | DigitalDocumentPermissionTypeReadPermission
  | DigitalDocumentPermissionTypeWritePermission
  deriving (Show, Eq, Ord, Generic)

instance FromJSON DigitalDocumentPermissionType where
  parseJSON =
    withText
      "DigitalDocumentPermissionType"
      ( \case
          "https://schema.org/CommentPermission" ->
            pure DigitalDocumentPermissionTypeCommentPermission
          "https://schema.org/ReadPermission" ->
            pure DigitalDocumentPermissionTypeReadPermission
          "https://schema.org/WritePermission" ->
            pure DigitalDocumentPermissionTypeWritePermission
          t ->
            fail
              ("Failed to parse DigitalDocumentPermissionType: " <> show t)
      )

instance ToJSON DigitalDocumentPermissionType where
  toJSON =
    (toJSON :: Text -> Value)
      . ( \case
            DigitalDocumentPermissionTypeCommentPermission ->
              "https://schema.org/CommentPermission"
            DigitalDocumentPermissionTypeReadPermission ->
              "https://schema.org/ReadPermission"
            DigitalDocumentPermissionTypeWritePermission ->
              "https://schema.org/WritePermission"
        )

data DigitalPlatformEnumeration
  = DigitalPlatformEnumerationAndroidPlatform
  | DigitalPlatformEnumerationDesktopWebPlatform
  | DigitalPlatformEnumerationGenericWebPlatform
  | DigitalPlatformEnumerationIOSPlatform
  | DigitalPlatformEnumerationMobileWebPlatform
  deriving (Show, Eq, Ord, Generic)

instance FromJSON DigitalPlatformEnumeration where
  parseJSON =
    withText
      "DigitalPlatformEnumeration"
      ( \case
          "https://schema.org/AndroidPlatform" ->
            pure DigitalPlatformEnumerationAndroidPlatform
          "https://schema.org/DesktopWebPlatform" ->
            pure DigitalPlatformEnumerationDesktopWebPlatform
          "https://schema.org/GenericWebPlatform" ->
            pure DigitalPlatformEnumerationGenericWebPlatform
          "https://schema.org/IOSPlatform" ->
            pure DigitalPlatformEnumerationIOSPlatform
          "https://schema.org/MobileWebPlatform" ->
            pure DigitalPlatformEnumerationMobileWebPlatform
          t ->
            fail
              ("Failed to parse DigitalPlatformEnumeration: " <> show t)
      )

instance ToJSON DigitalPlatformEnumeration where
  toJSON =
    (toJSON :: Text -> Value)
      . ( \case
            DigitalPlatformEnumerationAndroidPlatform ->
              "https://schema.org/AndroidPlatform"
            DigitalPlatformEnumerationDesktopWebPlatform ->
              "https://schema.org/DesktopWebPlatform"
            DigitalPlatformEnumerationGenericWebPlatform ->
              "https://schema.org/GenericWebPlatform"
            DigitalPlatformEnumerationIOSPlatform ->
              "https://schema.org/IOSPlatform"
            DigitalPlatformEnumerationMobileWebPlatform ->
              "https://schema.org/MobileWebPlatform"
        )

data DisagreeAction

classDisagreeAction ::
  Class DisagreeAction '[ReactAction, AssessAction, Action, Thing]
classDisagreeAction = Class "DisagreeAction"

data DiscoverAction

classDiscoverAction ::
  Class DiscoverAction '[FindAction, Action, Thing]
classDiscoverAction = Class "DiscoverAction"

data DiscussionForumPosting

classDiscussionForumPosting ::
  Class
    DiscussionForumPosting
    '[ SocialMediaPosting,
       Article,
       CreativeWork,
       Thing
     ]
classDiscussionForumPosting = Class "DiscussionForumPosting"

data DislikeAction

classDislikeAction ::
  Class DislikeAction '[ReactAction, AssessAction, Action, Thing]
classDislikeAction = Class "DislikeAction"

data Distance

classDistance :: Class Distance '[Quantity, Intangible, Thing]
classDistance = Class "Distance"

data Distillery

classDistillery ::
  Class
    Distillery
    '[ FoodEstablishment,
       LocalBusiness,
       Place,
       Organization,
       Thing,
       Thing
     ]
classDistillery = Class "Distillery"

data DonateAction

classDonateAction ::
  Class DonateAction '[TradeAction, Action, Thing]
classDonateAction = Class "DonateAction"

data DoseSchedule

classDoseSchedule ::
  Class DoseSchedule '[MedicalIntangible, MedicalEntity, Thing]
classDoseSchedule = Class "DoseSchedule"

data DownloadAction

classDownloadAction ::
  Class DownloadAction '[TransferAction, Action, Thing]
classDownloadAction = Class "DownloadAction"

data DrawAction

classDrawAction :: Class DrawAction '[CreateAction, Action, Thing]
classDrawAction = Class "DrawAction"

data Drawing

classDrawing :: Class Drawing '[CreativeWork, Thing]
classDrawing = Class "Drawing"

data DrinkAction

classDrinkAction ::
  Class DrinkAction '[ConsumeAction, Action, Thing]
classDrinkAction = Class "DrinkAction"

data DriveWheelConfigurationValue
  = DriveWheelConfigurationValueAllWheelDriveConfiguration
  | DriveWheelConfigurationValueFourWheelDriveConfiguration
  | DriveWheelConfigurationValueFrontWheelDriveConfiguration
  | DriveWheelConfigurationValueRearWheelDriveConfiguration
  deriving (Show, Eq, Ord, Generic)

instance FromJSON DriveWheelConfigurationValue where
  parseJSON =
    withText
      "DriveWheelConfigurationValue"
      ( \case
          "https://schema.org/AllWheelDriveConfiguration" ->
            pure DriveWheelConfigurationValueAllWheelDriveConfiguration
          "https://schema.org/FourWheelDriveConfiguration" ->
            pure DriveWheelConfigurationValueFourWheelDriveConfiguration
          "https://schema.org/FrontWheelDriveConfiguration" ->
            pure DriveWheelConfigurationValueFrontWheelDriveConfiguration
          "https://schema.org/RearWheelDriveConfiguration" ->
            pure DriveWheelConfigurationValueRearWheelDriveConfiguration
          t ->
            fail
              ("Failed to parse DriveWheelConfigurationValue: " <> show t)
      )

instance ToJSON DriveWheelConfigurationValue where
  toJSON =
    (toJSON :: Text -> Value)
      . ( \case
            DriveWheelConfigurationValueAllWheelDriveConfiguration ->
              "https://schema.org/AllWheelDriveConfiguration"
            DriveWheelConfigurationValueFourWheelDriveConfiguration ->
              "https://schema.org/FourWheelDriveConfiguration"
            DriveWheelConfigurationValueFrontWheelDriveConfiguration ->
              "https://schema.org/FrontWheelDriveConfiguration"
            DriveWheelConfigurationValueRearWheelDriveConfiguration ->
              "https://schema.org/RearWheelDriveConfiguration"
        )

data Drug

classDrug ::
  Class Drug '[Product, Substance, Thing, MedicalEntity, Thing]
classDrug = Class "Drug"

data DrugClass

classDrugClass :: Class DrugClass '[MedicalEntity, Thing]
classDrugClass = Class "DrugClass"

data DrugCost

classDrugCost :: Class DrugCost '[MedicalEntity, Thing]
classDrugCost = Class "DrugCost"

data DrugCostCategory
  = DrugCostCategoryReimbursementCap
  | DrugCostCategoryRetail
  | DrugCostCategoryWholesale
  deriving (Show, Eq, Ord, Generic)

instance FromJSON DrugCostCategory where
  parseJSON =
    withText
      "DrugCostCategory"
      ( \case
          "https://schema.org/ReimbursementCap" ->
            pure DrugCostCategoryReimbursementCap
          "https://schema.org/Retail" -> pure DrugCostCategoryRetail
          "https://schema.org/Wholesale" -> pure DrugCostCategoryWholesale
          t -> fail ("Failed to parse DrugCostCategory: " <> show t)
      )

instance ToJSON DrugCostCategory where
  toJSON =
    (toJSON :: Text -> Value)
      . ( \case
            DrugCostCategoryReimbursementCap ->
              "https://schema.org/ReimbursementCap"
            DrugCostCategoryRetail -> "https://schema.org/Retail"
            DrugCostCategoryWholesale -> "https://schema.org/Wholesale"
        )

data DrugLegalStatus

classDrugLegalStatus ::
  Class DrugLegalStatus '[MedicalIntangible, MedicalEntity, Thing]
classDrugLegalStatus = Class "DrugLegalStatus"

data DrugPregnancyCategory
  = DrugPregnancyCategoryFDAcategoryA
  | DrugPregnancyCategoryFDAcategoryB
  | DrugPregnancyCategoryFDAcategoryC
  | DrugPregnancyCategoryFDAcategoryD
  | DrugPregnancyCategoryFDAcategoryX
  | DrugPregnancyCategoryFDAnotEvaluated
  deriving (Show, Eq, Ord, Generic)

instance FromJSON DrugPregnancyCategory where
  parseJSON =
    withText
      "DrugPregnancyCategory"
      ( \case
          "https://schema.org/FDAcategoryA" ->
            pure DrugPregnancyCategoryFDAcategoryA
          "https://schema.org/FDAcategoryB" ->
            pure DrugPregnancyCategoryFDAcategoryB
          "https://schema.org/FDAcategoryC" ->
            pure DrugPregnancyCategoryFDAcategoryC
          "https://schema.org/FDAcategoryD" ->
            pure DrugPregnancyCategoryFDAcategoryD
          "https://schema.org/FDAcategoryX" ->
            pure DrugPregnancyCategoryFDAcategoryX
          "https://schema.org/FDAnotEvaluated" ->
            pure DrugPregnancyCategoryFDAnotEvaluated
          t -> fail ("Failed to parse DrugPregnancyCategory: " <> show t)
      )

instance ToJSON DrugPregnancyCategory where
  toJSON =
    (toJSON :: Text -> Value)
      . ( \case
            DrugPregnancyCategoryFDAcategoryA ->
              "https://schema.org/FDAcategoryA"
            DrugPregnancyCategoryFDAcategoryB ->
              "https://schema.org/FDAcategoryB"
            DrugPregnancyCategoryFDAcategoryC ->
              "https://schema.org/FDAcategoryC"
            DrugPregnancyCategoryFDAcategoryD ->
              "https://schema.org/FDAcategoryD"
            DrugPregnancyCategoryFDAcategoryX ->
              "https://schema.org/FDAcategoryX"
            DrugPregnancyCategoryFDAnotEvaluated ->
              "https://schema.org/FDAnotEvaluated"
        )

data DrugPrescriptionStatus
  = DrugPrescriptionStatusOTC
  | DrugPrescriptionStatusPrescriptionOnly
  deriving (Show, Eq, Ord, Generic)

instance FromJSON DrugPrescriptionStatus where
  parseJSON =
    withText
      "DrugPrescriptionStatus"
      ( \case
          "https://schema.org/OTC" -> pure DrugPrescriptionStatusOTC
          "https://schema.org/PrescriptionOnly" ->
            pure DrugPrescriptionStatusPrescriptionOnly
          t -> fail ("Failed to parse DrugPrescriptionStatus: " <> show t)
      )

instance ToJSON DrugPrescriptionStatus where
  toJSON =
    (toJSON :: Text -> Value)
      . ( \case
            DrugPrescriptionStatusOTC -> "https://schema.org/OTC"
            DrugPrescriptionStatusPrescriptionOnly ->
              "https://schema.org/PrescriptionOnly"
        )

data DrugStrength

classDrugStrength ::
  Class DrugStrength '[MedicalIntangible, MedicalEntity, Thing]
classDrugStrength = Class "DrugStrength"

data DryCleaningOrLaundry

classDryCleaningOrLaundry ::
  Class
    DryCleaningOrLaundry
    '[ LocalBusiness,
       Place,
       Organization,
       Thing,
       Thing
     ]
classDryCleaningOrLaundry = Class "DryCleaningOrLaundry"

data Duration

classDuration :: Class Duration '[Quantity, Intangible, Thing]
classDuration = Class "Duration"

data EUEnergyEfficiencyEnumeration
  = EUEnergyEfficiencyEnumerationEUEnergyEfficiencyCategoryA
  | EUEnergyEfficiencyEnumerationEUEnergyEfficiencyCategoryA1Plus
  | EUEnergyEfficiencyEnumerationEUEnergyEfficiencyCategoryA2Plus
  | EUEnergyEfficiencyEnumerationEUEnergyEfficiencyCategoryA3Plus
  | EUEnergyEfficiencyEnumerationEUEnergyEfficiencyCategoryB
  | EUEnergyEfficiencyEnumerationEUEnergyEfficiencyCategoryC
  | EUEnergyEfficiencyEnumerationEUEnergyEfficiencyCategoryD
  | EUEnergyEfficiencyEnumerationEUEnergyEfficiencyCategoryE
  | EUEnergyEfficiencyEnumerationEUEnergyEfficiencyCategoryF
  | EUEnergyEfficiencyEnumerationEUEnergyEfficiencyCategoryG
  deriving (Show, Eq, Ord, Generic)

instance FromJSON EUEnergyEfficiencyEnumeration where
  parseJSON =
    withText
      "EUEnergyEfficiencyEnumeration"
      ( \case
          "https://schema.org/EUEnergyEfficiencyCategoryA" ->
            pure EUEnergyEfficiencyEnumerationEUEnergyEfficiencyCategoryA
          "https://schema.org/EUEnergyEfficiencyCategoryA1Plus" ->
            pure
              EUEnergyEfficiencyEnumerationEUEnergyEfficiencyCategoryA1Plus
          "https://schema.org/EUEnergyEfficiencyCategoryA2Plus" ->
            pure
              EUEnergyEfficiencyEnumerationEUEnergyEfficiencyCategoryA2Plus
          "https://schema.org/EUEnergyEfficiencyCategoryA3Plus" ->
            pure
              EUEnergyEfficiencyEnumerationEUEnergyEfficiencyCategoryA3Plus
          "https://schema.org/EUEnergyEfficiencyCategoryB" ->
            pure EUEnergyEfficiencyEnumerationEUEnergyEfficiencyCategoryB
          "https://schema.org/EUEnergyEfficiencyCategoryC" ->
            pure EUEnergyEfficiencyEnumerationEUEnergyEfficiencyCategoryC
          "https://schema.org/EUEnergyEfficiencyCategoryD" ->
            pure EUEnergyEfficiencyEnumerationEUEnergyEfficiencyCategoryD
          "https://schema.org/EUEnergyEfficiencyCategoryE" ->
            pure EUEnergyEfficiencyEnumerationEUEnergyEfficiencyCategoryE
          "https://schema.org/EUEnergyEfficiencyCategoryF" ->
            pure EUEnergyEfficiencyEnumerationEUEnergyEfficiencyCategoryF
          "https://schema.org/EUEnergyEfficiencyCategoryG" ->
            pure EUEnergyEfficiencyEnumerationEUEnergyEfficiencyCategoryG
          t ->
            fail
              ("Failed to parse EUEnergyEfficiencyEnumeration: " <> show t)
      )

instance ToJSON EUEnergyEfficiencyEnumeration where
  toJSON =
    (toJSON :: Text -> Value)
      . ( \case
            EUEnergyEfficiencyEnumerationEUEnergyEfficiencyCategoryA ->
              "https://schema.org/EUEnergyEfficiencyCategoryA"
            EUEnergyEfficiencyEnumerationEUEnergyEfficiencyCategoryA1Plus ->
              "https://schema.org/EUEnergyEfficiencyCategoryA1Plus"
            EUEnergyEfficiencyEnumerationEUEnergyEfficiencyCategoryA2Plus ->
              "https://schema.org/EUEnergyEfficiencyCategoryA2Plus"
            EUEnergyEfficiencyEnumerationEUEnergyEfficiencyCategoryA3Plus ->
              "https://schema.org/EUEnergyEfficiencyCategoryA3Plus"
            EUEnergyEfficiencyEnumerationEUEnergyEfficiencyCategoryB ->
              "https://schema.org/EUEnergyEfficiencyCategoryB"
            EUEnergyEfficiencyEnumerationEUEnergyEfficiencyCategoryC ->
              "https://schema.org/EUEnergyEfficiencyCategoryC"
            EUEnergyEfficiencyEnumerationEUEnergyEfficiencyCategoryD ->
              "https://schema.org/EUEnergyEfficiencyCategoryD"
            EUEnergyEfficiencyEnumerationEUEnergyEfficiencyCategoryE ->
              "https://schema.org/EUEnergyEfficiencyCategoryE"
            EUEnergyEfficiencyEnumerationEUEnergyEfficiencyCategoryF ->
              "https://schema.org/EUEnergyEfficiencyCategoryF"
            EUEnergyEfficiencyEnumerationEUEnergyEfficiencyCategoryG ->
              "https://schema.org/EUEnergyEfficiencyCategoryG"
        )

data EatAction

classEatAction :: Class EatAction '[ConsumeAction, Action, Thing]
classEatAction = Class "EatAction"

data EducationEvent

classEducationEvent :: Class EducationEvent '[Event, Thing]
classEducationEvent = Class "EducationEvent"

data EducationalAudience

classEducationalAudience ::
  Class EducationalAudience '[Audience, Intangible, Thing]
classEducationalAudience = Class "EducationalAudience"

data EducationalOccupationalCredential

classEducationalOccupationalCredential ::
  Class EducationalOccupationalCredential '[CreativeWork, Thing]
classEducationalOccupationalCredential =
  Class "EducationalOccupationalCredential"

data EducationalOccupationalProgram

classEducationalOccupationalProgram ::
  Class EducationalOccupationalProgram '[Intangible, Thing]
classEducationalOccupationalProgram =
  Class "EducationalOccupationalProgram"

data EducationalOrganization

classEducationalOrganization ::
  Class
    EducationalOrganization
    '[ CivicStructure,
       Organization,
       Place,
       Thing,
       Thing
     ]
classEducationalOrganization = Class "EducationalOrganization"

data Electrician

classElectrician ::
  Class
    Electrician
    '[ HomeAndConstructionBusiness,
       LocalBusiness,
       Place,
       Organization,
       Thing,
       Thing
     ]
classElectrician = Class "Electrician"

data ElectronicsStore

classElectronicsStore ::
  Class
    ElectronicsStore
    '[ Store,
       LocalBusiness,
       Place,
       Organization,
       Thing,
       Thing
     ]
classElectronicsStore = Class "ElectronicsStore"

data ElementarySchool

classElementarySchool ::
  Class
    ElementarySchool
    '[ EducationalOrganization,
       CivicStructure,
       Organization,
       Place,
       Thing,
       Thing
     ]
classElementarySchool = Class "ElementarySchool"

data EmailMessage

classEmailMessage ::
  Class EmailMessage '[Message, CreativeWork, Thing]
classEmailMessage = Class "EmailMessage"

data Embassy

classEmbassy ::
  Class Embassy '[GovernmentBuilding, CivicStructure, Place, Thing]
classEmbassy = Class "Embassy"

data EmergencyService

classEmergencyService ::
  Class
    EmergencyService
    '[ LocalBusiness,
       Place,
       Organization,
       Thing,
       Thing
     ]
classEmergencyService = Class "EmergencyService"

data EmployeeRole

classEmployeeRole ::
  Class EmployeeRole '[OrganizationRole, Role, Intangible, Thing]
classEmployeeRole = Class "EmployeeRole"

data EmployerAggregateRating

classEmployerAggregateRating ::
  Class
    EmployerAggregateRating
    '[ AggregateRating,
       Rating,
       Intangible,
       Thing
     ]
classEmployerAggregateRating = Class "EmployerAggregateRating"

data EmployerReview

classEmployerReview ::
  Class EmployerReview '[Review, CreativeWork, Thing]
classEmployerReview = Class "EmployerReview"

data EmploymentAgency

classEmploymentAgency ::
  Class
    EmploymentAgency
    '[ LocalBusiness,
       Place,
       Organization,
       Thing,
       Thing
     ]
classEmploymentAgency = Class "EmploymentAgency"

data EndorseAction

classEndorseAction ::
  Class EndorseAction '[ReactAction, AssessAction, Action, Thing]
classEndorseAction = Class "EndorseAction"

data EndorsementRating

classEndorsementRating ::
  Class EndorsementRating '[Rating, Intangible, Thing]
classEndorsementRating = Class "EndorsementRating"

data Energy

classEnergy :: Class Energy '[Quantity, Intangible, Thing]
classEnergy = Class "Energy"

data EnergyConsumptionDetails

classEnergyConsumptionDetails ::
  Class EnergyConsumptionDetails '[Intangible, Thing]
classEnergyConsumptionDetails = Class "EnergyConsumptionDetails"

type EnergyEfficiencyEnumeration = Text

data EnergyStarEnergyEfficiencyEnumeration
  = EnergyStarEnergyEfficiencyEnumerationEnergyStarCertified
  deriving (Show, Eq, Ord, Generic)

instance FromJSON EnergyStarEnergyEfficiencyEnumeration where
  parseJSON =
    withText
      "EnergyStarEnergyEfficiencyEnumeration"
      ( \case
          "https://schema.org/EnergyStarCertified" ->
            pure EnergyStarEnergyEfficiencyEnumerationEnergyStarCertified
          t ->
            fail
              ( "Failed to parse EnergyStarEnergyEfficiencyEnumeration: "
                  <> show t
              )
      )

instance ToJSON EnergyStarEnergyEfficiencyEnumeration where
  toJSON =
    (toJSON :: Text -> Value)
      . ( \case
            EnergyStarEnergyEfficiencyEnumerationEnergyStarCertified ->
              "https://schema.org/EnergyStarCertified"
        )

data EngineSpecification

classEngineSpecification ::
  Class EngineSpecification '[StructuredValue, Intangible, Thing]
classEngineSpecification = Class "EngineSpecification"

data EntertainmentBusiness

classEntertainmentBusiness ::
  Class
    EntertainmentBusiness
    '[ LocalBusiness,
       Place,
       Organization,
       Thing,
       Thing
     ]
classEntertainmentBusiness = Class "EntertainmentBusiness"

data EntryPoint

classEntryPoint :: Class EntryPoint '[Intangible, Thing]
classEntryPoint = Class "EntryPoint"

data Enumeration

classEnumeration :: Class Enumeration '[Intangible, Thing]
classEnumeration = Class "Enumeration"

data Episode

classEpisode :: Class Episode '[CreativeWork, Thing]
classEpisode = Class "Episode"

data Event

classEvent :: Class Event '[Thing]
classEvent = Class "Event"

data EventAttendanceModeEnumeration
  = EventAttendanceModeEnumerationMixedEventAttendanceMode
  | EventAttendanceModeEnumerationOfflineEventAttendanceMode
  | EventAttendanceModeEnumerationOnlineEventAttendanceMode
  deriving (Show, Eq, Ord, Generic)

instance FromJSON EventAttendanceModeEnumeration where
  parseJSON =
    withText
      "EventAttendanceModeEnumeration"
      ( \case
          "https://schema.org/MixedEventAttendanceMode" ->
            pure EventAttendanceModeEnumerationMixedEventAttendanceMode
          "https://schema.org/OfflineEventAttendanceMode" ->
            pure EventAttendanceModeEnumerationOfflineEventAttendanceMode
          "https://schema.org/OnlineEventAttendanceMode" ->
            pure EventAttendanceModeEnumerationOnlineEventAttendanceMode
          t ->
            fail
              ("Failed to parse EventAttendanceModeEnumeration: " <> show t)
      )

instance ToJSON EventAttendanceModeEnumeration where
  toJSON =
    (toJSON :: Text -> Value)
      . ( \case
            EventAttendanceModeEnumerationMixedEventAttendanceMode ->
              "https://schema.org/MixedEventAttendanceMode"
            EventAttendanceModeEnumerationOfflineEventAttendanceMode ->
              "https://schema.org/OfflineEventAttendanceMode"
            EventAttendanceModeEnumerationOnlineEventAttendanceMode ->
              "https://schema.org/OnlineEventAttendanceMode"
        )

data EventReservation

classEventReservation ::
  Class EventReservation '[Reservation, Intangible, Thing]
classEventReservation = Class "EventReservation"

data EventSeries

classEventSeries ::
  Class EventSeries '[Event, Series, Thing, Intangible, Thing]
classEventSeries = Class "EventSeries"

data EventStatusType
  = EventStatusTypeEventCancelled
  | EventStatusTypeEventMovedOnline
  | EventStatusTypeEventPostponed
  | EventStatusTypeEventRescheduled
  | EventStatusTypeEventScheduled
  deriving (Show, Eq, Ord, Generic)

instance FromJSON EventStatusType where
  parseJSON =
    withText
      "EventStatusType"
      ( \case
          "https://schema.org/EventCancelled" ->
            pure EventStatusTypeEventCancelled
          "https://schema.org/EventMovedOnline" ->
            pure EventStatusTypeEventMovedOnline
          "https://schema.org/EventPostponed" ->
            pure EventStatusTypeEventPostponed
          "https://schema.org/EventRescheduled" ->
            pure EventStatusTypeEventRescheduled
          "https://schema.org/EventScheduled" ->
            pure EventStatusTypeEventScheduled
          t -> fail ("Failed to parse EventStatusType: " <> show t)
      )

instance ToJSON EventStatusType where
  toJSON =
    (toJSON :: Text -> Value)
      . ( \case
            EventStatusTypeEventCancelled ->
              "https://schema.org/EventCancelled"
            EventStatusTypeEventMovedOnline ->
              "https://schema.org/EventMovedOnline"
            EventStatusTypeEventPostponed ->
              "https://schema.org/EventPostponed"
            EventStatusTypeEventRescheduled ->
              "https://schema.org/EventRescheduled"
            EventStatusTypeEventScheduled ->
              "https://schema.org/EventScheduled"
        )

data EventVenue

classEventVenue :: Class EventVenue '[CivicStructure, Place, Thing]
classEventVenue = Class "EventVenue"

data ExchangeRateSpecification

classExchangeRateSpecification ::
  Class
    ExchangeRateSpecification
    '[ StructuredValue,
       Intangible,
       Thing
     ]
classExchangeRateSpecification = Class "ExchangeRateSpecification"

data ExerciseAction

classExerciseAction ::
  Class ExerciseAction '[PlayAction, Action, Thing]
classExerciseAction = Class "ExerciseAction"

data ExerciseGym

classExerciseGym ::
  Class
    ExerciseGym
    '[ SportsActivityLocation,
       LocalBusiness,
       Place,
       Organization,
       Thing,
       Thing
     ]
classExerciseGym = Class "ExerciseGym"

data ExercisePlan

classExercisePlan ::
  Class
    ExercisePlan
    '[ CreativeWork,
       PhysicalActivity,
       Thing,
       LifestyleModification,
       MedicalEntity,
       Thing
     ]
classExercisePlan = Class "ExercisePlan"

data ExhibitionEvent

classExhibitionEvent :: Class ExhibitionEvent '[Event, Thing]
classExhibitionEvent = Class "ExhibitionEvent"

data FAQPage

classFAQPage :: Class FAQPage '[WebPage, CreativeWork, Thing]
classFAQPage = Class "FAQPage"

data FMRadioChannel

classFMRadioChannel ::
  Class
    FMRadioChannel
    '[ RadioChannel,
       BroadcastChannel,
       Intangible,
       Thing
     ]
classFMRadioChannel = Class "FMRadioChannel"

data FastFoodRestaurant

classFastFoodRestaurant ::
  Class
    FastFoodRestaurant
    '[ FoodEstablishment,
       LocalBusiness,
       Place,
       Organization,
       Thing,
       Thing
     ]
classFastFoodRestaurant = Class "FastFoodRestaurant"

data Festival

classFestival :: Class Festival '[Event, Thing]
classFestival = Class "Festival"

data FilmAction

classFilmAction :: Class FilmAction '[CreateAction, Action, Thing]
classFilmAction = Class "FilmAction"

data FinancialProduct

classFinancialProduct ::
  Class FinancialProduct '[Service, Intangible, Thing]
classFinancialProduct = Class "FinancialProduct"

data FinancialService

classFinancialService ::
  Class
    FinancialService
    '[ LocalBusiness,
       Place,
       Organization,
       Thing,
       Thing
     ]
classFinancialService = Class "FinancialService"

data FindAction

classFindAction :: Class FindAction '[Action, Thing]
classFindAction = Class "FindAction"

data FireStation

classFireStation ::
  Class
    FireStation
    '[ EmergencyService,
       CivicStructure,
       LocalBusiness,
       Place,
       Organization,
       Thing,
       Thing,
       Place,
       Thing
     ]
classFireStation = Class "FireStation"

data Flight

classFlight :: Class Flight '[Trip, Intangible, Thing]
classFlight = Class "Flight"

data FlightReservation

classFlightReservation ::
  Class FlightReservation '[Reservation, Intangible, Thing]
classFlightReservation = Class "FlightReservation"

data FloorPlan

classFloorPlan :: Class FloorPlan '[Intangible, Thing]
classFloorPlan = Class "FloorPlan"

data Florist

classFlorist ::
  Class
    Florist
    '[ Store,
       LocalBusiness,
       Place,
       Organization,
       Thing,
       Thing
     ]
classFlorist = Class "Florist"

data FollowAction

classFollowAction ::
  Class FollowAction '[InteractAction, Action, Thing]
classFollowAction = Class "FollowAction"

data FoodEstablishment

classFoodEstablishment ::
  Class
    FoodEstablishment
    '[ LocalBusiness,
       Place,
       Organization,
       Thing,
       Thing
     ]
classFoodEstablishment = Class "FoodEstablishment"

data FoodEstablishmentReservation

classFoodEstablishmentReservation ::
  Class
    FoodEstablishmentReservation
    '[ Reservation,
       Intangible,
       Thing
     ]
classFoodEstablishmentReservation =
  Class "FoodEstablishmentReservation"

data FoodEvent

classFoodEvent :: Class FoodEvent '[Event, Thing]
classFoodEvent = Class "FoodEvent"

data FoodService

classFoodService :: Class FoodService '[Service, Intangible, Thing]
classFoodService = Class "FoodService"

data FundingAgency

classFundingAgency ::
  Class FundingAgency '[Project, Organization, Thing]
classFundingAgency = Class "FundingAgency"

data FundingScheme

classFundingScheme :: Class FundingScheme '[Organization, Thing]
classFundingScheme = Class "FundingScheme"

data FurnitureStore

classFurnitureStore ::
  Class
    FurnitureStore
    '[ Store,
       LocalBusiness,
       Place,
       Organization,
       Thing,
       Thing
     ]
classFurnitureStore = Class "FurnitureStore"

data Game

classGame :: Class Game '[CreativeWork, Thing]
classGame = Class "Game"

data GameAvailabilityEnumeration
  = GameAvailabilityEnumerationDemoGameAvailability
  | GameAvailabilityEnumerationFullGameAvailability
  deriving (Show, Eq, Ord, Generic)

instance FromJSON GameAvailabilityEnumeration where
  parseJSON =
    withText
      "GameAvailabilityEnumeration"
      ( \case
          "https://schema.org/DemoGameAvailability" ->
            pure GameAvailabilityEnumerationDemoGameAvailability
          "https://schema.org/FullGameAvailability" ->
            pure GameAvailabilityEnumerationFullGameAvailability
          t ->
            fail
              ("Failed to parse GameAvailabilityEnumeration: " <> show t)
      )

instance ToJSON GameAvailabilityEnumeration where
  toJSON =
    (toJSON :: Text -> Value)
      . ( \case
            GameAvailabilityEnumerationDemoGameAvailability ->
              "https://schema.org/DemoGameAvailability"
            GameAvailabilityEnumerationFullGameAvailability ->
              "https://schema.org/FullGameAvailability"
        )

data GamePlayMode
  = GamePlayModeCoOp
  | GamePlayModeMultiPlayer
  | GamePlayModeSinglePlayer
  deriving (Show, Eq, Ord, Generic)

instance FromJSON GamePlayMode where
  parseJSON =
    withText
      "GamePlayMode"
      ( \case
          "https://schema.org/CoOp" -> pure GamePlayModeCoOp
          "https://schema.org/MultiPlayer" -> pure GamePlayModeMultiPlayer
          "https://schema.org/SinglePlayer" -> pure GamePlayModeSinglePlayer
          t -> fail ("Failed to parse GamePlayMode: " <> show t)
      )

instance ToJSON GamePlayMode where
  toJSON =
    (toJSON :: Text -> Value)
      . ( \case
            GamePlayModeCoOp -> "https://schema.org/CoOp"
            GamePlayModeMultiPlayer -> "https://schema.org/MultiPlayer"
            GamePlayModeSinglePlayer -> "https://schema.org/SinglePlayer"
        )

data GameServer

classGameServer :: Class GameServer '[Intangible, Thing]
classGameServer = Class "GameServer"

data GameServerStatus
  = GameServerStatusOfflinePermanently
  | GameServerStatusOfflineTemporarily
  | GameServerStatusOnline
  | GameServerStatusOnlineFull
  deriving (Show, Eq, Ord, Generic)

instance FromJSON GameServerStatus where
  parseJSON =
    withText
      "GameServerStatus"
      ( \case
          "https://schema.org/OfflinePermanently" ->
            pure GameServerStatusOfflinePermanently
          "https://schema.org/OfflineTemporarily" ->
            pure GameServerStatusOfflineTemporarily
          "https://schema.org/Online" -> pure GameServerStatusOnline
          "https://schema.org/OnlineFull" -> pure GameServerStatusOnlineFull
          t -> fail ("Failed to parse GameServerStatus: " <> show t)
      )

instance ToJSON GameServerStatus where
  toJSON =
    (toJSON :: Text -> Value)
      . ( \case
            GameServerStatusOfflinePermanently ->
              "https://schema.org/OfflinePermanently"
            GameServerStatusOfflineTemporarily ->
              "https://schema.org/OfflineTemporarily"
            GameServerStatusOnline -> "https://schema.org/Online"
            GameServerStatusOnlineFull -> "https://schema.org/OnlineFull"
        )

data GardenStore

classGardenStore ::
  Class
    GardenStore
    '[ Store,
       LocalBusiness,
       Place,
       Organization,
       Thing,
       Thing
     ]
classGardenStore = Class "GardenStore"

data GasStation

classGasStation ::
  Class
    GasStation
    '[ AutomotiveBusiness,
       LocalBusiness,
       Place,
       Organization,
       Thing,
       Thing
     ]
classGasStation = Class "GasStation"

data GatedResidenceCommunity

classGatedResidenceCommunity ::
  Class GatedResidenceCommunity '[Residence, Place, Thing]
classGatedResidenceCommunity = Class "GatedResidenceCommunity"

data GenderType
  = GenderTypeFemale
  | GenderTypeMale
  deriving (Show, Eq, Ord, Generic)

instance FromJSON GenderType where
  parseJSON =
    withText
      "GenderType"
      ( \case
          "https://schema.org/Female" -> pure GenderTypeFemale
          "https://schema.org/Male" -> pure GenderTypeMale
          t -> fail ("Failed to parse GenderType: " <> show t)
      )

instance ToJSON GenderType where
  toJSON =
    (toJSON :: Text -> Value)
      . ( \case
            GenderTypeFemale -> "https://schema.org/Female"
            GenderTypeMale -> "https://schema.org/Male"
        )

data Gene

classGene :: Class Gene '[BioChemEntity, Thing]
classGene = Class "Gene"

data GeneralContractor

classGeneralContractor ::
  Class
    GeneralContractor
    '[ HomeAndConstructionBusiness,
       LocalBusiness,
       Place,
       Organization,
       Thing,
       Thing
     ]
classGeneralContractor = Class "GeneralContractor"

data GeoCircle

classGeoCircle ::
  Class GeoCircle '[GeoShape, StructuredValue, Intangible, Thing]
classGeoCircle = Class "GeoCircle"

data GeoCoordinates

classGeoCoordinates ::
  Class GeoCoordinates '[StructuredValue, Intangible, Thing]
classGeoCoordinates = Class "GeoCoordinates"

data GeoShape

classGeoShape ::
  Class GeoShape '[StructuredValue, Intangible, Thing]
classGeoShape = Class "GeoShape"

data GeospatialGeometry

classGeospatialGeometry ::
  Class GeospatialGeometry '[Intangible, Thing]
classGeospatialGeometry = Class "GeospatialGeometry"

data GiveAction

classGiveAction ::
  Class GiveAction '[TransferAction, Action, Thing]
classGiveAction = Class "GiveAction"

data GolfCourse

classGolfCourse ::
  Class
    GolfCourse
    '[ SportsActivityLocation,
       LocalBusiness,
       Place,
       Organization,
       Thing,
       Thing
     ]
classGolfCourse = Class "GolfCourse"

data GovernmentBenefitsType
  = GovernmentBenefitsTypeBasicIncome
  | GovernmentBenefitsTypeBusinessSupport
  | GovernmentBenefitsTypeDisabilitySupport
  | GovernmentBenefitsTypeHealthCare
  | GovernmentBenefitsTypeOneTimePayments
  | GovernmentBenefitsTypePaidLeave
  | GovernmentBenefitsTypeParentalSupport
  | GovernmentBenefitsTypeUnemploymentSupport
  deriving (Show, Eq, Ord, Generic)

instance FromJSON GovernmentBenefitsType where
  parseJSON =
    withText
      "GovernmentBenefitsType"
      ( \case
          "https://schema.org/BasicIncome" ->
            pure GovernmentBenefitsTypeBasicIncome
          "https://schema.org/BusinessSupport" ->
            pure GovernmentBenefitsTypeBusinessSupport
          "https://schema.org/DisabilitySupport" ->
            pure GovernmentBenefitsTypeDisabilitySupport
          "https://schema.org/HealthCare" ->
            pure GovernmentBenefitsTypeHealthCare
          "https://schema.org/OneTimePayments" ->
            pure GovernmentBenefitsTypeOneTimePayments
          "https://schema.org/PaidLeave" ->
            pure GovernmentBenefitsTypePaidLeave
          "https://schema.org/ParentalSupport" ->
            pure GovernmentBenefitsTypeParentalSupport
          "https://schema.org/UnemploymentSupport" ->
            pure GovernmentBenefitsTypeUnemploymentSupport
          t -> fail ("Failed to parse GovernmentBenefitsType: " <> show t)
      )

instance ToJSON GovernmentBenefitsType where
  toJSON =
    (toJSON :: Text -> Value)
      . ( \case
            GovernmentBenefitsTypeBasicIncome ->
              "https://schema.org/BasicIncome"
            GovernmentBenefitsTypeBusinessSupport ->
              "https://schema.org/BusinessSupport"
            GovernmentBenefitsTypeDisabilitySupport ->
              "https://schema.org/DisabilitySupport"
            GovernmentBenefitsTypeHealthCare -> "https://schema.org/HealthCare"
            GovernmentBenefitsTypeOneTimePayments ->
              "https://schema.org/OneTimePayments"
            GovernmentBenefitsTypePaidLeave -> "https://schema.org/PaidLeave"
            GovernmentBenefitsTypeParentalSupport ->
              "https://schema.org/ParentalSupport"
            GovernmentBenefitsTypeUnemploymentSupport ->
              "https://schema.org/UnemploymentSupport"
        )

data GovernmentBuilding

classGovernmentBuilding ::
  Class GovernmentBuilding '[CivicStructure, Place, Thing]
classGovernmentBuilding = Class "GovernmentBuilding"

data GovernmentOffice

classGovernmentOffice ::
  Class
    GovernmentOffice
    '[ LocalBusiness,
       Place,
       Organization,
       Thing,
       Thing
     ]
classGovernmentOffice = Class "GovernmentOffice"

data GovernmentOrganization

classGovernmentOrganization ::
  Class GovernmentOrganization '[Organization, Thing]
classGovernmentOrganization = Class "GovernmentOrganization"

data GovernmentPermit

classGovernmentPermit ::
  Class GovernmentPermit '[Permit, Intangible, Thing]
classGovernmentPermit = Class "GovernmentPermit"

data GovernmentService

classGovernmentService ::
  Class GovernmentService '[Service, Intangible, Thing]
classGovernmentService = Class "GovernmentService"

data Grant

classGrant :: Class Grant '[Intangible, Thing]
classGrant = Class "Grant"

data GroceryStore

classGroceryStore ::
  Class
    GroceryStore
    '[ Store,
       LocalBusiness,
       Place,
       Organization,
       Thing,
       Thing
     ]
classGroceryStore = Class "GroceryStore"

data Guide

classGuide :: Class Guide '[CreativeWork, Thing]
classGuide = Class "Guide"

data HVACBusiness

classHVACBusiness ::
  Class
    HVACBusiness
    '[ HomeAndConstructionBusiness,
       LocalBusiness,
       Place,
       Organization,
       Thing,
       Thing
     ]
classHVACBusiness = Class "HVACBusiness"

data Hackathon

classHackathon :: Class Hackathon '[Event, Thing]
classHackathon = Class "Hackathon"

data HairSalon

classHairSalon ::
  Class
    HairSalon
    '[ HealthAndBeautyBusiness,
       LocalBusiness,
       Place,
       Organization,
       Thing,
       Thing
     ]
classHairSalon = Class "HairSalon"

data HardwareStore

classHardwareStore ::
  Class
    HardwareStore
    '[ Store,
       LocalBusiness,
       Place,
       Organization,
       Thing,
       Thing
     ]
classHardwareStore = Class "HardwareStore"

data HealthAndBeautyBusiness

classHealthAndBeautyBusiness ::
  Class
    HealthAndBeautyBusiness
    '[ LocalBusiness,
       Place,
       Organization,
       Thing,
       Thing
     ]
classHealthAndBeautyBusiness = Class "HealthAndBeautyBusiness"

data HealthAspectEnumeration
  = HealthAspectEnumerationAllergiesHealthAspect
  | HealthAspectEnumerationBenefitsHealthAspect
  | HealthAspectEnumerationCausesHealthAspect
  | HealthAspectEnumerationContagiousnessHealthAspect
  | HealthAspectEnumerationEffectivenessHealthAspect
  | HealthAspectEnumerationGettingAccessHealthAspect
  | HealthAspectEnumerationHowItWorksHealthAspect
  | HealthAspectEnumerationHowOrWhereHealthAspect
  | HealthAspectEnumerationIngredientsHealthAspect
  | HealthAspectEnumerationLivingWithHealthAspect
  | HealthAspectEnumerationMayTreatHealthAspect
  | HealthAspectEnumerationMisconceptionsHealthAspect
  | HealthAspectEnumerationOverviewHealthAspect
  | HealthAspectEnumerationPatientExperienceHealthAspect
  | HealthAspectEnumerationPregnancyHealthAspect
  | HealthAspectEnumerationPreventionHealthAspect
  | HealthAspectEnumerationPrognosisHealthAspect
  | HealthAspectEnumerationRelatedTopicsHealthAspect
  | HealthAspectEnumerationRisksOrComplicationsHealthAspect
  | HealthAspectEnumerationSafetyHealthAspect
  | HealthAspectEnumerationScreeningHealthAspect
  | HealthAspectEnumerationSeeDoctorHealthAspect
  | HealthAspectEnumerationSelfCareHealthAspect
  | HealthAspectEnumerationSideEffectsHealthAspect
  | HealthAspectEnumerationStagesHealthAspect
  | HealthAspectEnumerationSymptomsHealthAspect
  | HealthAspectEnumerationTreatmentsHealthAspect
  | HealthAspectEnumerationTypesHealthAspect
  | HealthAspectEnumerationUsageOrScheduleHealthAspect
  deriving (Show, Eq, Ord, Generic)

instance FromJSON HealthAspectEnumeration where
  parseJSON =
    withText
      "HealthAspectEnumeration"
      ( \case
          "https://schema.org/AllergiesHealthAspect" ->
            pure HealthAspectEnumerationAllergiesHealthAspect
          "https://schema.org/BenefitsHealthAspect" ->
            pure HealthAspectEnumerationBenefitsHealthAspect
          "https://schema.org/CausesHealthAspect" ->
            pure HealthAspectEnumerationCausesHealthAspect
          "https://schema.org/ContagiousnessHealthAspect" ->
            pure HealthAspectEnumerationContagiousnessHealthAspect
          "https://schema.org/EffectivenessHealthAspect" ->
            pure HealthAspectEnumerationEffectivenessHealthAspect
          "https://schema.org/GettingAccessHealthAspect" ->
            pure HealthAspectEnumerationGettingAccessHealthAspect
          "https://schema.org/HowItWorksHealthAspect" ->
            pure HealthAspectEnumerationHowItWorksHealthAspect
          "https://schema.org/HowOrWhereHealthAspect" ->
            pure HealthAspectEnumerationHowOrWhereHealthAspect
          "https://schema.org/IngredientsHealthAspect" ->
            pure HealthAspectEnumerationIngredientsHealthAspect
          "https://schema.org/LivingWithHealthAspect" ->
            pure HealthAspectEnumerationLivingWithHealthAspect
          "https://schema.org/MayTreatHealthAspect" ->
            pure HealthAspectEnumerationMayTreatHealthAspect
          "https://schema.org/MisconceptionsHealthAspect" ->
            pure HealthAspectEnumerationMisconceptionsHealthAspect
          "https://schema.org/OverviewHealthAspect" ->
            pure HealthAspectEnumerationOverviewHealthAspect
          "https://schema.org/PatientExperienceHealthAspect" ->
            pure HealthAspectEnumerationPatientExperienceHealthAspect
          "https://schema.org/PregnancyHealthAspect" ->
            pure HealthAspectEnumerationPregnancyHealthAspect
          "https://schema.org/PreventionHealthAspect" ->
            pure HealthAspectEnumerationPreventionHealthAspect
          "https://schema.org/PrognosisHealthAspect" ->
            pure HealthAspectEnumerationPrognosisHealthAspect
          "https://schema.org/RelatedTopicsHealthAspect" ->
            pure HealthAspectEnumerationRelatedTopicsHealthAspect
          "https://schema.org/RisksOrComplicationsHealthAspect" ->
            pure HealthAspectEnumerationRisksOrComplicationsHealthAspect
          "https://schema.org/SafetyHealthAspect" ->
            pure HealthAspectEnumerationSafetyHealthAspect
          "https://schema.org/ScreeningHealthAspect" ->
            pure HealthAspectEnumerationScreeningHealthAspect
          "https://schema.org/SeeDoctorHealthAspect" ->
            pure HealthAspectEnumerationSeeDoctorHealthAspect
          "https://schema.org/SelfCareHealthAspect" ->
            pure HealthAspectEnumerationSelfCareHealthAspect
          "https://schema.org/SideEffectsHealthAspect" ->
            pure HealthAspectEnumerationSideEffectsHealthAspect
          "https://schema.org/StagesHealthAspect" ->
            pure HealthAspectEnumerationStagesHealthAspect
          "https://schema.org/SymptomsHealthAspect" ->
            pure HealthAspectEnumerationSymptomsHealthAspect
          "https://schema.org/TreatmentsHealthAspect" ->
            pure HealthAspectEnumerationTreatmentsHealthAspect
          "https://schema.org/TypesHealthAspect" ->
            pure HealthAspectEnumerationTypesHealthAspect
          "https://schema.org/UsageOrScheduleHealthAspect" ->
            pure HealthAspectEnumerationUsageOrScheduleHealthAspect
          t -> fail ("Failed to parse HealthAspectEnumeration: " <> show t)
      )

instance ToJSON HealthAspectEnumeration where
  toJSON =
    (toJSON :: Text -> Value)
      . ( \case
            HealthAspectEnumerationAllergiesHealthAspect ->
              "https://schema.org/AllergiesHealthAspect"
            HealthAspectEnumerationBenefitsHealthAspect ->
              "https://schema.org/BenefitsHealthAspect"
            HealthAspectEnumerationCausesHealthAspect ->
              "https://schema.org/CausesHealthAspect"
            HealthAspectEnumerationContagiousnessHealthAspect ->
              "https://schema.org/ContagiousnessHealthAspect"
            HealthAspectEnumerationEffectivenessHealthAspect ->
              "https://schema.org/EffectivenessHealthAspect"
            HealthAspectEnumerationGettingAccessHealthAspect ->
              "https://schema.org/GettingAccessHealthAspect"
            HealthAspectEnumerationHowItWorksHealthAspect ->
              "https://schema.org/HowItWorksHealthAspect"
            HealthAspectEnumerationHowOrWhereHealthAspect ->
              "https://schema.org/HowOrWhereHealthAspect"
            HealthAspectEnumerationIngredientsHealthAspect ->
              "https://schema.org/IngredientsHealthAspect"
            HealthAspectEnumerationLivingWithHealthAspect ->
              "https://schema.org/LivingWithHealthAspect"
            HealthAspectEnumerationMayTreatHealthAspect ->
              "https://schema.org/MayTreatHealthAspect"
            HealthAspectEnumerationMisconceptionsHealthAspect ->
              "https://schema.org/MisconceptionsHealthAspect"
            HealthAspectEnumerationOverviewHealthAspect ->
              "https://schema.org/OverviewHealthAspect"
            HealthAspectEnumerationPatientExperienceHealthAspect ->
              "https://schema.org/PatientExperienceHealthAspect"
            HealthAspectEnumerationPregnancyHealthAspect ->
              "https://schema.org/PregnancyHealthAspect"
            HealthAspectEnumerationPreventionHealthAspect ->
              "https://schema.org/PreventionHealthAspect"
            HealthAspectEnumerationPrognosisHealthAspect ->
              "https://schema.org/PrognosisHealthAspect"
            HealthAspectEnumerationRelatedTopicsHealthAspect ->
              "https://schema.org/RelatedTopicsHealthAspect"
            HealthAspectEnumerationRisksOrComplicationsHealthAspect ->
              "https://schema.org/RisksOrComplicationsHealthAspect"
            HealthAspectEnumerationSafetyHealthAspect ->
              "https://schema.org/SafetyHealthAspect"
            HealthAspectEnumerationScreeningHealthAspect ->
              "https://schema.org/ScreeningHealthAspect"
            HealthAspectEnumerationSeeDoctorHealthAspect ->
              "https://schema.org/SeeDoctorHealthAspect"
            HealthAspectEnumerationSelfCareHealthAspect ->
              "https://schema.org/SelfCareHealthAspect"
            HealthAspectEnumerationSideEffectsHealthAspect ->
              "https://schema.org/SideEffectsHealthAspect"
            HealthAspectEnumerationStagesHealthAspect ->
              "https://schema.org/StagesHealthAspect"
            HealthAspectEnumerationSymptomsHealthAspect ->
              "https://schema.org/SymptomsHealthAspect"
            HealthAspectEnumerationTreatmentsHealthAspect ->
              "https://schema.org/TreatmentsHealthAspect"
            HealthAspectEnumerationTypesHealthAspect ->
              "https://schema.org/TypesHealthAspect"
            HealthAspectEnumerationUsageOrScheduleHealthAspect ->
              "https://schema.org/UsageOrScheduleHealthAspect"
        )

data HealthClub

classHealthClub ::
  Class
    HealthClub
    '[ HealthAndBeautyBusiness,
       SportsActivityLocation,
       LocalBusiness,
       Place,
       Organization,
       Thing,
       Thing,
       LocalBusiness,
       Place,
       Organization,
       Thing,
       Thing
     ]
classHealthClub = Class "HealthClub"

data HealthInsurancePlan

classHealthInsurancePlan ::
  Class HealthInsurancePlan '[Intangible, Thing]
classHealthInsurancePlan = Class "HealthInsurancePlan"

data HealthPlanCostSharingSpecification

classHealthPlanCostSharingSpecification ::
  Class HealthPlanCostSharingSpecification '[Intangible, Thing]
classHealthPlanCostSharingSpecification =
  Class "HealthPlanCostSharingSpecification"

data HealthPlanFormulary

classHealthPlanFormulary ::
  Class HealthPlanFormulary '[Intangible, Thing]
classHealthPlanFormulary = Class "HealthPlanFormulary"

data HealthPlanNetwork

classHealthPlanNetwork ::
  Class HealthPlanNetwork '[Intangible, Thing]
classHealthPlanNetwork = Class "HealthPlanNetwork"

data HealthTopicContent

classHealthTopicContent ::
  Class HealthTopicContent '[WebContent, CreativeWork, Thing]
classHealthTopicContent = Class "HealthTopicContent"

data HighSchool

classHighSchool ::
  Class
    HighSchool
    '[ EducationalOrganization,
       CivicStructure,
       Organization,
       Place,
       Thing,
       Thing
     ]
classHighSchool = Class "HighSchool"

data HinduTemple

classHinduTemple ::
  Class HinduTemple '[PlaceOfWorship, CivicStructure, Place, Thing]
classHinduTemple = Class "HinduTemple"

data HobbyShop

classHobbyShop ::
  Class
    HobbyShop
    '[ Store,
       LocalBusiness,
       Place,
       Organization,
       Thing,
       Thing
     ]
classHobbyShop = Class "HobbyShop"

data HomeAndConstructionBusiness

classHomeAndConstructionBusiness ::
  Class
    HomeAndConstructionBusiness
    '[ LocalBusiness,
       Place,
       Organization,
       Thing,
       Thing
     ]
classHomeAndConstructionBusiness =
  Class "HomeAndConstructionBusiness"

data HomeGoodsStore

classHomeGoodsStore ::
  Class
    HomeGoodsStore
    '[ Store,
       LocalBusiness,
       Place,
       Organization,
       Thing,
       Thing
     ]
classHomeGoodsStore = Class "HomeGoodsStore"

data Hospital

classHospital ::
  Class
    Hospital
    '[ EmergencyService,
       MedicalOrganization,
       CivicStructure,
       LocalBusiness,
       Place,
       Organization,
       Thing,
       Thing,
       Organization,
       Thing,
       Place,
       Thing
     ]
classHospital = Class "Hospital"

data Hostel

classHostel ::
  Class
    Hostel
    '[ LodgingBusiness,
       LocalBusiness,
       Place,
       Organization,
       Thing,
       Thing
     ]
classHostel = Class "Hostel"

data Hotel

classHotel ::
  Class
    Hotel
    '[ LodgingBusiness,
       LocalBusiness,
       Place,
       Organization,
       Thing,
       Thing
     ]
classHotel = Class "Hotel"

data HotelRoom

classHotelRoom ::
  Class HotelRoom '[Room, Accommodation, Place, Thing]
classHotelRoom = Class "HotelRoom"

data House

classHouse :: Class House '[Accommodation, Place, Thing]
classHouse = Class "House"

data HousePainter

classHousePainter ::
  Class
    HousePainter
    '[ HomeAndConstructionBusiness,
       LocalBusiness,
       Place,
       Organization,
       Thing,
       Thing
     ]
classHousePainter = Class "HousePainter"

data HowTo

classHowTo :: Class HowTo '[CreativeWork, Thing]
classHowTo = Class "HowTo"

data HowToDirection

classHowToDirection ::
  Class
    HowToDirection
    '[ CreativeWork,
       ListItem,
       Thing,
       Intangible,
       Thing
     ]
classHowToDirection = Class "HowToDirection"

data HowToItem

classHowToItem :: Class HowToItem '[ListItem, Intangible, Thing]
classHowToItem = Class "HowToItem"

data HowToSection

classHowToSection ::
  Class
    HowToSection
    '[ ItemList,
       CreativeWork,
       ListItem,
       Intangible,
       Thing,
       Thing,
       Intangible,
       Thing
     ]
classHowToSection = Class "HowToSection"

data HowToStep

classHowToStep ::
  Class
    HowToStep
    '[ ItemList,
       CreativeWork,
       ListItem,
       Intangible,
       Thing,
       Thing,
       Intangible,
       Thing
     ]
classHowToStep = Class "HowToStep"

data HowToSupply

classHowToSupply ::
  Class HowToSupply '[HowToItem, ListItem, Intangible, Thing]
classHowToSupply = Class "HowToSupply"

data HowToTip

classHowToTip ::
  Class HowToTip '[ListItem, CreativeWork, Intangible, Thing, Thing]
classHowToTip = Class "HowToTip"

data HowToTool

classHowToTool ::
  Class HowToTool '[HowToItem, ListItem, Intangible, Thing]
classHowToTool = Class "HowToTool"

data HyperToc

classHyperToc :: Class HyperToc '[CreativeWork, Thing]
classHyperToc = Class "HyperToc"

data HyperTocEntry

classHyperTocEntry :: Class HyperTocEntry '[CreativeWork, Thing]
classHyperTocEntry = Class "HyperTocEntry"

data IceCreamShop

classIceCreamShop ::
  Class
    IceCreamShop
    '[ FoodEstablishment,
       LocalBusiness,
       Place,
       Organization,
       Thing,
       Thing
     ]
classIceCreamShop = Class "IceCreamShop"

data IgnoreAction

classIgnoreAction ::
  Class IgnoreAction '[AssessAction, Action, Thing]
classIgnoreAction = Class "IgnoreAction"

data ImageGallery

classImageGallery ::
  Class
    ImageGallery
    '[ MediaGallery,
       CollectionPage,
       WebPage,
       CreativeWork,
       Thing
     ]
classImageGallery = Class "ImageGallery"

data ImageObject

classImageObject ::
  Class ImageObject '[MediaObject, CreativeWork, Thing]
classImageObject = Class "ImageObject"

data ImageObjectSnapshot

classImageObjectSnapshot ::
  Class
    ImageObjectSnapshot
    '[ ImageObject,
       MediaObject,
       CreativeWork,
       Thing
     ]
classImageObjectSnapshot = Class "ImageObjectSnapshot"

data ImagingTest

classImagingTest ::
  Class ImagingTest '[MedicalTest, MedicalEntity, Thing]
classImagingTest = Class "ImagingTest"

data IndividualProduct

classIndividualProduct :: Class IndividualProduct '[Product, Thing]
classIndividualProduct = Class "IndividualProduct"

data InfectiousAgentClass
  = InfectiousAgentClassBacteria
  | InfectiousAgentClassFungus
  | InfectiousAgentClassMulticellularParasite
  | InfectiousAgentClassPrion
  | InfectiousAgentClassProtozoa
  | InfectiousAgentClassVirus
  deriving (Show, Eq, Ord, Generic)

instance FromJSON InfectiousAgentClass where
  parseJSON =
    withText
      "InfectiousAgentClass"
      ( \case
          "https://schema.org/Bacteria" -> pure InfectiousAgentClassBacteria
          "https://schema.org/Fungus" -> pure InfectiousAgentClassFungus
          "https://schema.org/MulticellularParasite" ->
            pure InfectiousAgentClassMulticellularParasite
          "https://schema.org/Prion" -> pure InfectiousAgentClassPrion
          "https://schema.org/Protozoa" -> pure InfectiousAgentClassProtozoa
          "https://schema.org/Virus" -> pure InfectiousAgentClassVirus
          t -> fail ("Failed to parse InfectiousAgentClass: " <> show t)
      )

instance ToJSON InfectiousAgentClass where
  toJSON =
    (toJSON :: Text -> Value)
      . ( \case
            InfectiousAgentClassBacteria -> "https://schema.org/Bacteria"
            InfectiousAgentClassFungus -> "https://schema.org/Fungus"
            InfectiousAgentClassMulticellularParasite ->
              "https://schema.org/MulticellularParasite"
            InfectiousAgentClassPrion -> "https://schema.org/Prion"
            InfectiousAgentClassProtozoa -> "https://schema.org/Protozoa"
            InfectiousAgentClassVirus -> "https://schema.org/Virus"
        )

data InfectiousDisease

classInfectiousDisease ::
  Class InfectiousDisease '[MedicalCondition, MedicalEntity, Thing]
classInfectiousDisease = Class "InfectiousDisease"

data InformAction

classInformAction ::
  Class
    InformAction
    '[ CommunicateAction,
       InteractAction,
       Action,
       Thing
     ]
classInformAction = Class "InformAction"

data InsertAction

classInsertAction ::
  Class InsertAction '[AddAction, UpdateAction, Action, Thing]
classInsertAction = Class "InsertAction"

data InstallAction

classInstallAction ::
  Class InstallAction '[ConsumeAction, Action, Thing]
classInstallAction = Class "InstallAction"

data InsuranceAgency

classInsuranceAgency ::
  Class
    InsuranceAgency
    '[ FinancialService,
       LocalBusiness,
       Place,
       Organization,
       Thing,
       Thing
     ]
classInsuranceAgency = Class "InsuranceAgency"

data Intangible

classIntangible :: Class Intangible '[Thing]
classIntangible = Class "Intangible"

data InteractAction

classInteractAction :: Class InteractAction '[Action, Thing]
classInteractAction = Class "InteractAction"

data InteractionCounter

classInteractionCounter ::
  Class InteractionCounter '[StructuredValue, Intangible, Thing]
classInteractionCounter = Class "InteractionCounter"

data InternetCafe

classInternetCafe ::
  Class
    InternetCafe
    '[ LocalBusiness,
       Place,
       Organization,
       Thing,
       Thing
     ]
classInternetCafe = Class "InternetCafe"

data InvestmentFund

classInvestmentFund ::
  Class
    InvestmentFund
    '[ InvestmentOrDeposit,
       FinancialProduct,
       Service,
       Intangible,
       Thing
     ]
classInvestmentFund = Class "InvestmentFund"

data InvestmentOrDeposit

classInvestmentOrDeposit ::
  Class
    InvestmentOrDeposit
    '[ FinancialProduct,
       Service,
       Intangible,
       Thing
     ]
classInvestmentOrDeposit = Class "InvestmentOrDeposit"

data InviteAction

classInviteAction ::
  Class
    InviteAction
    '[ CommunicateAction,
       InteractAction,
       Action,
       Thing
     ]
classInviteAction = Class "InviteAction"

data Invoice

classInvoice :: Class Invoice '[Intangible, Thing]
classInvoice = Class "Invoice"

data ItemAvailability
  = ItemAvailabilityBackOrder
  | ItemAvailabilityDiscontinued
  | ItemAvailabilityInStock
  | ItemAvailabilityInStoreOnly
  | ItemAvailabilityLimitedAvailability
  | ItemAvailabilityOnlineOnly
  | ItemAvailabilityOutOfStock
  | ItemAvailabilityPreOrder
  | ItemAvailabilityPreSale
  | ItemAvailabilitySoldOut
  deriving (Show, Eq, Ord, Generic)

instance FromJSON ItemAvailability where
  parseJSON =
    withText
      "ItemAvailability"
      ( \case
          "https://schema.org/BackOrder" -> pure ItemAvailabilityBackOrder
          "https://schema.org/Discontinued" ->
            pure ItemAvailabilityDiscontinued
          "https://schema.org/InStock" -> pure ItemAvailabilityInStock
          "https://schema.org/InStoreOnly" ->
            pure ItemAvailabilityInStoreOnly
          "https://schema.org/LimitedAvailability" ->
            pure ItemAvailabilityLimitedAvailability
          "https://schema.org/OnlineOnly" -> pure ItemAvailabilityOnlineOnly
          "https://schema.org/OutOfStock" -> pure ItemAvailabilityOutOfStock
          "https://schema.org/PreOrder" -> pure ItemAvailabilityPreOrder
          "https://schema.org/PreSale" -> pure ItemAvailabilityPreSale
          "https://schema.org/SoldOut" -> pure ItemAvailabilitySoldOut
          t -> fail ("Failed to parse ItemAvailability: " <> show t)
      )

instance ToJSON ItemAvailability where
  toJSON =
    (toJSON :: Text -> Value)
      . ( \case
            ItemAvailabilityBackOrder -> "https://schema.org/BackOrder"
            ItemAvailabilityDiscontinued -> "https://schema.org/Discontinued"
            ItemAvailabilityInStock -> "https://schema.org/InStock"
            ItemAvailabilityInStoreOnly -> "https://schema.org/InStoreOnly"
            ItemAvailabilityLimitedAvailability ->
              "https://schema.org/LimitedAvailability"
            ItemAvailabilityOnlineOnly -> "https://schema.org/OnlineOnly"
            ItemAvailabilityOutOfStock -> "https://schema.org/OutOfStock"
            ItemAvailabilityPreOrder -> "https://schema.org/PreOrder"
            ItemAvailabilityPreSale -> "https://schema.org/PreSale"
            ItemAvailabilitySoldOut -> "https://schema.org/SoldOut"
        )

data ItemList

classItemList :: Class ItemList '[Intangible, Thing]
classItemList = Class "ItemList"

data ItemListOrderType
  = ItemListOrderTypeItemListOrderAscending
  | ItemListOrderTypeItemListOrderDescending
  | ItemListOrderTypeItemListUnordered
  deriving (Show, Eq, Ord, Generic)

instance FromJSON ItemListOrderType where
  parseJSON =
    withText
      "ItemListOrderType"
      ( \case
          "https://schema.org/ItemListOrderAscending" ->
            pure ItemListOrderTypeItemListOrderAscending
          "https://schema.org/ItemListOrderDescending" ->
            pure ItemListOrderTypeItemListOrderDescending
          "https://schema.org/ItemListUnordered" ->
            pure ItemListOrderTypeItemListUnordered
          t -> fail ("Failed to parse ItemListOrderType: " <> show t)
      )

instance ToJSON ItemListOrderType where
  toJSON =
    (toJSON :: Text -> Value)
      . ( \case
            ItemListOrderTypeItemListOrderAscending ->
              "https://schema.org/ItemListOrderAscending"
            ItemListOrderTypeItemListOrderDescending ->
              "https://schema.org/ItemListOrderDescending"
            ItemListOrderTypeItemListUnordered ->
              "https://schema.org/ItemListUnordered"
        )

data ItemPage

classItemPage :: Class ItemPage '[WebPage, CreativeWork, Thing]
classItemPage = Class "ItemPage"

data JewelryStore

classJewelryStore ::
  Class
    JewelryStore
    '[ Store,
       LocalBusiness,
       Place,
       Organization,
       Thing,
       Thing
     ]
classJewelryStore = Class "JewelryStore"

data JobPosting

classJobPosting :: Class JobPosting '[Intangible, Thing]
classJobPosting = Class "JobPosting"

data JoinAction

classJoinAction ::
  Class JoinAction '[InteractAction, Action, Thing]
classJoinAction = Class "JoinAction"

data Joint

classJoint ::
  Class Joint '[AnatomicalStructure, MedicalEntity, Thing]
classJoint = Class "Joint"

data LakeBodyOfWater

classLakeBodyOfWater ::
  Class LakeBodyOfWater '[BodyOfWater, Landform, Place, Thing]
classLakeBodyOfWater = Class "LakeBodyOfWater"

data Landform

classLandform :: Class Landform '[Place, Thing]
classLandform = Class "Landform"

data LandmarksOrHistoricalBuildings

classLandmarksOrHistoricalBuildings ::
  Class LandmarksOrHistoricalBuildings '[Place, Thing]
classLandmarksOrHistoricalBuildings =
  Class "LandmarksOrHistoricalBuildings"

data Language

classLanguage :: Class Language '[Intangible, Thing]
classLanguage = Class "Language"

data LearningResource

classLearningResource ::
  Class LearningResource '[CreativeWork, Thing]
classLearningResource = Class "LearningResource"

data LeaveAction

classLeaveAction ::
  Class LeaveAction '[InteractAction, Action, Thing]
classLeaveAction = Class "LeaveAction"

data LegalForceStatus
  = LegalForceStatusInForce
  | LegalForceStatusNotInForce
  | LegalForceStatusPartiallyInForce
  deriving (Show, Eq, Ord, Generic)

instance FromJSON LegalForceStatus where
  parseJSON =
    withText
      "LegalForceStatus"
      ( \case
          "https://schema.org/InForce" -> pure LegalForceStatusInForce
          "https://schema.org/NotInForce" -> pure LegalForceStatusNotInForce
          "https://schema.org/PartiallyInForce" ->
            pure LegalForceStatusPartiallyInForce
          t -> fail ("Failed to parse LegalForceStatus: " <> show t)
      )

instance ToJSON LegalForceStatus where
  toJSON =
    (toJSON :: Text -> Value)
      . ( \case
            LegalForceStatusInForce -> "https://schema.org/InForce"
            LegalForceStatusNotInForce -> "https://schema.org/NotInForce"
            LegalForceStatusPartiallyInForce ->
              "https://schema.org/PartiallyInForce"
        )

data LegalService

classLegalService ::
  Class
    LegalService
    '[ LocalBusiness,
       Place,
       Organization,
       Thing,
       Thing
     ]
classLegalService = Class "LegalService"

data LegalValueLevel
  = LegalValueLevelAuthoritativeLegalValue
  | LegalValueLevelDefinitiveLegalValue
  | LegalValueLevelOfficialLegalValue
  | LegalValueLevelUnofficialLegalValue
  deriving (Show, Eq, Ord, Generic)

instance FromJSON LegalValueLevel where
  parseJSON =
    withText
      "LegalValueLevel"
      ( \case
          "https://schema.org/AuthoritativeLegalValue" ->
            pure LegalValueLevelAuthoritativeLegalValue
          "https://schema.org/DefinitiveLegalValue" ->
            pure LegalValueLevelDefinitiveLegalValue
          "https://schema.org/OfficialLegalValue" ->
            pure LegalValueLevelOfficialLegalValue
          "https://schema.org/UnofficialLegalValue" ->
            pure LegalValueLevelUnofficialLegalValue
          t -> fail ("Failed to parse LegalValueLevel: " <> show t)
      )

instance ToJSON LegalValueLevel where
  toJSON =
    (toJSON :: Text -> Value)
      . ( \case
            LegalValueLevelAuthoritativeLegalValue ->
              "https://schema.org/AuthoritativeLegalValue"
            LegalValueLevelDefinitiveLegalValue ->
              "https://schema.org/DefinitiveLegalValue"
            LegalValueLevelOfficialLegalValue ->
              "https://schema.org/OfficialLegalValue"
            LegalValueLevelUnofficialLegalValue ->
              "https://schema.org/UnofficialLegalValue"
        )

data Legislation

classLegislation :: Class Legislation '[CreativeWork, Thing]
classLegislation = Class "Legislation"

data LegislationObject

classLegislationObject ::
  Class
    LegislationObject
    '[ Legislation,
       MediaObject,
       CreativeWork,
       Thing,
       CreativeWork,
       Thing
     ]
classLegislationObject = Class "LegislationObject"

data LegislativeBuilding

classLegislativeBuilding ::
  Class
    LegislativeBuilding
    '[ GovernmentBuilding,
       CivicStructure,
       Place,
       Thing
     ]
classLegislativeBuilding = Class "LegislativeBuilding"

data LendAction

classLendAction ::
  Class LendAction '[TransferAction, Action, Thing]
classLendAction = Class "LendAction"

data Library

classLibrary ::
  Class Library '[LocalBusiness, Place, Organization, Thing, Thing]
classLibrary = Class "Library"

data LibrarySystem

classLibrarySystem :: Class LibrarySystem '[Organization, Thing]
classLibrarySystem = Class "LibrarySystem"

data LifestyleModification

classLifestyleModification ::
  Class LifestyleModification '[MedicalEntity, Thing]
classLifestyleModification = Class "LifestyleModification"

data Ligament

classLigament ::
  Class Ligament '[AnatomicalStructure, MedicalEntity, Thing]
classLigament = Class "Ligament"

data LikeAction

classLikeAction ::
  Class LikeAction '[ReactAction, AssessAction, Action, Thing]
classLikeAction = Class "LikeAction"

data LinkRole

classLinkRole :: Class LinkRole '[Role, Intangible, Thing]
classLinkRole = Class "LinkRole"

data LiquorStore

classLiquorStore ::
  Class
    LiquorStore
    '[ Store,
       LocalBusiness,
       Place,
       Organization,
       Thing,
       Thing
     ]
classLiquorStore = Class "LiquorStore"

data ListItem

classListItem :: Class ListItem '[Intangible, Thing]
classListItem = Class "ListItem"

data ListenAction

classListenAction ::
  Class ListenAction '[ConsumeAction, Action, Thing]
classListenAction = Class "ListenAction"

data LiteraryEvent

classLiteraryEvent :: Class LiteraryEvent '[Event, Thing]
classLiteraryEvent = Class "LiteraryEvent"

data LiveBlogPosting

classLiveBlogPosting ::
  Class
    LiveBlogPosting
    '[ BlogPosting,
       SocialMediaPosting,
       Article,
       CreativeWork,
       Thing
     ]
classLiveBlogPosting = Class "LiveBlogPosting"

data LoanOrCredit

classLoanOrCredit ::
  Class LoanOrCredit '[FinancialProduct, Service, Intangible, Thing]
classLoanOrCredit = Class "LoanOrCredit"

data LocalBusiness

classLocalBusiness ::
  Class LocalBusiness '[Place, Organization, Thing, Thing]
classLocalBusiness = Class "LocalBusiness"

data LocationFeatureSpecification

classLocationFeatureSpecification ::
  Class
    LocationFeatureSpecification
    '[ PropertyValue,
       StructuredValue,
       Intangible,
       Thing
     ]
classLocationFeatureSpecification =
  Class "LocationFeatureSpecification"

data Locksmith

classLocksmith ::
  Class
    Locksmith
    '[ HomeAndConstructionBusiness,
       LocalBusiness,
       Place,
       Organization,
       Thing,
       Thing
     ]
classLocksmith = Class "Locksmith"

data LodgingBusiness

classLodgingBusiness ::
  Class
    LodgingBusiness
    '[ LocalBusiness,
       Place,
       Organization,
       Thing,
       Thing
     ]
classLodgingBusiness = Class "LodgingBusiness"

data LodgingReservation

classLodgingReservation ::
  Class LodgingReservation '[Reservation, Intangible, Thing]
classLodgingReservation = Class "LodgingReservation"

data LoseAction

classLoseAction :: Class LoseAction '[AchieveAction, Action, Thing]
classLoseAction = Class "LoseAction"

data LymphaticVessel

classLymphaticVessel ::
  Class
    LymphaticVessel
    '[ Vessel,
       AnatomicalStructure,
       MedicalEntity,
       Thing
     ]
classLymphaticVessel = Class "LymphaticVessel"

data Manuscript

classManuscript :: Class Manuscript '[CreativeWork, Thing]
classManuscript = Class "Manuscript"

data Map

classMap :: Class Map '[CreativeWork, Thing]
classMap = Class "Map"

data MapCategoryType
  = MapCategoryTypeParkingMap
  | MapCategoryTypeSeatingMap
  | MapCategoryTypeTransitMap
  | MapCategoryTypeVenueMap
  deriving (Show, Eq, Ord, Generic)

instance FromJSON MapCategoryType where
  parseJSON =
    withText
      "MapCategoryType"
      ( \case
          "https://schema.org/ParkingMap" -> pure MapCategoryTypeParkingMap
          "https://schema.org/SeatingMap" -> pure MapCategoryTypeSeatingMap
          "https://schema.org/TransitMap" -> pure MapCategoryTypeTransitMap
          "https://schema.org/VenueMap" -> pure MapCategoryTypeVenueMap
          t -> fail ("Failed to parse MapCategoryType: " <> show t)
      )

instance ToJSON MapCategoryType where
  toJSON =
    (toJSON :: Text -> Value)
      . ( \case
            MapCategoryTypeParkingMap -> "https://schema.org/ParkingMap"
            MapCategoryTypeSeatingMap -> "https://schema.org/SeatingMap"
            MapCategoryTypeTransitMap -> "https://schema.org/TransitMap"
            MapCategoryTypeVenueMap -> "https://schema.org/VenueMap"
        )

data MarryAction

classMarryAction ::
  Class MarryAction '[InteractAction, Action, Thing]
classMarryAction = Class "MarryAction"

data Mass

classMass :: Class Mass '[Quantity, Intangible, Thing]
classMass = Class "Mass"

data MathSolver

classMathSolver :: Class MathSolver '[CreativeWork, Thing]
classMathSolver = Class "MathSolver"

data MaximumDoseSchedule

classMaximumDoseSchedule ::
  Class
    MaximumDoseSchedule
    '[ DoseSchedule,
       MedicalIntangible,
       MedicalEntity,
       Thing
     ]
classMaximumDoseSchedule = Class "MaximumDoseSchedule"

type MeasurementTypeEnumeration = Text

data MediaGallery

classMediaGallery ::
  Class MediaGallery '[CollectionPage, WebPage, CreativeWork, Thing]
classMediaGallery = Class "MediaGallery"

data MediaManipulationRatingEnumeration
  = MediaManipulationRatingEnumerationDecontextualizedContent
  | MediaManipulationRatingEnumerationEditedOrCroppedContent
  | MediaManipulationRatingEnumerationOriginalMediaContent
  | MediaManipulationRatingEnumerationSatireOrParodyContent
  | MediaManipulationRatingEnumerationStagedContent
  | MediaManipulationRatingEnumerationTransformedContent
  deriving (Show, Eq, Ord, Generic)

instance FromJSON MediaManipulationRatingEnumeration where
  parseJSON =
    withText
      "MediaManipulationRatingEnumeration"
      ( \case
          "https://schema.org/DecontextualizedContent" ->
            pure MediaManipulationRatingEnumerationDecontextualizedContent
          "https://schema.org/EditedOrCroppedContent" ->
            pure MediaManipulationRatingEnumerationEditedOrCroppedContent
          "https://schema.org/OriginalMediaContent" ->
            pure MediaManipulationRatingEnumerationOriginalMediaContent
          "https://schema.org/SatireOrParodyContent" ->
            pure MediaManipulationRatingEnumerationSatireOrParodyContent
          "https://schema.org/StagedContent" ->
            pure MediaManipulationRatingEnumerationStagedContent
          "https://schema.org/TransformedContent" ->
            pure MediaManipulationRatingEnumerationTransformedContent
          t ->
            fail
              ("Failed to parse MediaManipulationRatingEnumeration: " <> show t)
      )

instance ToJSON MediaManipulationRatingEnumeration where
  toJSON =
    (toJSON :: Text -> Value)
      . ( \case
            MediaManipulationRatingEnumerationDecontextualizedContent ->
              "https://schema.org/DecontextualizedContent"
            MediaManipulationRatingEnumerationEditedOrCroppedContent ->
              "https://schema.org/EditedOrCroppedContent"
            MediaManipulationRatingEnumerationOriginalMediaContent ->
              "https://schema.org/OriginalMediaContent"
            MediaManipulationRatingEnumerationSatireOrParodyContent ->
              "https://schema.org/SatireOrParodyContent"
            MediaManipulationRatingEnumerationStagedContent ->
              "https://schema.org/StagedContent"
            MediaManipulationRatingEnumerationTransformedContent ->
              "https://schema.org/TransformedContent"
        )

data MediaObject

classMediaObject :: Class MediaObject '[CreativeWork, Thing]
classMediaObject = Class "MediaObject"

data MediaReview

classMediaReview ::
  Class MediaReview '[Review, CreativeWork, Thing]
classMediaReview = Class "MediaReview"

data MediaReviewItem

classMediaReviewItem ::
  Class MediaReviewItem '[CreativeWork, Thing]
classMediaReviewItem = Class "MediaReviewItem"

data MediaSubscription

classMediaSubscription ::
  Class MediaSubscription '[Intangible, Thing]
classMediaSubscription = Class "MediaSubscription"

data MedicalAudience

classMedicalAudience ::
  Class
    MedicalAudience
    '[ PeopleAudience,
       Audience,
       Audience,
       Intangible,
       Thing,
       Intangible,
       Thing
     ]
classMedicalAudience = Class "MedicalAudience"

data MedicalAudienceType
  = MedicalAudienceTypeClinician
  | MedicalAudienceTypeMedicalResearcher
  deriving (Show, Eq, Ord, Generic)

instance FromJSON MedicalAudienceType where
  parseJSON =
    withText
      "MedicalAudienceType"
      ( \case
          "https://schema.org/Clinician" -> pure MedicalAudienceTypeClinician
          "https://schema.org/MedicalResearcher" ->
            pure MedicalAudienceTypeMedicalResearcher
          t -> fail ("Failed to parse MedicalAudienceType: " <> show t)
      )

instance ToJSON MedicalAudienceType where
  toJSON =
    (toJSON :: Text -> Value)
      . ( \case
            MedicalAudienceTypeClinician -> "https://schema.org/Clinician"
            MedicalAudienceTypeMedicalResearcher ->
              "https://schema.org/MedicalResearcher"
        )

data MedicalBusiness

classMedicalBusiness ::
  Class
    MedicalBusiness
    '[ LocalBusiness,
       Place,
       Organization,
       Thing,
       Thing
     ]
classMedicalBusiness = Class "MedicalBusiness"

data MedicalCause

classMedicalCause :: Class MedicalCause '[MedicalEntity, Thing]
classMedicalCause = Class "MedicalCause"

data MedicalClinic

classMedicalClinic ::
  Class
    MedicalClinic
    '[ MedicalBusiness,
       MedicalOrganization,
       LocalBusiness,
       Place,
       Organization,
       Thing,
       Thing,
       Organization,
       Thing
     ]
classMedicalClinic = Class "MedicalClinic"

data MedicalCode

classMedicalCode ::
  Class
    MedicalCode
    '[ MedicalIntangible,
       CategoryCode,
       MedicalEntity,
       Thing,
       DefinedTerm,
       Intangible,
       Thing
     ]
classMedicalCode = Class "MedicalCode"

data MedicalCondition

classMedicalCondition ::
  Class MedicalCondition '[MedicalEntity, Thing]
classMedicalCondition = Class "MedicalCondition"

data MedicalConditionStage

classMedicalConditionStage ::
  Class
    MedicalConditionStage
    '[ MedicalIntangible,
       MedicalEntity,
       Thing
     ]
classMedicalConditionStage = Class "MedicalConditionStage"

data MedicalContraindication

classMedicalContraindication ::
  Class MedicalContraindication '[MedicalEntity, Thing]
classMedicalContraindication = Class "MedicalContraindication"

data MedicalDevice

classMedicalDevice :: Class MedicalDevice '[MedicalEntity, Thing]
classMedicalDevice = Class "MedicalDevice"

data MedicalDevicePurpose
  = MedicalDevicePurposeDiagnostic
  | MedicalDevicePurposeTherapeutic
  deriving (Show, Eq, Ord, Generic)

instance FromJSON MedicalDevicePurpose where
  parseJSON =
    withText
      "MedicalDevicePurpose"
      ( \case
          "https://schema.org/Diagnostic" ->
            pure MedicalDevicePurposeDiagnostic
          "https://schema.org/Therapeutic" ->
            pure MedicalDevicePurposeTherapeutic
          t -> fail ("Failed to parse MedicalDevicePurpose: " <> show t)
      )

instance ToJSON MedicalDevicePurpose where
  toJSON =
    (toJSON :: Text -> Value)
      . ( \case
            MedicalDevicePurposeDiagnostic -> "https://schema.org/Diagnostic"
            MedicalDevicePurposeTherapeutic ->
              "https://schema.org/Therapeutic"
        )

data MedicalEntity

classMedicalEntity :: Class MedicalEntity '[Thing]
classMedicalEntity = Class "MedicalEntity"

type MedicalEnumeration = Text

data MedicalEvidenceLevel
  = MedicalEvidenceLevelEvidenceLevelA
  | MedicalEvidenceLevelEvidenceLevelB
  | MedicalEvidenceLevelEvidenceLevelC
  deriving (Show, Eq, Ord, Generic)

instance FromJSON MedicalEvidenceLevel where
  parseJSON =
    withText
      "MedicalEvidenceLevel"
      ( \case
          "https://schema.org/EvidenceLevelA" ->
            pure MedicalEvidenceLevelEvidenceLevelA
          "https://schema.org/EvidenceLevelB" ->
            pure MedicalEvidenceLevelEvidenceLevelB
          "https://schema.org/EvidenceLevelC" ->
            pure MedicalEvidenceLevelEvidenceLevelC
          t -> fail ("Failed to parse MedicalEvidenceLevel: " <> show t)
      )

instance ToJSON MedicalEvidenceLevel where
  toJSON =
    (toJSON :: Text -> Value)
      . ( \case
            MedicalEvidenceLevelEvidenceLevelA ->
              "https://schema.org/EvidenceLevelA"
            MedicalEvidenceLevelEvidenceLevelB ->
              "https://schema.org/EvidenceLevelB"
            MedicalEvidenceLevelEvidenceLevelC ->
              "https://schema.org/EvidenceLevelC"
        )

data MedicalGuideline

classMedicalGuideline ::
  Class MedicalGuideline '[MedicalEntity, Thing]
classMedicalGuideline = Class "MedicalGuideline"

data MedicalGuidelineContraindication

classMedicalGuidelineContraindication ::
  Class
    MedicalGuidelineContraindication
    '[ MedicalGuideline,
       MedicalEntity,
       Thing
     ]
classMedicalGuidelineContraindication =
  Class "MedicalGuidelineContraindication"

data MedicalGuidelineRecommendation

classMedicalGuidelineRecommendation ::
  Class
    MedicalGuidelineRecommendation
    '[ MedicalGuideline,
       MedicalEntity,
       Thing
     ]
classMedicalGuidelineRecommendation =
  Class "MedicalGuidelineRecommendation"

data MedicalImagingTechnique
  = MedicalImagingTechniqueCT
  | MedicalImagingTechniqueMRI
  | MedicalImagingTechniquePET
  | MedicalImagingTechniqueRadiography
  | MedicalImagingTechniqueUltrasound
  | MedicalImagingTechniqueXRay
  deriving (Show, Eq, Ord, Generic)

instance FromJSON MedicalImagingTechnique where
  parseJSON =
    withText
      "MedicalImagingTechnique"
      ( \case
          "https://schema.org/CT" -> pure MedicalImagingTechniqueCT
          "https://schema.org/MRI" -> pure MedicalImagingTechniqueMRI
          "https://schema.org/PET" -> pure MedicalImagingTechniquePET
          "https://schema.org/Radiography" ->
            pure MedicalImagingTechniqueRadiography
          "https://schema.org/Ultrasound" ->
            pure MedicalImagingTechniqueUltrasound
          "https://schema.org/XRay" -> pure MedicalImagingTechniqueXRay
          t -> fail ("Failed to parse MedicalImagingTechnique: " <> show t)
      )

instance ToJSON MedicalImagingTechnique where
  toJSON =
    (toJSON :: Text -> Value)
      . ( \case
            MedicalImagingTechniqueCT -> "https://schema.org/CT"
            MedicalImagingTechniqueMRI -> "https://schema.org/MRI"
            MedicalImagingTechniquePET -> "https://schema.org/PET"
            MedicalImagingTechniqueRadiography ->
              "https://schema.org/Radiography"
            MedicalImagingTechniqueUltrasound ->
              "https://schema.org/Ultrasound"
            MedicalImagingTechniqueXRay -> "https://schema.org/XRay"
        )

data MedicalIndication

classMedicalIndication ::
  Class MedicalIndication '[MedicalEntity, Thing]
classMedicalIndication = Class "MedicalIndication"

data MedicalIntangible

classMedicalIntangible ::
  Class MedicalIntangible '[MedicalEntity, Thing]
classMedicalIntangible = Class "MedicalIntangible"

data MedicalObservationalStudy

classMedicalObservationalStudy ::
  Class
    MedicalObservationalStudy
    '[ MedicalStudy,
       MedicalEntity,
       Thing
     ]
classMedicalObservationalStudy = Class "MedicalObservationalStudy"

data MedicalObservationalStudyDesign
  = MedicalObservationalStudyDesignCaseSeries
  | MedicalObservationalStudyDesignCohortStudy
  | MedicalObservationalStudyDesignCrossSectional
  | MedicalObservationalStudyDesignLongitudinal
  | MedicalObservationalStudyDesignObservational
  | MedicalObservationalStudyDesignRegistry
  deriving (Show, Eq, Ord, Generic)

instance FromJSON MedicalObservationalStudyDesign where
  parseJSON =
    withText
      "MedicalObservationalStudyDesign"
      ( \case
          "https://schema.org/CaseSeries" ->
            pure MedicalObservationalStudyDesignCaseSeries
          "https://schema.org/CohortStudy" ->
            pure MedicalObservationalStudyDesignCohortStudy
          "https://schema.org/CrossSectional" ->
            pure MedicalObservationalStudyDesignCrossSectional
          "https://schema.org/Longitudinal" ->
            pure MedicalObservationalStudyDesignLongitudinal
          "https://schema.org/Observational" ->
            pure MedicalObservationalStudyDesignObservational
          "https://schema.org/Registry" ->
            pure MedicalObservationalStudyDesignRegistry
          t ->
            fail
              ("Failed to parse MedicalObservationalStudyDesign: " <> show t)
      )

instance ToJSON MedicalObservationalStudyDesign where
  toJSON =
    (toJSON :: Text -> Value)
      . ( \case
            MedicalObservationalStudyDesignCaseSeries ->
              "https://schema.org/CaseSeries"
            MedicalObservationalStudyDesignCohortStudy ->
              "https://schema.org/CohortStudy"
            MedicalObservationalStudyDesignCrossSectional ->
              "https://schema.org/CrossSectional"
            MedicalObservationalStudyDesignLongitudinal ->
              "https://schema.org/Longitudinal"
            MedicalObservationalStudyDesignObservational ->
              "https://schema.org/Observational"
            MedicalObservationalStudyDesignRegistry ->
              "https://schema.org/Registry"
        )

data MedicalOrganization

classMedicalOrganization ::
  Class MedicalOrganization '[Organization, Thing]
classMedicalOrganization = Class "MedicalOrganization"

data MedicalProcedure

classMedicalProcedure ::
  Class MedicalProcedure '[MedicalEntity, Thing]
classMedicalProcedure = Class "MedicalProcedure"

data MedicalProcedureType
  = MedicalProcedureTypeNoninvasiveProcedure
  | MedicalProcedureTypePercutaneousProcedure
  deriving (Show, Eq, Ord, Generic)

instance FromJSON MedicalProcedureType where
  parseJSON =
    withText
      "MedicalProcedureType"
      ( \case
          "https://schema.org/NoninvasiveProcedure" ->
            pure MedicalProcedureTypeNoninvasiveProcedure
          "https://schema.org/PercutaneousProcedure" ->
            pure MedicalProcedureTypePercutaneousProcedure
          t -> fail ("Failed to parse MedicalProcedureType: " <> show t)
      )

instance ToJSON MedicalProcedureType where
  toJSON =
    (toJSON :: Text -> Value)
      . ( \case
            MedicalProcedureTypeNoninvasiveProcedure ->
              "https://schema.org/NoninvasiveProcedure"
            MedicalProcedureTypePercutaneousProcedure ->
              "https://schema.org/PercutaneousProcedure"
        )

data MedicalRiskCalculator

classMedicalRiskCalculator ::
  Class
    MedicalRiskCalculator
    '[ MedicalRiskEstimator,
       MedicalEntity,
       Thing
     ]
classMedicalRiskCalculator = Class "MedicalRiskCalculator"

data MedicalRiskEstimator

classMedicalRiskEstimator ::
  Class MedicalRiskEstimator '[MedicalEntity, Thing]
classMedicalRiskEstimator = Class "MedicalRiskEstimator"

data MedicalRiskFactor

classMedicalRiskFactor ::
  Class MedicalRiskFactor '[MedicalEntity, Thing]
classMedicalRiskFactor = Class "MedicalRiskFactor"

data MedicalRiskScore

classMedicalRiskScore ::
  Class
    MedicalRiskScore
    '[ MedicalRiskEstimator,
       MedicalEntity,
       Thing
     ]
classMedicalRiskScore = Class "MedicalRiskScore"

data MedicalScholarlyArticle

classMedicalScholarlyArticle ::
  Class
    MedicalScholarlyArticle
    '[ ScholarlyArticle,
       Article,
       CreativeWork,
       Thing
     ]
classMedicalScholarlyArticle = Class "MedicalScholarlyArticle"

data MedicalSign

classMedicalSign ::
  Class
    MedicalSign
    '[ MedicalSignOrSymptom,
       MedicalCondition,
       MedicalEntity,
       Thing
     ]
classMedicalSign = Class "MedicalSign"

data MedicalSignOrSymptom

classMedicalSignOrSymptom ::
  Class
    MedicalSignOrSymptom
    '[ MedicalCondition,
       MedicalEntity,
       Thing
     ]
classMedicalSignOrSymptom = Class "MedicalSignOrSymptom"

data MedicalSpecialty
  = MedicalSpecialtyAnesthesia
  | MedicalSpecialtyCardiovascular
  | MedicalSpecialtyCommunityHealth
  | MedicalSpecialtyDentistry
  | MedicalSpecialtyDermatologic
  | MedicalSpecialtyDermatology
  | MedicalSpecialtyDietNutrition
  | MedicalSpecialtyEmergency
  | MedicalSpecialtyEndocrine
  | MedicalSpecialtyGastroenterologic
  | MedicalSpecialtyGenetic
  | MedicalSpecialtyGeriatric
  | MedicalSpecialtyGynecologic
  | MedicalSpecialtyHematologic
  | MedicalSpecialtyInfectious
  | MedicalSpecialtyLaboratoryScience
  | MedicalSpecialtyMidwifery
  | MedicalSpecialtyMusculoskeletal
  | MedicalSpecialtyNeurologic
  | MedicalSpecialtyNursing
  | MedicalSpecialtyObstetric
  | MedicalSpecialtyOncologic
  | MedicalSpecialtyOptometric
  | MedicalSpecialtyOtolaryngologic
  | MedicalSpecialtyPathology
  | MedicalSpecialtyPediatric
  | MedicalSpecialtyPharmacySpecialty
  | MedicalSpecialtyPhysiotherapy
  | MedicalSpecialtyPlasticSurgery
  | MedicalSpecialtyPodiatric
  | MedicalSpecialtyPrimaryCare
  | MedicalSpecialtyPsychiatric
  | MedicalSpecialtyPublicHealth
  | MedicalSpecialtyPulmonary
  | MedicalSpecialtyRadiography
  | MedicalSpecialtyRenal
  | MedicalSpecialtyRespiratoryTherapy
  | MedicalSpecialtyRheumatologic
  | MedicalSpecialtySpeechPathology
  | MedicalSpecialtySurgical
  | MedicalSpecialtyToxicologic
  | MedicalSpecialtyUrologic
  deriving (Show, Eq, Ord, Generic)

instance FromJSON MedicalSpecialty where
  parseJSON =
    withText
      "MedicalSpecialty"
      ( \case
          "https://schema.org/Anesthesia" -> pure MedicalSpecialtyAnesthesia
          "https://schema.org/Cardiovascular" ->
            pure MedicalSpecialtyCardiovascular
          "https://schema.org/CommunityHealth" ->
            pure MedicalSpecialtyCommunityHealth
          "https://schema.org/Dentistry" -> pure MedicalSpecialtyDentistry
          "https://schema.org/Dermatologic" ->
            pure MedicalSpecialtyDermatologic
          "https://schema.org/Dermatology" ->
            pure MedicalSpecialtyDermatology
          "https://schema.org/DietNutrition" ->
            pure MedicalSpecialtyDietNutrition
          "https://schema.org/Emergency" -> pure MedicalSpecialtyEmergency
          "https://schema.org/Endocrine" -> pure MedicalSpecialtyEndocrine
          "https://schema.org/Gastroenterologic" ->
            pure MedicalSpecialtyGastroenterologic
          "https://schema.org/Genetic" -> pure MedicalSpecialtyGenetic
          "https://schema.org/Geriatric" -> pure MedicalSpecialtyGeriatric
          "https://schema.org/Gynecologic" ->
            pure MedicalSpecialtyGynecologic
          "https://schema.org/Hematologic" ->
            pure MedicalSpecialtyHematologic
          "https://schema.org/Infectious" -> pure MedicalSpecialtyInfectious
          "https://schema.org/LaboratoryScience" ->
            pure MedicalSpecialtyLaboratoryScience
          "https://schema.org/Midwifery" -> pure MedicalSpecialtyMidwifery
          "https://schema.org/Musculoskeletal" ->
            pure MedicalSpecialtyMusculoskeletal
          "https://schema.org/Neurologic" -> pure MedicalSpecialtyNeurologic
          "https://schema.org/Nursing" -> pure MedicalSpecialtyNursing
          "https://schema.org/Obstetric" -> pure MedicalSpecialtyObstetric
          "https://schema.org/Oncologic" -> pure MedicalSpecialtyOncologic
          "https://schema.org/Optometric" -> pure MedicalSpecialtyOptometric
          "https://schema.org/Otolaryngologic" ->
            pure MedicalSpecialtyOtolaryngologic
          "https://schema.org/Pathology" -> pure MedicalSpecialtyPathology
          "https://schema.org/Pediatric" -> pure MedicalSpecialtyPediatric
          "https://schema.org/PharmacySpecialty" ->
            pure MedicalSpecialtyPharmacySpecialty
          "https://schema.org/Physiotherapy" ->
            pure MedicalSpecialtyPhysiotherapy
          "https://schema.org/PlasticSurgery" ->
            pure MedicalSpecialtyPlasticSurgery
          "https://schema.org/Podiatric" -> pure MedicalSpecialtyPodiatric
          "https://schema.org/PrimaryCare" ->
            pure MedicalSpecialtyPrimaryCare
          "https://schema.org/Psychiatric" ->
            pure MedicalSpecialtyPsychiatric
          "https://schema.org/PublicHealth" ->
            pure MedicalSpecialtyPublicHealth
          "https://schema.org/Pulmonary" -> pure MedicalSpecialtyPulmonary
          "https://schema.org/Radiography" ->
            pure MedicalSpecialtyRadiography
          "https://schema.org/Renal" -> pure MedicalSpecialtyRenal
          "https://schema.org/RespiratoryTherapy" ->
            pure MedicalSpecialtyRespiratoryTherapy
          "https://schema.org/Rheumatologic" ->
            pure MedicalSpecialtyRheumatologic
          "https://schema.org/SpeechPathology" ->
            pure MedicalSpecialtySpeechPathology
          "https://schema.org/Surgical" -> pure MedicalSpecialtySurgical
          "https://schema.org/Toxicologic" ->
            pure MedicalSpecialtyToxicologic
          "https://schema.org/Urologic" -> pure MedicalSpecialtyUrologic
          t -> fail ("Failed to parse MedicalSpecialty: " <> show t)
      )

instance ToJSON MedicalSpecialty where
  toJSON =
    (toJSON :: Text -> Value)
      . ( \case
            MedicalSpecialtyAnesthesia -> "https://schema.org/Anesthesia"
            MedicalSpecialtyCardiovascular ->
              "https://schema.org/Cardiovascular"
            MedicalSpecialtyCommunityHealth ->
              "https://schema.org/CommunityHealth"
            MedicalSpecialtyDentistry -> "https://schema.org/Dentistry"
            MedicalSpecialtyDermatologic -> "https://schema.org/Dermatologic"
            MedicalSpecialtyDermatology -> "https://schema.org/Dermatology"
            MedicalSpecialtyDietNutrition -> "https://schema.org/DietNutrition"
            MedicalSpecialtyEmergency -> "https://schema.org/Emergency"
            MedicalSpecialtyEndocrine -> "https://schema.org/Endocrine"
            MedicalSpecialtyGastroenterologic ->
              "https://schema.org/Gastroenterologic"
            MedicalSpecialtyGenetic -> "https://schema.org/Genetic"
            MedicalSpecialtyGeriatric -> "https://schema.org/Geriatric"
            MedicalSpecialtyGynecologic -> "https://schema.org/Gynecologic"
            MedicalSpecialtyHematologic -> "https://schema.org/Hematologic"
            MedicalSpecialtyInfectious -> "https://schema.org/Infectious"
            MedicalSpecialtyLaboratoryScience ->
              "https://schema.org/LaboratoryScience"
            MedicalSpecialtyMidwifery -> "https://schema.org/Midwifery"
            MedicalSpecialtyMusculoskeletal ->
              "https://schema.org/Musculoskeletal"
            MedicalSpecialtyNeurologic -> "https://schema.org/Neurologic"
            MedicalSpecialtyNursing -> "https://schema.org/Nursing"
            MedicalSpecialtyObstetric -> "https://schema.org/Obstetric"
            MedicalSpecialtyOncologic -> "https://schema.org/Oncologic"
            MedicalSpecialtyOptometric -> "https://schema.org/Optometric"
            MedicalSpecialtyOtolaryngologic ->
              "https://schema.org/Otolaryngologic"
            MedicalSpecialtyPathology -> "https://schema.org/Pathology"
            MedicalSpecialtyPediatric -> "https://schema.org/Pediatric"
            MedicalSpecialtyPharmacySpecialty ->
              "https://schema.org/PharmacySpecialty"
            MedicalSpecialtyPhysiotherapy -> "https://schema.org/Physiotherapy"
            MedicalSpecialtyPlasticSurgery ->
              "https://schema.org/PlasticSurgery"
            MedicalSpecialtyPodiatric -> "https://schema.org/Podiatric"
            MedicalSpecialtyPrimaryCare -> "https://schema.org/PrimaryCare"
            MedicalSpecialtyPsychiatric -> "https://schema.org/Psychiatric"
            MedicalSpecialtyPublicHealth -> "https://schema.org/PublicHealth"
            MedicalSpecialtyPulmonary -> "https://schema.org/Pulmonary"
            MedicalSpecialtyRadiography -> "https://schema.org/Radiography"
            MedicalSpecialtyRenal -> "https://schema.org/Renal"
            MedicalSpecialtyRespiratoryTherapy ->
              "https://schema.org/RespiratoryTherapy"
            MedicalSpecialtyRheumatologic -> "https://schema.org/Rheumatologic"
            MedicalSpecialtySpeechPathology ->
              "https://schema.org/SpeechPathology"
            MedicalSpecialtySurgical -> "https://schema.org/Surgical"
            MedicalSpecialtyToxicologic -> "https://schema.org/Toxicologic"
            MedicalSpecialtyUrologic -> "https://schema.org/Urologic"
        )

data MedicalStudy

classMedicalStudy :: Class MedicalStudy '[MedicalEntity, Thing]
classMedicalStudy = Class "MedicalStudy"

data MedicalStudyStatus
  = MedicalStudyStatusActiveNotRecruiting
  | MedicalStudyStatusCompleted
  | MedicalStudyStatusEnrollingByInvitation
  | MedicalStudyStatusNotYetRecruiting
  | MedicalStudyStatusRecruiting
  | MedicalStudyStatusResultsAvailable
  | MedicalStudyStatusResultsNotAvailable
  | MedicalStudyStatusSuspended
  | MedicalStudyStatusTerminated
  | MedicalStudyStatusWithdrawn
  deriving (Show, Eq, Ord, Generic)

instance FromJSON MedicalStudyStatus where
  parseJSON =
    withText
      "MedicalStudyStatus"
      ( \case
          "https://schema.org/ActiveNotRecruiting" ->
            pure MedicalStudyStatusActiveNotRecruiting
          "https://schema.org/Completed" -> pure MedicalStudyStatusCompleted
          "https://schema.org/EnrollingByInvitation" ->
            pure MedicalStudyStatusEnrollingByInvitation
          "https://schema.org/NotYetRecruiting" ->
            pure MedicalStudyStatusNotYetRecruiting
          "https://schema.org/Recruiting" ->
            pure MedicalStudyStatusRecruiting
          "https://schema.org/ResultsAvailable" ->
            pure MedicalStudyStatusResultsAvailable
          "https://schema.org/ResultsNotAvailable" ->
            pure MedicalStudyStatusResultsNotAvailable
          "https://schema.org/Suspended" -> pure MedicalStudyStatusSuspended
          "https://schema.org/Terminated" ->
            pure MedicalStudyStatusTerminated
          "https://schema.org/Withdrawn" -> pure MedicalStudyStatusWithdrawn
          t -> fail ("Failed to parse MedicalStudyStatus: " <> show t)
      )

instance ToJSON MedicalStudyStatus where
  toJSON =
    (toJSON :: Text -> Value)
      . ( \case
            MedicalStudyStatusActiveNotRecruiting ->
              "https://schema.org/ActiveNotRecruiting"
            MedicalStudyStatusCompleted -> "https://schema.org/Completed"
            MedicalStudyStatusEnrollingByInvitation ->
              "https://schema.org/EnrollingByInvitation"
            MedicalStudyStatusNotYetRecruiting ->
              "https://schema.org/NotYetRecruiting"
            MedicalStudyStatusRecruiting -> "https://schema.org/Recruiting"
            MedicalStudyStatusResultsAvailable ->
              "https://schema.org/ResultsAvailable"
            MedicalStudyStatusResultsNotAvailable ->
              "https://schema.org/ResultsNotAvailable"
            MedicalStudyStatusSuspended -> "https://schema.org/Suspended"
            MedicalStudyStatusTerminated -> "https://schema.org/Terminated"
            MedicalStudyStatusWithdrawn -> "https://schema.org/Withdrawn"
        )

data MedicalSymptom

classMedicalSymptom ::
  Class
    MedicalSymptom
    '[ MedicalSignOrSymptom,
       MedicalCondition,
       MedicalEntity,
       Thing
     ]
classMedicalSymptom = Class "MedicalSymptom"

data MedicalTest

classMedicalTest :: Class MedicalTest '[MedicalEntity, Thing]
classMedicalTest = Class "MedicalTest"

data MedicalTestPanel

classMedicalTestPanel ::
  Class MedicalTestPanel '[MedicalTest, MedicalEntity, Thing]
classMedicalTestPanel = Class "MedicalTestPanel"

data MedicalTherapy

classMedicalTherapy ::
  Class
    MedicalTherapy
    '[ TherapeuticProcedure,
       MedicalProcedure,
       MedicalEntity,
       Thing
     ]
classMedicalTherapy = Class "MedicalTherapy"

data MedicalTrial

classMedicalTrial ::
  Class MedicalTrial '[MedicalStudy, MedicalEntity, Thing]
classMedicalTrial = Class "MedicalTrial"

data MedicalTrialDesign
  = MedicalTrialDesignDoubleBlindedTrial
  | MedicalTrialDesignInternationalTrial
  | MedicalTrialDesignMultiCenterTrial
  | MedicalTrialDesignOpenTrial
  | MedicalTrialDesignPlaceboControlledTrial
  | MedicalTrialDesignRandomizedTrial
  | MedicalTrialDesignSingleBlindedTrial
  | MedicalTrialDesignSingleCenterTrial
  | MedicalTrialDesignTripleBlindedTrial
  deriving (Show, Eq, Ord, Generic)

instance FromJSON MedicalTrialDesign where
  parseJSON =
    withText
      "MedicalTrialDesign"
      ( \case
          "https://schema.org/DoubleBlindedTrial" ->
            pure MedicalTrialDesignDoubleBlindedTrial
          "https://schema.org/InternationalTrial" ->
            pure MedicalTrialDesignInternationalTrial
          "https://schema.org/MultiCenterTrial" ->
            pure MedicalTrialDesignMultiCenterTrial
          "https://schema.org/OpenTrial" -> pure MedicalTrialDesignOpenTrial
          "https://schema.org/PlaceboControlledTrial" ->
            pure MedicalTrialDesignPlaceboControlledTrial
          "https://schema.org/RandomizedTrial" ->
            pure MedicalTrialDesignRandomizedTrial
          "https://schema.org/SingleBlindedTrial" ->
            pure MedicalTrialDesignSingleBlindedTrial
          "https://schema.org/SingleCenterTrial" ->
            pure MedicalTrialDesignSingleCenterTrial
          "https://schema.org/TripleBlindedTrial" ->
            pure MedicalTrialDesignTripleBlindedTrial
          t -> fail ("Failed to parse MedicalTrialDesign: " <> show t)
      )

instance ToJSON MedicalTrialDesign where
  toJSON =
    (toJSON :: Text -> Value)
      . ( \case
            MedicalTrialDesignDoubleBlindedTrial ->
              "https://schema.org/DoubleBlindedTrial"
            MedicalTrialDesignInternationalTrial ->
              "https://schema.org/InternationalTrial"
            MedicalTrialDesignMultiCenterTrial ->
              "https://schema.org/MultiCenterTrial"
            MedicalTrialDesignOpenTrial -> "https://schema.org/OpenTrial"
            MedicalTrialDesignPlaceboControlledTrial ->
              "https://schema.org/PlaceboControlledTrial"
            MedicalTrialDesignRandomizedTrial ->
              "https://schema.org/RandomizedTrial"
            MedicalTrialDesignSingleBlindedTrial ->
              "https://schema.org/SingleBlindedTrial"
            MedicalTrialDesignSingleCenterTrial ->
              "https://schema.org/SingleCenterTrial"
            MedicalTrialDesignTripleBlindedTrial ->
              "https://schema.org/TripleBlindedTrial"
        )

data MedicalWebPage

classMedicalWebPage ::
  Class MedicalWebPage '[WebPage, CreativeWork, Thing]
classMedicalWebPage = Class "MedicalWebPage"

data MedicineSystem
  = MedicineSystemAyurvedic
  | MedicineSystemChiropractic
  | MedicineSystemHomeopathic
  | MedicineSystemOsteopathic
  | MedicineSystemTraditionalChinese
  | MedicineSystemWesternConventional
  deriving (Show, Eq, Ord, Generic)

instance FromJSON MedicineSystem where
  parseJSON =
    withText
      "MedicineSystem"
      ( \case
          "https://schema.org/Ayurvedic" -> pure MedicineSystemAyurvedic
          "https://schema.org/Chiropractic" ->
            pure MedicineSystemChiropractic
          "https://schema.org/Homeopathic" -> pure MedicineSystemHomeopathic
          "https://schema.org/Osteopathic" -> pure MedicineSystemOsteopathic
          "https://schema.org/TraditionalChinese" ->
            pure MedicineSystemTraditionalChinese
          "https://schema.org/WesternConventional" ->
            pure MedicineSystemWesternConventional
          t -> fail ("Failed to parse MedicineSystem: " <> show t)
      )

instance ToJSON MedicineSystem where
  toJSON =
    (toJSON :: Text -> Value)
      . ( \case
            MedicineSystemAyurvedic -> "https://schema.org/Ayurvedic"
            MedicineSystemChiropractic -> "https://schema.org/Chiropractic"
            MedicineSystemHomeopathic -> "https://schema.org/Homeopathic"
            MedicineSystemOsteopathic -> "https://schema.org/Osteopathic"
            MedicineSystemTraditionalChinese ->
              "https://schema.org/TraditionalChinese"
            MedicineSystemWesternConventional ->
              "https://schema.org/WesternConventional"
        )

data MeetingRoom

classMeetingRoom ::
  Class MeetingRoom '[Room, Accommodation, Place, Thing]
classMeetingRoom = Class "MeetingRoom"

data MensClothingStore

classMensClothingStore ::
  Class
    MensClothingStore
    '[ Store,
       LocalBusiness,
       Place,
       Organization,
       Thing,
       Thing
     ]
classMensClothingStore = Class "MensClothingStore"

data Menu

classMenu :: Class Menu '[CreativeWork, Thing]
classMenu = Class "Menu"

data MenuItem

classMenuItem :: Class MenuItem '[Intangible, Thing]
classMenuItem = Class "MenuItem"

data MenuSection

classMenuSection :: Class MenuSection '[CreativeWork, Thing]
classMenuSection = Class "MenuSection"

data MerchantReturnEnumeration
  = MerchantReturnEnumerationMerchantReturnFiniteReturnWindow
  | MerchantReturnEnumerationMerchantReturnNotPermitted
  | MerchantReturnEnumerationMerchantReturnUnlimitedWindow
  | MerchantReturnEnumerationMerchantReturnUnspecified
  deriving (Show, Eq, Ord, Generic)

instance FromJSON MerchantReturnEnumeration where
  parseJSON =
    withText
      "MerchantReturnEnumeration"
      ( \case
          "https://schema.org/MerchantReturnFiniteReturnWindow" ->
            pure MerchantReturnEnumerationMerchantReturnFiniteReturnWindow
          "https://schema.org/MerchantReturnNotPermitted" ->
            pure MerchantReturnEnumerationMerchantReturnNotPermitted
          "https://schema.org/MerchantReturnUnlimitedWindow" ->
            pure MerchantReturnEnumerationMerchantReturnUnlimitedWindow
          "https://schema.org/MerchantReturnUnspecified" ->
            pure MerchantReturnEnumerationMerchantReturnUnspecified
          t ->
            fail
              ("Failed to parse MerchantReturnEnumeration: " <> show t)
      )

instance ToJSON MerchantReturnEnumeration where
  toJSON =
    (toJSON :: Text -> Value)
      . ( \case
            MerchantReturnEnumerationMerchantReturnFiniteReturnWindow ->
              "https://schema.org/MerchantReturnFiniteReturnWindow"
            MerchantReturnEnumerationMerchantReturnNotPermitted ->
              "https://schema.org/MerchantReturnNotPermitted"
            MerchantReturnEnumerationMerchantReturnUnlimitedWindow ->
              "https://schema.org/MerchantReturnUnlimitedWindow"
            MerchantReturnEnumerationMerchantReturnUnspecified ->
              "https://schema.org/MerchantReturnUnspecified"
        )

data MerchantReturnPolicy

classMerchantReturnPolicy ::
  Class MerchantReturnPolicy '[Intangible, Thing]
classMerchantReturnPolicy = Class "MerchantReturnPolicy"

data MerchantReturnPolicySeasonalOverride

classMerchantReturnPolicySeasonalOverride ::
  Class MerchantReturnPolicySeasonalOverride '[Intangible, Thing]
classMerchantReturnPolicySeasonalOverride =
  Class "MerchantReturnPolicySeasonalOverride"

data Message

classMessage :: Class Message '[CreativeWork, Thing]
classMessage = Class "Message"

data MiddleSchool

classMiddleSchool ::
  Class
    MiddleSchool
    '[ EducationalOrganization,
       CivicStructure,
       Organization,
       Place,
       Thing,
       Thing
     ]
classMiddleSchool = Class "MiddleSchool"

data MobileApplication

classMobileApplication ::
  Class MobileApplication '[SoftwareApplication, CreativeWork, Thing]
classMobileApplication = Class "MobileApplication"

data MobilePhoneStore

classMobilePhoneStore ::
  Class
    MobilePhoneStore
    '[ Store,
       LocalBusiness,
       Place,
       Organization,
       Thing,
       Thing
     ]
classMobilePhoneStore = Class "MobilePhoneStore"

data MolecularEntity

classMolecularEntity ::
  Class MolecularEntity '[BioChemEntity, Thing]
classMolecularEntity = Class "MolecularEntity"

data MonetaryAmount

classMonetaryAmount ::
  Class MonetaryAmount '[StructuredValue, Intangible, Thing]
classMonetaryAmount = Class "MonetaryAmount"

data MonetaryAmountDistribution

classMonetaryAmountDistribution ::
  Class
    MonetaryAmountDistribution
    '[ QuantitativeValueDistribution,
       StructuredValue,
       Intangible,
       Thing
     ]
classMonetaryAmountDistribution =
  Class "MonetaryAmountDistribution"

data MonetaryGrant

classMonetaryGrant ::
  Class MonetaryGrant '[Grant, Intangible, Thing]
classMonetaryGrant = Class "MonetaryGrant"

data MoneyTransfer

classMoneyTransfer ::
  Class MoneyTransfer '[TransferAction, Action, Thing]
classMoneyTransfer = Class "MoneyTransfer"

data MortgageLoan

classMortgageLoan ::
  Class
    MortgageLoan
    '[ LoanOrCredit,
       FinancialProduct,
       Service,
       Intangible,
       Thing
     ]
classMortgageLoan = Class "MortgageLoan"

data Mosque

classMosque ::
  Class Mosque '[PlaceOfWorship, CivicStructure, Place, Thing]
classMosque = Class "Mosque"

data Motel

classMotel ::
  Class
    Motel
    '[ LodgingBusiness,
       LocalBusiness,
       Place,
       Organization,
       Thing,
       Thing
     ]
classMotel = Class "Motel"

data Motorcycle

classMotorcycle :: Class Motorcycle '[Vehicle, Product, Thing]
classMotorcycle = Class "Motorcycle"

data MotorcycleDealer

classMotorcycleDealer ::
  Class
    MotorcycleDealer
    '[ AutomotiveBusiness,
       LocalBusiness,
       Place,
       Organization,
       Thing,
       Thing
     ]
classMotorcycleDealer = Class "MotorcycleDealer"

data MotorcycleRepair

classMotorcycleRepair ::
  Class
    MotorcycleRepair
    '[ AutomotiveBusiness,
       LocalBusiness,
       Place,
       Organization,
       Thing,
       Thing
     ]
classMotorcycleRepair = Class "MotorcycleRepair"

data MotorizedBicycle

classMotorizedBicycle ::
  Class MotorizedBicycle '[Vehicle, Product, Thing]
classMotorizedBicycle = Class "MotorizedBicycle"

data Mountain

classMountain :: Class Mountain '[Landform, Place, Thing]
classMountain = Class "Mountain"

data MoveAction

classMoveAction :: Class MoveAction '[Action, Thing]
classMoveAction = Class "MoveAction"

data Movie

classMovie :: Class Movie '[CreativeWork, Thing]
classMovie = Class "Movie"

data MovieClip

classMovieClip :: Class MovieClip '[Clip, CreativeWork, Thing]
classMovieClip = Class "MovieClip"

data MovieRentalStore

classMovieRentalStore ::
  Class
    MovieRentalStore
    '[ Store,
       LocalBusiness,
       Place,
       Organization,
       Thing,
       Thing
     ]
classMovieRentalStore = Class "MovieRentalStore"

data MovieSeries

classMovieSeries ::
  Class
    MovieSeries
    '[ CreativeWorkSeries,
       Series,
       CreativeWork,
       Intangible,
       Thing,
       Thing
     ]
classMovieSeries = Class "MovieSeries"

data MovieTheater

classMovieTheater ::
  Class
    MovieTheater
    '[ CivicStructure,
       EntertainmentBusiness,
       Place,
       Thing,
       LocalBusiness,
       Place,
       Organization,
       Thing,
       Thing
     ]
classMovieTheater = Class "MovieTheater"

data MovingCompany

classMovingCompany ::
  Class
    MovingCompany
    '[ HomeAndConstructionBusiness,
       LocalBusiness,
       Place,
       Organization,
       Thing,
       Thing
     ]
classMovingCompany = Class "MovingCompany"

data Muscle

classMuscle ::
  Class Muscle '[AnatomicalStructure, MedicalEntity, Thing]
classMuscle = Class "Muscle"

data Museum

classMuseum :: Class Museum '[CivicStructure, Place, Thing]
classMuseum = Class "Museum"

data MusicAlbum

classMusicAlbum ::
  Class MusicAlbum '[MusicPlaylist, CreativeWork, Thing]
classMusicAlbum = Class "MusicAlbum"

data MusicAlbumProductionType
  = MusicAlbumProductionTypeCompilationAlbum
  | MusicAlbumProductionTypeDJMixAlbum
  | MusicAlbumProductionTypeDemoAlbum
  | MusicAlbumProductionTypeLiveAlbum
  | MusicAlbumProductionTypeMixtapeAlbum
  | MusicAlbumProductionTypeRemixAlbum
  | MusicAlbumProductionTypeSoundtrackAlbum
  | MusicAlbumProductionTypeSpokenWordAlbum
  | MusicAlbumProductionTypeStudioAlbum
  deriving (Show, Eq, Ord, Generic)

instance FromJSON MusicAlbumProductionType where
  parseJSON =
    withText
      "MusicAlbumProductionType"
      ( \case
          "https://schema.org/CompilationAlbum" ->
            pure MusicAlbumProductionTypeCompilationAlbum
          "https://schema.org/DJMixAlbum" ->
            pure MusicAlbumProductionTypeDJMixAlbum
          "https://schema.org/DemoAlbum" ->
            pure MusicAlbumProductionTypeDemoAlbum
          "https://schema.org/LiveAlbum" ->
            pure MusicAlbumProductionTypeLiveAlbum
          "https://schema.org/MixtapeAlbum" ->
            pure MusicAlbumProductionTypeMixtapeAlbum
          "https://schema.org/RemixAlbum" ->
            pure MusicAlbumProductionTypeRemixAlbum
          "https://schema.org/SoundtrackAlbum" ->
            pure MusicAlbumProductionTypeSoundtrackAlbum
          "https://schema.org/SpokenWordAlbum" ->
            pure MusicAlbumProductionTypeSpokenWordAlbum
          "https://schema.org/StudioAlbum" ->
            pure MusicAlbumProductionTypeStudioAlbum
          t -> fail ("Failed to parse MusicAlbumProductionType: " <> show t)
      )

instance ToJSON MusicAlbumProductionType where
  toJSON =
    (toJSON :: Text -> Value)
      . ( \case
            MusicAlbumProductionTypeCompilationAlbum ->
              "https://schema.org/CompilationAlbum"
            MusicAlbumProductionTypeDJMixAlbum ->
              "https://schema.org/DJMixAlbum"
            MusicAlbumProductionTypeDemoAlbum -> "https://schema.org/DemoAlbum"
            MusicAlbumProductionTypeLiveAlbum -> "https://schema.org/LiveAlbum"
            MusicAlbumProductionTypeMixtapeAlbum ->
              "https://schema.org/MixtapeAlbum"
            MusicAlbumProductionTypeRemixAlbum ->
              "https://schema.org/RemixAlbum"
            MusicAlbumProductionTypeSoundtrackAlbum ->
              "https://schema.org/SoundtrackAlbum"
            MusicAlbumProductionTypeSpokenWordAlbum ->
              "https://schema.org/SpokenWordAlbum"
            MusicAlbumProductionTypeStudioAlbum ->
              "https://schema.org/StudioAlbum"
        )

data MusicAlbumReleaseType
  = MusicAlbumReleaseTypeAlbumRelease
  | MusicAlbumReleaseTypeBroadcastRelease
  | MusicAlbumReleaseTypeEPRelease
  | MusicAlbumReleaseTypeSingleRelease
  deriving (Show, Eq, Ord, Generic)

instance FromJSON MusicAlbumReleaseType where
  parseJSON =
    withText
      "MusicAlbumReleaseType"
      ( \case
          "https://schema.org/AlbumRelease" ->
            pure MusicAlbumReleaseTypeAlbumRelease
          "https://schema.org/BroadcastRelease" ->
            pure MusicAlbumReleaseTypeBroadcastRelease
          "https://schema.org/EPRelease" ->
            pure MusicAlbumReleaseTypeEPRelease
          "https://schema.org/SingleRelease" ->
            pure MusicAlbumReleaseTypeSingleRelease
          t -> fail ("Failed to parse MusicAlbumReleaseType: " <> show t)
      )

instance ToJSON MusicAlbumReleaseType where
  toJSON =
    (toJSON :: Text -> Value)
      . ( \case
            MusicAlbumReleaseTypeAlbumRelease ->
              "https://schema.org/AlbumRelease"
            MusicAlbumReleaseTypeBroadcastRelease ->
              "https://schema.org/BroadcastRelease"
            MusicAlbumReleaseTypeEPRelease -> "https://schema.org/EPRelease"
            MusicAlbumReleaseTypeSingleRelease ->
              "https://schema.org/SingleRelease"
        )

data MusicComposition

classMusicComposition ::
  Class MusicComposition '[CreativeWork, Thing]
classMusicComposition = Class "MusicComposition"

data MusicEvent

classMusicEvent :: Class MusicEvent '[Event, Thing]
classMusicEvent = Class "MusicEvent"

data MusicGroup

classMusicGroup ::
  Class MusicGroup '[PerformingGroup, Organization, Thing]
classMusicGroup = Class "MusicGroup"

data MusicPlaylist

classMusicPlaylist :: Class MusicPlaylist '[CreativeWork, Thing]
classMusicPlaylist = Class "MusicPlaylist"

data MusicRecording

classMusicRecording :: Class MusicRecording '[CreativeWork, Thing]
classMusicRecording = Class "MusicRecording"

data MusicRelease

classMusicRelease ::
  Class MusicRelease '[MusicPlaylist, CreativeWork, Thing]
classMusicRelease = Class "MusicRelease"

data MusicReleaseFormatType
  = MusicReleaseFormatTypeCDFormat
  | MusicReleaseFormatTypeCassetteFormat
  | MusicReleaseFormatTypeDVDFormat
  | MusicReleaseFormatTypeDigitalAudioTapeFormat
  | MusicReleaseFormatTypeDigitalFormat
  | MusicReleaseFormatTypeLaserDiscFormat
  | MusicReleaseFormatTypeVinylFormat
  deriving (Show, Eq, Ord, Generic)

instance FromJSON MusicReleaseFormatType where
  parseJSON =
    withText
      "MusicReleaseFormatType"
      ( \case
          "https://schema.org/CDFormat" ->
            pure MusicReleaseFormatTypeCDFormat
          "https://schema.org/CassetteFormat" ->
            pure MusicReleaseFormatTypeCassetteFormat
          "https://schema.org/DVDFormat" ->
            pure MusicReleaseFormatTypeDVDFormat
          "https://schema.org/DigitalAudioTapeFormat" ->
            pure MusicReleaseFormatTypeDigitalAudioTapeFormat
          "https://schema.org/DigitalFormat" ->
            pure MusicReleaseFormatTypeDigitalFormat
          "https://schema.org/LaserDiscFormat" ->
            pure MusicReleaseFormatTypeLaserDiscFormat
          "https://schema.org/VinylFormat" ->
            pure MusicReleaseFormatTypeVinylFormat
          t -> fail ("Failed to parse MusicReleaseFormatType: " <> show t)
      )

instance ToJSON MusicReleaseFormatType where
  toJSON =
    (toJSON :: Text -> Value)
      . ( \case
            MusicReleaseFormatTypeCDFormat -> "https://schema.org/CDFormat"
            MusicReleaseFormatTypeCassetteFormat ->
              "https://schema.org/CassetteFormat"
            MusicReleaseFormatTypeDVDFormat -> "https://schema.org/DVDFormat"
            MusicReleaseFormatTypeDigitalAudioTapeFormat ->
              "https://schema.org/DigitalAudioTapeFormat"
            MusicReleaseFormatTypeDigitalFormat ->
              "https://schema.org/DigitalFormat"
            MusicReleaseFormatTypeLaserDiscFormat ->
              "https://schema.org/LaserDiscFormat"
            MusicReleaseFormatTypeVinylFormat ->
              "https://schema.org/VinylFormat"
        )

data MusicStore

classMusicStore ::
  Class
    MusicStore
    '[ Store,
       LocalBusiness,
       Place,
       Organization,
       Thing,
       Thing
     ]
classMusicStore = Class "MusicStore"

data MusicVenue

classMusicVenue :: Class MusicVenue '[CivicStructure, Place, Thing]
classMusicVenue = Class "MusicVenue"

data MusicVideoObject

classMusicVideoObject ::
  Class MusicVideoObject '[MediaObject, CreativeWork, Thing]
classMusicVideoObject = Class "MusicVideoObject"

data NGO

classNGO :: Class NGO '[Organization, Thing]
classNGO = Class "NGO"

data NLNonprofitType
  = NLNonprofitTypeNonprofitANBI
  | NLNonprofitTypeNonprofitSBBI
  deriving (Show, Eq, Ord, Generic)

instance FromJSON NLNonprofitType where
  parseJSON =
    withText
      "NLNonprofitType"
      ( \case
          "https://schema.org/NonprofitANBI" ->
            pure NLNonprofitTypeNonprofitANBI
          "https://schema.org/NonprofitSBBI" ->
            pure NLNonprofitTypeNonprofitSBBI
          t -> fail ("Failed to parse NLNonprofitType: " <> show t)
      )

instance ToJSON NLNonprofitType where
  toJSON =
    (toJSON :: Text -> Value)
      . ( \case
            NLNonprofitTypeNonprofitANBI -> "https://schema.org/NonprofitANBI"
            NLNonprofitTypeNonprofitSBBI -> "https://schema.org/NonprofitSBBI"
        )

data NailSalon

classNailSalon ::
  Class
    NailSalon
    '[ HealthAndBeautyBusiness,
       LocalBusiness,
       Place,
       Organization,
       Thing,
       Thing
     ]
classNailSalon = Class "NailSalon"

data Nerve

classNerve ::
  Class Nerve '[AnatomicalStructure, MedicalEntity, Thing]
classNerve = Class "Nerve"

data NewsArticle

classNewsArticle ::
  Class NewsArticle '[Article, CreativeWork, Thing]
classNewsArticle = Class "NewsArticle"

data NewsMediaOrganization

classNewsMediaOrganization ::
  Class NewsMediaOrganization '[Organization, Thing]
classNewsMediaOrganization = Class "NewsMediaOrganization"

data Newspaper

classNewspaper ::
  Class
    Newspaper
    '[ Periodical,
       CreativeWorkSeries,
       Series,
       CreativeWork,
       Intangible,
       Thing,
       Thing
     ]
classNewspaper = Class "Newspaper"

data NightClub

classNightClub ::
  Class
    NightClub
    '[ EntertainmentBusiness,
       LocalBusiness,
       Place,
       Organization,
       Thing,
       Thing
     ]
classNightClub = Class "NightClub"

type NonprofitType = Text

data Notary

classNotary ::
  Class
    Notary
    '[ LegalService,
       LocalBusiness,
       Place,
       Organization,
       Thing,
       Thing
     ]
classNotary = Class "Notary"

data NoteDigitalDocument

classNoteDigitalDocument ::
  Class NoteDigitalDocument '[DigitalDocument, CreativeWork, Thing]
classNoteDigitalDocument = Class "NoteDigitalDocument"

data NutritionInformation

classNutritionInformation ::
  Class NutritionInformation '[StructuredValue, Intangible, Thing]
classNutritionInformation = Class "NutritionInformation"

data Observation

classObservation :: Class Observation '[Intangible, Thing]
classObservation = Class "Observation"

data Occupation

classOccupation :: Class Occupation '[Intangible, Thing]
classOccupation = Class "Occupation"

data OccupationalExperienceRequirements

classOccupationalExperienceRequirements ::
  Class OccupationalExperienceRequirements '[Intangible, Thing]
classOccupationalExperienceRequirements =
  Class "OccupationalExperienceRequirements"

data OccupationalTherapy

classOccupationalTherapy ::
  Class
    OccupationalTherapy
    '[ MedicalTherapy,
       TherapeuticProcedure,
       MedicalProcedure,
       MedicalEntity,
       Thing
     ]
classOccupationalTherapy = Class "OccupationalTherapy"

data OceanBodyOfWater

classOceanBodyOfWater ::
  Class OceanBodyOfWater '[BodyOfWater, Landform, Place, Thing]
classOceanBodyOfWater = Class "OceanBodyOfWater"

data Offer

classOffer :: Class Offer '[Intangible, Thing]
classOffer = Class "Offer"

data OfferCatalog

classOfferCatalog ::
  Class OfferCatalog '[ItemList, Intangible, Thing]
classOfferCatalog = Class "OfferCatalog"

data OfferForLease

classOfferForLease ::
  Class OfferForLease '[Offer, Intangible, Thing]
classOfferForLease = Class "OfferForLease"

data OfferForPurchase

classOfferForPurchase ::
  Class OfferForPurchase '[Offer, Intangible, Thing]
classOfferForPurchase = Class "OfferForPurchase"

data OfferItemCondition
  = OfferItemConditionDamagedCondition
  | OfferItemConditionNewCondition
  | OfferItemConditionRefurbishedCondition
  | OfferItemConditionUsedCondition
  deriving (Show, Eq, Ord, Generic)

instance FromJSON OfferItemCondition where
  parseJSON =
    withText
      "OfferItemCondition"
      ( \case
          "https://schema.org/DamagedCondition" ->
            pure OfferItemConditionDamagedCondition
          "https://schema.org/NewCondition" ->
            pure OfferItemConditionNewCondition
          "https://schema.org/RefurbishedCondition" ->
            pure OfferItemConditionRefurbishedCondition
          "https://schema.org/UsedCondition" ->
            pure OfferItemConditionUsedCondition
          t -> fail ("Failed to parse OfferItemCondition: " <> show t)
      )

instance ToJSON OfferItemCondition where
  toJSON =
    (toJSON :: Text -> Value)
      . ( \case
            OfferItemConditionDamagedCondition ->
              "https://schema.org/DamagedCondition"
            OfferItemConditionNewCondition -> "https://schema.org/NewCondition"
            OfferItemConditionRefurbishedCondition ->
              "https://schema.org/RefurbishedCondition"
            OfferItemConditionUsedCondition ->
              "https://schema.org/UsedCondition"
        )

data OfferShippingDetails

classOfferShippingDetails ::
  Class OfferShippingDetails '[StructuredValue, Intangible, Thing]
classOfferShippingDetails = Class "OfferShippingDetails"

data OfficeEquipmentStore

classOfficeEquipmentStore ::
  Class
    OfficeEquipmentStore
    '[ Store,
       LocalBusiness,
       Place,
       Organization,
       Thing,
       Thing
     ]
classOfficeEquipmentStore = Class "OfficeEquipmentStore"

data OnDemandEvent

classOnDemandEvent ::
  Class OnDemandEvent '[PublicationEvent, Event, Thing]
classOnDemandEvent = Class "OnDemandEvent"

data OnlineBusiness

classOnlineBusiness :: Class OnlineBusiness '[Organization, Thing]
classOnlineBusiness = Class "OnlineBusiness"

data OnlineStore

classOnlineStore ::
  Class OnlineStore '[OnlineBusiness, Organization, Thing]
classOnlineStore = Class "OnlineStore"

data OpeningHoursSpecification

classOpeningHoursSpecification ::
  Class
    OpeningHoursSpecification
    '[ StructuredValue,
       Intangible,
       Thing
     ]
classOpeningHoursSpecification = Class "OpeningHoursSpecification"

data OpinionNewsArticle

classOpinionNewsArticle ::
  Class
    OpinionNewsArticle
    '[ NewsArticle,
       Article,
       CreativeWork,
       Thing
     ]
classOpinionNewsArticle = Class "OpinionNewsArticle"

data Optician

classOptician ::
  Class
    Optician
    '[ MedicalBusiness,
       LocalBusiness,
       Place,
       Organization,
       Thing,
       Thing
     ]
classOptician = Class "Optician"

data Order

classOrder :: Class Order '[Intangible, Thing]
classOrder = Class "Order"

data OrderAction

classOrderAction :: Class OrderAction '[TradeAction, Action, Thing]
classOrderAction = Class "OrderAction"

data OrderItem

classOrderItem :: Class OrderItem '[Intangible, Thing]
classOrderItem = Class "OrderItem"

data OrderStatus
  = OrderStatusOrderCancelled
  | OrderStatusOrderDelivered
  | OrderStatusOrderInTransit
  | OrderStatusOrderPaymentDue
  | OrderStatusOrderPickupAvailable
  | OrderStatusOrderProblem
  | OrderStatusOrderProcessing
  | OrderStatusOrderReturned
  deriving (Show, Eq, Ord, Generic)

instance FromJSON OrderStatus where
  parseJSON =
    withText
      "OrderStatus"
      ( \case
          "https://schema.org/OrderCancelled" ->
            pure OrderStatusOrderCancelled
          "https://schema.org/OrderDelivered" ->
            pure OrderStatusOrderDelivered
          "https://schema.org/OrderInTransit" ->
            pure OrderStatusOrderInTransit
          "https://schema.org/OrderPaymentDue" ->
            pure OrderStatusOrderPaymentDue
          "https://schema.org/OrderPickupAvailable" ->
            pure OrderStatusOrderPickupAvailable
          "https://schema.org/OrderProblem" -> pure OrderStatusOrderProblem
          "https://schema.org/OrderProcessing" ->
            pure OrderStatusOrderProcessing
          "https://schema.org/OrderReturned" -> pure OrderStatusOrderReturned
          t -> fail ("Failed to parse OrderStatus: " <> show t)
      )

instance ToJSON OrderStatus where
  toJSON =
    (toJSON :: Text -> Value)
      . ( \case
            OrderStatusOrderCancelled -> "https://schema.org/OrderCancelled"
            OrderStatusOrderDelivered -> "https://schema.org/OrderDelivered"
            OrderStatusOrderInTransit -> "https://schema.org/OrderInTransit"
            OrderStatusOrderPaymentDue -> "https://schema.org/OrderPaymentDue"
            OrderStatusOrderPickupAvailable ->
              "https://schema.org/OrderPickupAvailable"
            OrderStatusOrderProblem -> "https://schema.org/OrderProblem"
            OrderStatusOrderProcessing -> "https://schema.org/OrderProcessing"
            OrderStatusOrderReturned -> "https://schema.org/OrderReturned"
        )

data Organization

classOrganization :: Class Organization '[Thing]
classOrganization = Class "Organization"

data OrganizationRole

classOrganizationRole ::
  Class OrganizationRole '[Role, Intangible, Thing]
classOrganizationRole = Class "OrganizationRole"

data OrganizeAction

classOrganizeAction :: Class OrganizeAction '[Action, Thing]
classOrganizeAction = Class "OrganizeAction"

data OutletStore

classOutletStore ::
  Class
    OutletStore
    '[ Store,
       LocalBusiness,
       Place,
       Organization,
       Thing,
       Thing
     ]
classOutletStore = Class "OutletStore"

data OwnershipInfo

classOwnershipInfo ::
  Class OwnershipInfo '[StructuredValue, Intangible, Thing]
classOwnershipInfo = Class "OwnershipInfo"

data PaintAction

classPaintAction ::
  Class PaintAction '[CreateAction, Action, Thing]
classPaintAction = Class "PaintAction"

data Painting

classPainting :: Class Painting '[CreativeWork, Thing]
classPainting = Class "Painting"

data PalliativeProcedure

classPalliativeProcedure ::
  Class
    PalliativeProcedure
    '[ MedicalProcedure,
       MedicalTherapy,
       MedicalEntity,
       Thing,
       TherapeuticProcedure,
       MedicalProcedure,
       MedicalEntity,
       Thing
     ]
classPalliativeProcedure = Class "PalliativeProcedure"

data ParcelDelivery

classParcelDelivery :: Class ParcelDelivery '[Intangible, Thing]
classParcelDelivery = Class "ParcelDelivery"

data ParentAudience

classParentAudience ::
  Class ParentAudience '[PeopleAudience, Audience, Intangible, Thing]
classParentAudience = Class "ParentAudience"

data Park

classPark :: Class Park '[CivicStructure, Place, Thing]
classPark = Class "Park"

data ParkingFacility

classParkingFacility ::
  Class ParkingFacility '[CivicStructure, Place, Thing]
classParkingFacility = Class "ParkingFacility"

data PathologyTest

classPathologyTest ::
  Class PathologyTest '[MedicalTest, MedicalEntity, Thing]
classPathologyTest = Class "PathologyTest"

data Patient

classPatient ::
  Class
    Patient
    '[ MedicalAudience,
       Person,
       PeopleAudience,
       Audience,
       Audience,
       Intangible,
       Thing,
       Intangible,
       Thing,
       Thing
     ]
classPatient = Class "Patient"

data PawnShop

classPawnShop ::
  Class
    PawnShop
    '[ Store,
       LocalBusiness,
       Place,
       Organization,
       Thing,
       Thing
     ]
classPawnShop = Class "PawnShop"

data PayAction

classPayAction :: Class PayAction '[TradeAction, Action, Thing]
classPayAction = Class "PayAction"

type PaymentCard = Text

data PaymentChargeSpecification

classPaymentChargeSpecification ::
  Class
    PaymentChargeSpecification
    '[ PriceSpecification,
       StructuredValue,
       Intangible,
       Thing
     ]
classPaymentChargeSpecification =
  Class "PaymentChargeSpecification"

type PaymentMethod = Text

data PaymentService

classPaymentService ::
  Class
    PaymentService
    '[ FinancialProduct,
       Service,
       Intangible,
       Thing
     ]
classPaymentService = Class "PaymentService"

data PaymentStatusType
  = PaymentStatusTypePaymentAutomaticallyApplied
  | PaymentStatusTypePaymentComplete
  | PaymentStatusTypePaymentDeclined
  | PaymentStatusTypePaymentDue
  | PaymentStatusTypePaymentPastDue
  deriving (Show, Eq, Ord, Generic)

instance FromJSON PaymentStatusType where
  parseJSON =
    withText
      "PaymentStatusType"
      ( \case
          "https://schema.org/PaymentAutomaticallyApplied" ->
            pure PaymentStatusTypePaymentAutomaticallyApplied
          "https://schema.org/PaymentComplete" ->
            pure PaymentStatusTypePaymentComplete
          "https://schema.org/PaymentDeclined" ->
            pure PaymentStatusTypePaymentDeclined
          "https://schema.org/PaymentDue" -> pure PaymentStatusTypePaymentDue
          "https://schema.org/PaymentPastDue" ->
            pure PaymentStatusTypePaymentPastDue
          t -> fail ("Failed to parse PaymentStatusType: " <> show t)
      )

instance ToJSON PaymentStatusType where
  toJSON =
    (toJSON :: Text -> Value)
      . ( \case
            PaymentStatusTypePaymentAutomaticallyApplied ->
              "https://schema.org/PaymentAutomaticallyApplied"
            PaymentStatusTypePaymentComplete ->
              "https://schema.org/PaymentComplete"
            PaymentStatusTypePaymentDeclined ->
              "https://schema.org/PaymentDeclined"
            PaymentStatusTypePaymentDue -> "https://schema.org/PaymentDue"
            PaymentStatusTypePaymentPastDue ->
              "https://schema.org/PaymentPastDue"
        )

data PeopleAudience

classPeopleAudience ::
  Class PeopleAudience '[Audience, Intangible, Thing]
classPeopleAudience = Class "PeopleAudience"

data PerformAction

classPerformAction ::
  Class PerformAction '[PlayAction, Action, Thing]
classPerformAction = Class "PerformAction"

data PerformanceRole

classPerformanceRole ::
  Class PerformanceRole '[Role, Intangible, Thing]
classPerformanceRole = Class "PerformanceRole"

data PerformingArtsTheater

classPerformingArtsTheater ::
  Class PerformingArtsTheater '[CivicStructure, Place, Thing]
classPerformingArtsTheater = Class "PerformingArtsTheater"

data PerformingGroup

classPerformingGroup ::
  Class PerformingGroup '[Organization, Thing]
classPerformingGroup = Class "PerformingGroup"

data Periodical

classPeriodical ::
  Class
    Periodical
    '[ CreativeWorkSeries,
       Series,
       CreativeWork,
       Intangible,
       Thing,
       Thing
     ]
classPeriodical = Class "Periodical"

data Permit

classPermit :: Class Permit '[Intangible, Thing]
classPermit = Class "Permit"

data Person

classPerson :: Class Person '[Thing]
classPerson = Class "Person"

data PetStore

classPetStore ::
  Class
    PetStore
    '[ Store,
       LocalBusiness,
       Place,
       Organization,
       Thing,
       Thing
     ]
classPetStore = Class "PetStore"

data Pharmacy

classPharmacy ::
  Class
    Pharmacy
    '[ MedicalBusiness,
       MedicalOrganization,
       LocalBusiness,
       Place,
       Organization,
       Thing,
       Thing,
       Organization,
       Thing
     ]
classPharmacy = Class "Pharmacy"

data Photograph

classPhotograph :: Class Photograph '[CreativeWork, Thing]
classPhotograph = Class "Photograph"

data PhotographAction

classPhotographAction ::
  Class PhotographAction '[CreateAction, Action, Thing]
classPhotographAction = Class "PhotographAction"

data PhysicalActivity

classPhysicalActivity ::
  Class
    PhysicalActivity
    '[ LifestyleModification,
       MedicalEntity,
       Thing
     ]
classPhysicalActivity = Class "PhysicalActivity"

data PhysicalActivityCategory
  = PhysicalActivityCategoryAerobicActivity
  | PhysicalActivityCategoryAnaerobicActivity
  | PhysicalActivityCategoryBalance
  | PhysicalActivityCategoryFlexibility
  | PhysicalActivityCategoryLeisureTimeActivity
  | PhysicalActivityCategoryOccupationalActivity
  | PhysicalActivityCategoryStrengthTraining
  deriving (Show, Eq, Ord, Generic)

instance FromJSON PhysicalActivityCategory where
  parseJSON =
    withText
      "PhysicalActivityCategory"
      ( \case
          "https://schema.org/AerobicActivity" ->
            pure PhysicalActivityCategoryAerobicActivity
          "https://schema.org/AnaerobicActivity" ->
            pure PhysicalActivityCategoryAnaerobicActivity
          "https://schema.org/Balance" ->
            pure PhysicalActivityCategoryBalance
          "https://schema.org/Flexibility" ->
            pure PhysicalActivityCategoryFlexibility
          "https://schema.org/LeisureTimeActivity" ->
            pure PhysicalActivityCategoryLeisureTimeActivity
          "https://schema.org/OccupationalActivity" ->
            pure PhysicalActivityCategoryOccupationalActivity
          "https://schema.org/StrengthTraining" ->
            pure PhysicalActivityCategoryStrengthTraining
          t -> fail ("Failed to parse PhysicalActivityCategory: " <> show t)
      )

instance ToJSON PhysicalActivityCategory where
  toJSON =
    (toJSON :: Text -> Value)
      . ( \case
            PhysicalActivityCategoryAerobicActivity ->
              "https://schema.org/AerobicActivity"
            PhysicalActivityCategoryAnaerobicActivity ->
              "https://schema.org/AnaerobicActivity"
            PhysicalActivityCategoryBalance -> "https://schema.org/Balance"
            PhysicalActivityCategoryFlexibility ->
              "https://schema.org/Flexibility"
            PhysicalActivityCategoryLeisureTimeActivity ->
              "https://schema.org/LeisureTimeActivity"
            PhysicalActivityCategoryOccupationalActivity ->
              "https://schema.org/OccupationalActivity"
            PhysicalActivityCategoryStrengthTraining ->
              "https://schema.org/StrengthTraining"
        )

data PhysicalExam
  = PhysicalExamAbdomen
  | PhysicalExamAppearance
  | PhysicalExamCardiovascularExam
  | PhysicalExamEar
  | PhysicalExamEye
  | PhysicalExamGenitourinary
  | PhysicalExamHead
  | PhysicalExamLung
  | PhysicalExamMusculoskeletalExam
  | PhysicalExamNeck
  | PhysicalExamNeuro
  | PhysicalExamNose
  | PhysicalExamSkin
  | PhysicalExamThroat
  deriving (Show, Eq, Ord, Generic)

instance FromJSON PhysicalExam where
  parseJSON =
    withText
      "PhysicalExam"
      ( \case
          "https://schema.org/Abdomen" -> pure PhysicalExamAbdomen
          "https://schema.org/Appearance" -> pure PhysicalExamAppearance
          "https://schema.org/CardiovascularExam" ->
            pure PhysicalExamCardiovascularExam
          "https://schema.org/Ear" -> pure PhysicalExamEar
          "https://schema.org/Eye" -> pure PhysicalExamEye
          "https://schema.org/Genitourinary" ->
            pure PhysicalExamGenitourinary
          "https://schema.org/Head" -> pure PhysicalExamHead
          "https://schema.org/Lung" -> pure PhysicalExamLung
          "https://schema.org/MusculoskeletalExam" ->
            pure PhysicalExamMusculoskeletalExam
          "https://schema.org/Neck" -> pure PhysicalExamNeck
          "https://schema.org/Neuro" -> pure PhysicalExamNeuro
          "https://schema.org/Nose" -> pure PhysicalExamNose
          "https://schema.org/Skin" -> pure PhysicalExamSkin
          "https://schema.org/Throat" -> pure PhysicalExamThroat
          t -> fail ("Failed to parse PhysicalExam: " <> show t)
      )

instance ToJSON PhysicalExam where
  toJSON =
    (toJSON :: Text -> Value)
      . ( \case
            PhysicalExamAbdomen -> "https://schema.org/Abdomen"
            PhysicalExamAppearance -> "https://schema.org/Appearance"
            PhysicalExamCardiovascularExam ->
              "https://schema.org/CardiovascularExam"
            PhysicalExamEar -> "https://schema.org/Ear"
            PhysicalExamEye -> "https://schema.org/Eye"
            PhysicalExamGenitourinary -> "https://schema.org/Genitourinary"
            PhysicalExamHead -> "https://schema.org/Head"
            PhysicalExamLung -> "https://schema.org/Lung"
            PhysicalExamMusculoskeletalExam ->
              "https://schema.org/MusculoskeletalExam"
            PhysicalExamNeck -> "https://schema.org/Neck"
            PhysicalExamNeuro -> "https://schema.org/Neuro"
            PhysicalExamNose -> "https://schema.org/Nose"
            PhysicalExamSkin -> "https://schema.org/Skin"
            PhysicalExamThroat -> "https://schema.org/Throat"
        )

data PhysicalTherapy

classPhysicalTherapy ::
  Class
    PhysicalTherapy
    '[ MedicalTherapy,
       TherapeuticProcedure,
       MedicalProcedure,
       MedicalEntity,
       Thing
     ]
classPhysicalTherapy = Class "PhysicalTherapy"

data Physician

classPhysician ::
  Class
    Physician
    '[ MedicalOrganization,
       MedicalBusiness,
       Organization,
       Thing,
       LocalBusiness,
       Place,
       Organization,
       Thing,
       Thing
     ]
classPhysician = Class "Physician"

data Place

classPlace :: Class Place '[Thing]
classPlace = Class "Place"

data PlaceOfWorship

classPlaceOfWorship ::
  Class PlaceOfWorship '[CivicStructure, Place, Thing]
classPlaceOfWorship = Class "PlaceOfWorship"

data PlanAction

classPlanAction ::
  Class PlanAction '[OrganizeAction, Action, Thing]
classPlanAction = Class "PlanAction"

data Play

classPlay :: Class Play '[CreativeWork, Thing]
classPlay = Class "Play"

data PlayAction

classPlayAction :: Class PlayAction '[Action, Thing]
classPlayAction = Class "PlayAction"

data PlayGameAction

classPlayGameAction ::
  Class PlayGameAction '[ConsumeAction, Action, Thing]
classPlayGameAction = Class "PlayGameAction"

data Playground

classPlayground :: Class Playground '[CivicStructure, Place, Thing]
classPlayground = Class "Playground"

data Plumber

classPlumber ::
  Class
    Plumber
    '[ HomeAndConstructionBusiness,
       LocalBusiness,
       Place,
       Organization,
       Thing,
       Thing
     ]
classPlumber = Class "Plumber"

data PodcastEpisode

classPodcastEpisode ::
  Class PodcastEpisode '[Episode, CreativeWork, Thing]
classPodcastEpisode = Class "PodcastEpisode"

data PodcastSeason

classPodcastSeason ::
  Class PodcastSeason '[CreativeWorkSeason, CreativeWork, Thing]
classPodcastSeason = Class "PodcastSeason"

data PodcastSeries

classPodcastSeries ::
  Class
    PodcastSeries
    '[ CreativeWorkSeries,
       Series,
       CreativeWork,
       Intangible,
       Thing,
       Thing
     ]
classPodcastSeries = Class "PodcastSeries"

data PoliceStation

classPoliceStation ::
  Class
    PoliceStation
    '[ EmergencyService,
       CivicStructure,
       LocalBusiness,
       Place,
       Organization,
       Thing,
       Thing,
       Place,
       Thing
     ]
classPoliceStation = Class "PoliceStation"

data Pond

classPond :: Class Pond '[BodyOfWater, Landform, Place, Thing]
classPond = Class "Pond"

data PostOffice

classPostOffice ::
  Class
    PostOffice
    '[ GovernmentOffice,
       LocalBusiness,
       Place,
       Organization,
       Thing,
       Thing
     ]
classPostOffice = Class "PostOffice"

data PostalAddress

classPostalAddress ::
  Class
    PostalAddress
    '[ ContactPoint,
       StructuredValue,
       Intangible,
       Thing
     ]
classPostalAddress = Class "PostalAddress"

data PostalCodeRangeSpecification

classPostalCodeRangeSpecification ::
  Class
    PostalCodeRangeSpecification
    '[ StructuredValue,
       Intangible,
       Thing
     ]
classPostalCodeRangeSpecification =
  Class "PostalCodeRangeSpecification"

data Poster

classPoster :: Class Poster '[CreativeWork, Thing]
classPoster = Class "Poster"

data PreOrderAction

classPreOrderAction ::
  Class PreOrderAction '[TradeAction, Action, Thing]
classPreOrderAction = Class "PreOrderAction"

data PrependAction

classPrependAction ::
  Class
    PrependAction
    '[ InsertAction,
       AddAction,
       UpdateAction,
       Action,
       Thing
     ]
classPrependAction = Class "PrependAction"

data Preschool

classPreschool ::
  Class
    Preschool
    '[ EducationalOrganization,
       CivicStructure,
       Organization,
       Place,
       Thing,
       Thing
     ]
classPreschool = Class "Preschool"

data PresentationDigitalDocument

classPresentationDigitalDocument ::
  Class
    PresentationDigitalDocument
    '[ DigitalDocument,
       CreativeWork,
       Thing
     ]
classPresentationDigitalDocument =
  Class "PresentationDigitalDocument"

data PreventionIndication

classPreventionIndication ::
  Class
    PreventionIndication
    '[ MedicalIndication,
       MedicalEntity,
       Thing
     ]
classPreventionIndication = Class "PreventionIndication"

data PriceComponentTypeEnumeration
  = PriceComponentTypeEnumerationActivationFee
  | PriceComponentTypeEnumerationCleaningFee
  | PriceComponentTypeEnumerationDistanceFee
  | PriceComponentTypeEnumerationDownpayment
  | PriceComponentTypeEnumerationInstallment
  | PriceComponentTypeEnumerationSubscription
  deriving (Show, Eq, Ord, Generic)

instance FromJSON PriceComponentTypeEnumeration where
  parseJSON =
    withText
      "PriceComponentTypeEnumeration"
      ( \case
          "https://schema.org/ActivationFee" ->
            pure PriceComponentTypeEnumerationActivationFee
          "https://schema.org/CleaningFee" ->
            pure PriceComponentTypeEnumerationCleaningFee
          "https://schema.org/DistanceFee" ->
            pure PriceComponentTypeEnumerationDistanceFee
          "https://schema.org/Downpayment" ->
            pure PriceComponentTypeEnumerationDownpayment
          "https://schema.org/Installment" ->
            pure PriceComponentTypeEnumerationInstallment
          "https://schema.org/Subscription" ->
            pure PriceComponentTypeEnumerationSubscription
          t ->
            fail
              ("Failed to parse PriceComponentTypeEnumeration: " <> show t)
      )

instance ToJSON PriceComponentTypeEnumeration where
  toJSON =
    (toJSON :: Text -> Value)
      . ( \case
            PriceComponentTypeEnumerationActivationFee ->
              "https://schema.org/ActivationFee"
            PriceComponentTypeEnumerationCleaningFee ->
              "https://schema.org/CleaningFee"
            PriceComponentTypeEnumerationDistanceFee ->
              "https://schema.org/DistanceFee"
            PriceComponentTypeEnumerationDownpayment ->
              "https://schema.org/Downpayment"
            PriceComponentTypeEnumerationInstallment ->
              "https://schema.org/Installment"
            PriceComponentTypeEnumerationSubscription ->
              "https://schema.org/Subscription"
        )

data PriceSpecification

classPriceSpecification ::
  Class PriceSpecification '[StructuredValue, Intangible, Thing]
classPriceSpecification = Class "PriceSpecification"

data PriceTypeEnumeration
  = PriceTypeEnumerationInvoicePrice
  | PriceTypeEnumerationListPrice
  | PriceTypeEnumerationMSRP
  | PriceTypeEnumerationMinimumAdvertisedPrice
  | PriceTypeEnumerationSRP
  | PriceTypeEnumerationSalePrice
  deriving (Show, Eq, Ord, Generic)

instance FromJSON PriceTypeEnumeration where
  parseJSON =
    withText
      "PriceTypeEnumeration"
      ( \case
          "https://schema.org/InvoicePrice" ->
            pure PriceTypeEnumerationInvoicePrice
          "https://schema.org/ListPrice" ->
            pure PriceTypeEnumerationListPrice
          "https://schema.org/MSRP" -> pure PriceTypeEnumerationMSRP
          "https://schema.org/MinimumAdvertisedPrice" ->
            pure PriceTypeEnumerationMinimumAdvertisedPrice
          "https://schema.org/SRP" -> pure PriceTypeEnumerationSRP
          "https://schema.org/SalePrice" ->
            pure PriceTypeEnumerationSalePrice
          t -> fail ("Failed to parse PriceTypeEnumeration: " <> show t)
      )

instance ToJSON PriceTypeEnumeration where
  toJSON =
    (toJSON :: Text -> Value)
      . ( \case
            PriceTypeEnumerationInvoicePrice ->
              "https://schema.org/InvoicePrice"
            PriceTypeEnumerationListPrice -> "https://schema.org/ListPrice"
            PriceTypeEnumerationMSRP -> "https://schema.org/MSRP"
            PriceTypeEnumerationMinimumAdvertisedPrice ->
              "https://schema.org/MinimumAdvertisedPrice"
            PriceTypeEnumerationSRP -> "https://schema.org/SRP"
            PriceTypeEnumerationSalePrice -> "https://schema.org/SalePrice"
        )

data Product

classProduct :: Class Product '[Thing]
classProduct = Class "Product"

data ProductCollection

classProductCollection ::
  Class
    ProductCollection
    '[ Collection,
       Product,
       CreativeWork,
       Thing,
       Thing
     ]
classProductCollection = Class "ProductCollection"

data ProductGroup

classProductGroup :: Class ProductGroup '[Product, Thing]
classProductGroup = Class "ProductGroup"

data ProductModel

classProductModel :: Class ProductModel '[Product, Thing]
classProductModel = Class "ProductModel"

data ProfessionalService

classProfessionalService ::
  Class
    ProfessionalService
    '[ LocalBusiness,
       Place,
       Organization,
       Thing,
       Thing
     ]
classProfessionalService = Class "ProfessionalService"

data ProfilePage

classProfilePage ::
  Class ProfilePage '[WebPage, CreativeWork, Thing]
classProfilePage = Class "ProfilePage"

data ProgramMembership

classProgramMembership ::
  Class ProgramMembership '[Intangible, Thing]
classProgramMembership = Class "ProgramMembership"

data Project

classProject :: Class Project '[Organization, Thing]
classProject = Class "Project"

type PronounceableText = Text

data Property_

classProperty_ :: Class Property_ '[Intangible, Thing]
classProperty_ = Class "Property"

data PropertyValue

classPropertyValue ::
  Class PropertyValue '[StructuredValue, Intangible, Thing]
classPropertyValue = Class "PropertyValue"

data PropertyValueSpecification

classPropertyValueSpecification ::
  Class PropertyValueSpecification '[Intangible, Thing]
classPropertyValueSpecification =
  Class "PropertyValueSpecification"

data Protein

classProtein :: Class Protein '[BioChemEntity, Thing]
classProtein = Class "Protein"

data PsychologicalTreatment

classPsychologicalTreatment ::
  Class
    PsychologicalTreatment
    '[ TherapeuticProcedure,
       MedicalProcedure,
       MedicalEntity,
       Thing
     ]
classPsychologicalTreatment = Class "PsychologicalTreatment"

data PublicSwimmingPool

classPublicSwimmingPool ::
  Class
    PublicSwimmingPool
    '[ SportsActivityLocation,
       LocalBusiness,
       Place,
       Organization,
       Thing,
       Thing
     ]
classPublicSwimmingPool = Class "PublicSwimmingPool"

data PublicToilet

classPublicToilet ::
  Class PublicToilet '[CivicStructure, Place, Thing]
classPublicToilet = Class "PublicToilet"

data PublicationEvent

classPublicationEvent :: Class PublicationEvent '[Event, Thing]
classPublicationEvent = Class "PublicationEvent"

data PublicationIssue

classPublicationIssue ::
  Class PublicationIssue '[CreativeWork, Thing]
classPublicationIssue = Class "PublicationIssue"

data PublicationVolume

classPublicationVolume ::
  Class PublicationVolume '[CreativeWork, Thing]
classPublicationVolume = Class "PublicationVolume"

data QAPage

classQAPage :: Class QAPage '[WebPage, CreativeWork, Thing]
classQAPage = Class "QAPage"

type QualitativeValue = Text

data QuantitativeValue

classQuantitativeValue ::
  Class QuantitativeValue '[StructuredValue, Intangible, Thing]
classQuantitativeValue = Class "QuantitativeValue"

data QuantitativeValueDistribution

classQuantitativeValueDistribution ::
  Class
    QuantitativeValueDistribution
    '[ StructuredValue,
       Intangible,
       Thing
     ]
classQuantitativeValueDistribution =
  Class "QuantitativeValueDistribution"

data Quantity

classQuantity :: Class Quantity '[Intangible, Thing]
classQuantity = Class "Quantity"

data Question

classQuestion :: Class Question '[Comment, CreativeWork, Thing]
classQuestion = Class "Question"

data Quiz

classQuiz :: Class Quiz '[LearningResource, CreativeWork, Thing]
classQuiz = Class "Quiz"

data Quotation

classQuotation :: Class Quotation '[CreativeWork, Thing]
classQuotation = Class "Quotation"

data QuoteAction

classQuoteAction :: Class QuoteAction '[TradeAction, Action, Thing]
classQuoteAction = Class "QuoteAction"

data RVPark

classRVPark :: Class RVPark '[CivicStructure, Place, Thing]
classRVPark = Class "RVPark"

data RadiationTherapy

classRadiationTherapy ::
  Class
    RadiationTherapy
    '[ MedicalTherapy,
       TherapeuticProcedure,
       MedicalProcedure,
       MedicalEntity,
       Thing
     ]
classRadiationTherapy = Class "RadiationTherapy"

data RadioBroadcastService

classRadioBroadcastService ::
  Class
    RadioBroadcastService
    '[ BroadcastService,
       Service,
       Intangible,
       Thing
     ]
classRadioBroadcastService = Class "RadioBroadcastService"

data RadioChannel

classRadioChannel ::
  Class RadioChannel '[BroadcastChannel, Intangible, Thing]
classRadioChannel = Class "RadioChannel"

data RadioClip

classRadioClip :: Class RadioClip '[Clip, CreativeWork, Thing]
classRadioClip = Class "RadioClip"

data RadioEpisode

classRadioEpisode ::
  Class RadioEpisode '[Episode, CreativeWork, Thing]
classRadioEpisode = Class "RadioEpisode"

data RadioSeason

classRadioSeason ::
  Class RadioSeason '[CreativeWorkSeason, CreativeWork, Thing]
classRadioSeason = Class "RadioSeason"

data RadioSeries

classRadioSeries ::
  Class
    RadioSeries
    '[ CreativeWorkSeries,
       Series,
       CreativeWork,
       Intangible,
       Thing,
       Thing
     ]
classRadioSeries = Class "RadioSeries"

data RadioStation

classRadioStation ::
  Class
    RadioStation
    '[ LocalBusiness,
       Place,
       Organization,
       Thing,
       Thing
     ]
classRadioStation = Class "RadioStation"

data Rating

classRating :: Class Rating '[Intangible, Thing]
classRating = Class "Rating"

data ReactAction

classReactAction ::
  Class ReactAction '[AssessAction, Action, Thing]
classReactAction = Class "ReactAction"

data ReadAction

classReadAction :: Class ReadAction '[ConsumeAction, Action, Thing]
classReadAction = Class "ReadAction"

data RealEstateAgent

classRealEstateAgent ::
  Class
    RealEstateAgent
    '[ LocalBusiness,
       Place,
       Organization,
       Thing,
       Thing
     ]
classRealEstateAgent = Class "RealEstateAgent"

data RealEstateListing

classRealEstateListing ::
  Class RealEstateListing '[WebPage, CreativeWork, Thing]
classRealEstateListing = Class "RealEstateListing"

data ReceiveAction

classReceiveAction ::
  Class ReceiveAction '[TransferAction, Action, Thing]
classReceiveAction = Class "ReceiveAction"

data Recipe

classRecipe :: Class Recipe '[HowTo, CreativeWork, Thing]
classRecipe = Class "Recipe"

data Recommendation

classRecommendation ::
  Class Recommendation '[Review, CreativeWork, Thing]
classRecommendation = Class "Recommendation"

data RecommendedDoseSchedule

classRecommendedDoseSchedule ::
  Class
    RecommendedDoseSchedule
    '[ DoseSchedule,
       MedicalIntangible,
       MedicalEntity,
       Thing
     ]
classRecommendedDoseSchedule = Class "RecommendedDoseSchedule"

data RecyclingCenter

classRecyclingCenter ::
  Class
    RecyclingCenter
    '[ LocalBusiness,
       Place,
       Organization,
       Thing,
       Thing
     ]
classRecyclingCenter = Class "RecyclingCenter"

data RefundTypeEnumeration
  = RefundTypeEnumerationExchangeRefund
  | RefundTypeEnumerationFullRefund
  | RefundTypeEnumerationStoreCreditRefund
  deriving (Show, Eq, Ord, Generic)

instance FromJSON RefundTypeEnumeration where
  parseJSON =
    withText
      "RefundTypeEnumeration"
      ( \case
          "https://schema.org/ExchangeRefund" ->
            pure RefundTypeEnumerationExchangeRefund
          "https://schema.org/FullRefund" ->
            pure RefundTypeEnumerationFullRefund
          "https://schema.org/StoreCreditRefund" ->
            pure RefundTypeEnumerationStoreCreditRefund
          t -> fail ("Failed to parse RefundTypeEnumeration: " <> show t)
      )

instance ToJSON RefundTypeEnumeration where
  toJSON =
    (toJSON :: Text -> Value)
      . ( \case
            RefundTypeEnumerationExchangeRefund ->
              "https://schema.org/ExchangeRefund"
            RefundTypeEnumerationFullRefund -> "https://schema.org/FullRefund"
            RefundTypeEnumerationStoreCreditRefund ->
              "https://schema.org/StoreCreditRefund"
        )

data RegisterAction

classRegisterAction ::
  Class RegisterAction '[InteractAction, Action, Thing]
classRegisterAction = Class "RegisterAction"

data RejectAction

classRejectAction ::
  Class RejectAction '[AllocateAction, OrganizeAction, Action, Thing]
classRejectAction = Class "RejectAction"

data RentAction

classRentAction :: Class RentAction '[TradeAction, Action, Thing]
classRentAction = Class "RentAction"

data RentalCarReservation

classRentalCarReservation ::
  Class RentalCarReservation '[Reservation, Intangible, Thing]
classRentalCarReservation = Class "RentalCarReservation"

data RepaymentSpecification

classRepaymentSpecification ::
  Class RepaymentSpecification '[StructuredValue, Intangible, Thing]
classRepaymentSpecification = Class "RepaymentSpecification"

data ReplaceAction

classReplaceAction ::
  Class ReplaceAction '[UpdateAction, Action, Thing]
classReplaceAction = Class "ReplaceAction"

data ReplyAction

classReplyAction ::
  Class
    ReplyAction
    '[ CommunicateAction,
       InteractAction,
       Action,
       Thing
     ]
classReplyAction = Class "ReplyAction"

data Report

classReport :: Class Report '[Article, CreativeWork, Thing]
classReport = Class "Report"

data ReportageNewsArticle

classReportageNewsArticle ::
  Class
    ReportageNewsArticle
    '[ NewsArticle,
       Article,
       CreativeWork,
       Thing
     ]
classReportageNewsArticle = Class "ReportageNewsArticle"

data ReportedDoseSchedule

classReportedDoseSchedule ::
  Class
    ReportedDoseSchedule
    '[ DoseSchedule,
       MedicalIntangible,
       MedicalEntity,
       Thing
     ]
classReportedDoseSchedule = Class "ReportedDoseSchedule"

data ResearchOrganization

classResearchOrganization ::
  Class ResearchOrganization '[Organization, Thing]
classResearchOrganization = Class "ResearchOrganization"

data ResearchProject

classResearchProject ::
  Class ResearchProject '[Project, Organization, Thing]
classResearchProject = Class "ResearchProject"

data Researcher

classResearcher :: Class Researcher '[Audience, Intangible, Thing]
classResearcher = Class "Researcher"

data Reservation

classReservation :: Class Reservation '[Intangible, Thing]
classReservation = Class "Reservation"

data ReservationPackage

classReservationPackage ::
  Class ReservationPackage '[Reservation, Intangible, Thing]
classReservationPackage = Class "ReservationPackage"

data ReservationStatusType
  = ReservationStatusTypeReservationCancelled
  | ReservationStatusTypeReservationConfirmed
  | ReservationStatusTypeReservationHold
  | ReservationStatusTypeReservationPending
  deriving (Show, Eq, Ord, Generic)

instance FromJSON ReservationStatusType where
  parseJSON =
    withText
      "ReservationStatusType"
      ( \case
          "https://schema.org/ReservationCancelled" ->
            pure ReservationStatusTypeReservationCancelled
          "https://schema.org/ReservationConfirmed" ->
            pure ReservationStatusTypeReservationConfirmed
          "https://schema.org/ReservationHold" ->
            pure ReservationStatusTypeReservationHold
          "https://schema.org/ReservationPending" ->
            pure ReservationStatusTypeReservationPending
          t -> fail ("Failed to parse ReservationStatusType: " <> show t)
      )

instance ToJSON ReservationStatusType where
  toJSON =
    (toJSON :: Text -> Value)
      . ( \case
            ReservationStatusTypeReservationCancelled ->
              "https://schema.org/ReservationCancelled"
            ReservationStatusTypeReservationConfirmed ->
              "https://schema.org/ReservationConfirmed"
            ReservationStatusTypeReservationHold ->
              "https://schema.org/ReservationHold"
            ReservationStatusTypeReservationPending ->
              "https://schema.org/ReservationPending"
        )

data ReserveAction

classReserveAction ::
  Class ReserveAction '[PlanAction, OrganizeAction, Action, Thing]
classReserveAction = Class "ReserveAction"

data Reservoir

classReservoir ::
  Class Reservoir '[BodyOfWater, Landform, Place, Thing]
classReservoir = Class "Reservoir"

data Residence

classResidence :: Class Residence '[Place, Thing]
classResidence = Class "Residence"

data Resort

classResort ::
  Class
    Resort
    '[ LodgingBusiness,
       LocalBusiness,
       Place,
       Organization,
       Thing,
       Thing
     ]
classResort = Class "Resort"

data Restaurant

classRestaurant ::
  Class
    Restaurant
    '[ FoodEstablishment,
       LocalBusiness,
       Place,
       Organization,
       Thing,
       Thing
     ]
classRestaurant = Class "Restaurant"

data RestrictedDiet
  = RestrictedDietDiabeticDiet
  | RestrictedDietGlutenFreeDiet
  | RestrictedDietHalalDiet
  | RestrictedDietHinduDiet
  | RestrictedDietKosherDiet
  | RestrictedDietLowCalorieDiet
  | RestrictedDietLowFatDiet
  | RestrictedDietLowLactoseDiet
  | RestrictedDietLowSaltDiet
  | RestrictedDietVeganDiet
  | RestrictedDietVegetarianDiet
  deriving (Show, Eq, Ord, Generic)

instance FromJSON RestrictedDiet where
  parseJSON =
    withText
      "RestrictedDiet"
      ( \case
          "https://schema.org/DiabeticDiet" ->
            pure RestrictedDietDiabeticDiet
          "https://schema.org/GlutenFreeDiet" ->
            pure RestrictedDietGlutenFreeDiet
          "https://schema.org/HalalDiet" -> pure RestrictedDietHalalDiet
          "https://schema.org/HinduDiet" -> pure RestrictedDietHinduDiet
          "https://schema.org/KosherDiet" -> pure RestrictedDietKosherDiet
          "https://schema.org/LowCalorieDiet" ->
            pure RestrictedDietLowCalorieDiet
          "https://schema.org/LowFatDiet" -> pure RestrictedDietLowFatDiet
          "https://schema.org/LowLactoseDiet" ->
            pure RestrictedDietLowLactoseDiet
          "https://schema.org/LowSaltDiet" -> pure RestrictedDietLowSaltDiet
          "https://schema.org/VeganDiet" -> pure RestrictedDietVeganDiet
          "https://schema.org/VegetarianDiet" ->
            pure RestrictedDietVegetarianDiet
          t -> fail ("Failed to parse RestrictedDiet: " <> show t)
      )

instance ToJSON RestrictedDiet where
  toJSON =
    (toJSON :: Text -> Value)
      . ( \case
            RestrictedDietDiabeticDiet -> "https://schema.org/DiabeticDiet"
            RestrictedDietGlutenFreeDiet -> "https://schema.org/GlutenFreeDiet"
            RestrictedDietHalalDiet -> "https://schema.org/HalalDiet"
            RestrictedDietHinduDiet -> "https://schema.org/HinduDiet"
            RestrictedDietKosherDiet -> "https://schema.org/KosherDiet"
            RestrictedDietLowCalorieDiet -> "https://schema.org/LowCalorieDiet"
            RestrictedDietLowFatDiet -> "https://schema.org/LowFatDiet"
            RestrictedDietLowLactoseDiet -> "https://schema.org/LowLactoseDiet"
            RestrictedDietLowSaltDiet -> "https://schema.org/LowSaltDiet"
            RestrictedDietVeganDiet -> "https://schema.org/VeganDiet"
            RestrictedDietVegetarianDiet ->
              "https://schema.org/VegetarianDiet"
        )

data ResumeAction

classResumeAction ::
  Class ResumeAction '[ControlAction, Action, Thing]
classResumeAction = Class "ResumeAction"

data ReturnAction

classReturnAction ::
  Class ReturnAction '[TransferAction, Action, Thing]
classReturnAction = Class "ReturnAction"

data ReturnFeesEnumeration
  = ReturnFeesEnumerationFreeReturn
  | ReturnFeesEnumerationOriginalShippingFees
  | ReturnFeesEnumerationRestockingFees
  | ReturnFeesEnumerationReturnFeesCustomerResponsibility
  | ReturnFeesEnumerationReturnShippingFees
  deriving (Show, Eq, Ord, Generic)

instance FromJSON ReturnFeesEnumeration where
  parseJSON =
    withText
      "ReturnFeesEnumeration"
      ( \case
          "https://schema.org/FreeReturn" ->
            pure ReturnFeesEnumerationFreeReturn
          "https://schema.org/OriginalShippingFees" ->
            pure ReturnFeesEnumerationOriginalShippingFees
          "https://schema.org/RestockingFees" ->
            pure ReturnFeesEnumerationRestockingFees
          "https://schema.org/ReturnFeesCustomerResponsibility" ->
            pure ReturnFeesEnumerationReturnFeesCustomerResponsibility
          "https://schema.org/ReturnShippingFees" ->
            pure ReturnFeesEnumerationReturnShippingFees
          t -> fail ("Failed to parse ReturnFeesEnumeration: " <> show t)
      )

instance ToJSON ReturnFeesEnumeration where
  toJSON =
    (toJSON :: Text -> Value)
      . ( \case
            ReturnFeesEnumerationFreeReturn -> "https://schema.org/FreeReturn"
            ReturnFeesEnumerationOriginalShippingFees ->
              "https://schema.org/OriginalShippingFees"
            ReturnFeesEnumerationRestockingFees ->
              "https://schema.org/RestockingFees"
            ReturnFeesEnumerationReturnFeesCustomerResponsibility ->
              "https://schema.org/ReturnFeesCustomerResponsibility"
            ReturnFeesEnumerationReturnShippingFees ->
              "https://schema.org/ReturnShippingFees"
        )

data ReturnLabelSourceEnumeration
  = ReturnLabelSourceEnumerationReturnLabelCustomerResponsibility
  | ReturnLabelSourceEnumerationReturnLabelDownloadAndPrint
  | ReturnLabelSourceEnumerationReturnLabelInBox
  deriving (Show, Eq, Ord, Generic)

instance FromJSON ReturnLabelSourceEnumeration where
  parseJSON =
    withText
      "ReturnLabelSourceEnumeration"
      ( \case
          "https://schema.org/ReturnLabelCustomerResponsibility" ->
            pure
              ReturnLabelSourceEnumerationReturnLabelCustomerResponsibility
          "https://schema.org/ReturnLabelDownloadAndPrint" ->
            pure ReturnLabelSourceEnumerationReturnLabelDownloadAndPrint
          "https://schema.org/ReturnLabelInBox" ->
            pure ReturnLabelSourceEnumerationReturnLabelInBox
          t ->
            fail
              ("Failed to parse ReturnLabelSourceEnumeration: " <> show t)
      )

instance ToJSON ReturnLabelSourceEnumeration where
  toJSON =
    (toJSON :: Text -> Value)
      . ( \case
            ReturnLabelSourceEnumerationReturnLabelCustomerResponsibility ->
              "https://schema.org/ReturnLabelCustomerResponsibility"
            ReturnLabelSourceEnumerationReturnLabelDownloadAndPrint ->
              "https://schema.org/ReturnLabelDownloadAndPrint"
            ReturnLabelSourceEnumerationReturnLabelInBox ->
              "https://schema.org/ReturnLabelInBox"
        )

data ReturnMethodEnumeration
  = ReturnMethodEnumerationReturnAtKiosk
  | ReturnMethodEnumerationReturnByMail
  | ReturnMethodEnumerationReturnInStore
  deriving (Show, Eq, Ord, Generic)

instance FromJSON ReturnMethodEnumeration where
  parseJSON =
    withText
      "ReturnMethodEnumeration"
      ( \case
          "https://schema.org/ReturnAtKiosk" ->
            pure ReturnMethodEnumerationReturnAtKiosk
          "https://schema.org/ReturnByMail" ->
            pure ReturnMethodEnumerationReturnByMail
          "https://schema.org/ReturnInStore" ->
            pure ReturnMethodEnumerationReturnInStore
          t -> fail ("Failed to parse ReturnMethodEnumeration: " <> show t)
      )

instance ToJSON ReturnMethodEnumeration where
  toJSON =
    (toJSON :: Text -> Value)
      . ( \case
            ReturnMethodEnumerationReturnAtKiosk ->
              "https://schema.org/ReturnAtKiosk"
            ReturnMethodEnumerationReturnByMail ->
              "https://schema.org/ReturnByMail"
            ReturnMethodEnumerationReturnInStore ->
              "https://schema.org/ReturnInStore"
        )

data Review

classReview :: Class Review '[CreativeWork, Thing]
classReview = Class "Review"

data ReviewAction

classReviewAction ::
  Class ReviewAction '[AssessAction, Action, Thing]
classReviewAction = Class "ReviewAction"

data ReviewNewsArticle

classReviewNewsArticle ::
  Class
    ReviewNewsArticle
    '[ NewsArticle,
       CriticReview,
       Article,
       CreativeWork,
       Thing,
       Review,
       CreativeWork,
       Thing
     ]
classReviewNewsArticle = Class "ReviewNewsArticle"

data RiverBodyOfWater

classRiverBodyOfWater ::
  Class RiverBodyOfWater '[BodyOfWater, Landform, Place, Thing]
classRiverBodyOfWater = Class "RiverBodyOfWater"

data Role

classRole :: Class Role '[Intangible, Thing]
classRole = Class "Role"

data RoofingContractor

classRoofingContractor ::
  Class
    RoofingContractor
    '[ HomeAndConstructionBusiness,
       LocalBusiness,
       Place,
       Organization,
       Thing,
       Thing
     ]
classRoofingContractor = Class "RoofingContractor"

data Room

classRoom :: Class Room '[Accommodation, Place, Thing]
classRoom = Class "Room"

data RsvpAction

classRsvpAction ::
  Class
    RsvpAction
    '[ InformAction,
       CommunicateAction,
       InteractAction,
       Action,
       Thing
     ]
classRsvpAction = Class "RsvpAction"

data RsvpResponseType
  = RsvpResponseTypeRsvpResponseMaybe
  | RsvpResponseTypeRsvpResponseNo
  | RsvpResponseTypeRsvpResponseYes
  deriving (Show, Eq, Ord, Generic)

instance FromJSON RsvpResponseType where
  parseJSON =
    withText
      "RsvpResponseType"
      ( \case
          "https://schema.org/RsvpResponseMaybe" ->
            pure RsvpResponseTypeRsvpResponseMaybe
          "https://schema.org/RsvpResponseNo" ->
            pure RsvpResponseTypeRsvpResponseNo
          "https://schema.org/RsvpResponseYes" ->
            pure RsvpResponseTypeRsvpResponseYes
          t -> fail ("Failed to parse RsvpResponseType: " <> show t)
      )

instance ToJSON RsvpResponseType where
  toJSON =
    (toJSON :: Text -> Value)
      . ( \case
            RsvpResponseTypeRsvpResponseMaybe ->
              "https://schema.org/RsvpResponseMaybe"
            RsvpResponseTypeRsvpResponseNo ->
              "https://schema.org/RsvpResponseNo"
            RsvpResponseTypeRsvpResponseYes ->
              "https://schema.org/RsvpResponseYes"
        )

data SaleEvent

classSaleEvent :: Class SaleEvent '[Event, Thing]
classSaleEvent = Class "SaleEvent"

data SatiricalArticle

classSatiricalArticle ::
  Class SatiricalArticle '[Article, CreativeWork, Thing]
classSatiricalArticle = Class "SatiricalArticle"

data Schedule

classSchedule :: Class Schedule '[Intangible, Thing]
classSchedule = Class "Schedule"

data ScheduleAction

classScheduleAction ::
  Class ScheduleAction '[PlanAction, OrganizeAction, Action, Thing]
classScheduleAction = Class "ScheduleAction"

data ScholarlyArticle

classScholarlyArticle ::
  Class ScholarlyArticle '[Article, CreativeWork, Thing]
classScholarlyArticle = Class "ScholarlyArticle"

data School

classSchool ::
  Class
    School
    '[ EducationalOrganization,
       CivicStructure,
       Organization,
       Place,
       Thing,
       Thing
     ]
classSchool = Class "School"

data SchoolDistrict

classSchoolDistrict ::
  Class SchoolDistrict '[AdministrativeArea, Place, Thing]
classSchoolDistrict = Class "SchoolDistrict"

data ScreeningEvent

classScreeningEvent :: Class ScreeningEvent '[Event, Thing]
classScreeningEvent = Class "ScreeningEvent"

data Sculpture

classSculpture :: Class Sculpture '[CreativeWork, Thing]
classSculpture = Class "Sculpture"

data SeaBodyOfWater

classSeaBodyOfWater ::
  Class SeaBodyOfWater '[BodyOfWater, Landform, Place, Thing]
classSeaBodyOfWater = Class "SeaBodyOfWater"

data SearchAction

classSearchAction :: Class SearchAction '[Action, Thing]
classSearchAction = Class "SearchAction"

data SearchRescueOrganization

classSearchRescueOrganization ::
  Class SearchRescueOrganization '[Organization, Thing]
classSearchRescueOrganization = Class "SearchRescueOrganization"

data SearchResultsPage

classSearchResultsPage ::
  Class SearchResultsPage '[WebPage, CreativeWork, Thing]
classSearchResultsPage = Class "SearchResultsPage"

data Season

classSeason :: Class Season '[CreativeWork, Thing]
classSeason = Class "Season"

data Seat

classSeat :: Class Seat '[Intangible, Thing]
classSeat = Class "Seat"

data SeekToAction

classSeekToAction :: Class SeekToAction '[Action, Thing]
classSeekToAction = Class "SeekToAction"

data SelfStorage

classSelfStorage ::
  Class
    SelfStorage
    '[ LocalBusiness,
       Place,
       Organization,
       Thing,
       Thing
     ]
classSelfStorage = Class "SelfStorage"

data SellAction

classSellAction :: Class SellAction '[TradeAction, Action, Thing]
classSellAction = Class "SellAction"

data SendAction

classSendAction ::
  Class SendAction '[TransferAction, Action, Thing]
classSendAction = Class "SendAction"

data Series

classSeries :: Class Series '[Intangible, Thing]
classSeries = Class "Series"

data Service

classService :: Class Service '[Intangible, Thing]
classService = Class "Service"

data ServiceChannel

classServiceChannel :: Class ServiceChannel '[Intangible, Thing]
classServiceChannel = Class "ServiceChannel"

data ShareAction

classShareAction ::
  Class
    ShareAction
    '[ CommunicateAction,
       InteractAction,
       Action,
       Thing
     ]
classShareAction = Class "ShareAction"

data SheetMusic

classSheetMusic :: Class SheetMusic '[CreativeWork, Thing]
classSheetMusic = Class "SheetMusic"

data ShippingDeliveryTime

classShippingDeliveryTime ::
  Class ShippingDeliveryTime '[StructuredValue, Intangible, Thing]
classShippingDeliveryTime = Class "ShippingDeliveryTime"

data ShippingRateSettings

classShippingRateSettings ::
  Class ShippingRateSettings '[StructuredValue, Intangible, Thing]
classShippingRateSettings = Class "ShippingRateSettings"

data ShoeStore

classShoeStore ::
  Class
    ShoeStore
    '[ Store,
       LocalBusiness,
       Place,
       Organization,
       Thing,
       Thing
     ]
classShoeStore = Class "ShoeStore"

data ShoppingCenter

classShoppingCenter ::
  Class
    ShoppingCenter
    '[ LocalBusiness,
       Place,
       Organization,
       Thing,
       Thing
     ]
classShoppingCenter = Class "ShoppingCenter"

data ShortStory

classShortStory :: Class ShortStory '[CreativeWork, Thing]
classShortStory = Class "ShortStory"

data SingleFamilyResidence

classSingleFamilyResidence ::
  Class SingleFamilyResidence '[House, Accommodation, Place, Thing]
classSingleFamilyResidence = Class "SingleFamilyResidence"

data SiteNavigationElement

classSiteNavigationElement ::
  Class SiteNavigationElement '[WebPageElement, CreativeWork, Thing]
classSiteNavigationElement = Class "SiteNavigationElement"

type SizeGroupEnumeration = Text

type SizeSpecification = Text

data SizeSystemEnumeration
  = SizeSystemEnumerationSizeSystemImperial
  | SizeSystemEnumerationSizeSystemMetric
  deriving (Show, Eq, Ord, Generic)

instance FromJSON SizeSystemEnumeration where
  parseJSON =
    withText
      "SizeSystemEnumeration"
      ( \case
          "https://schema.org/SizeSystemImperial" ->
            pure SizeSystemEnumerationSizeSystemImperial
          "https://schema.org/SizeSystemMetric" ->
            pure SizeSystemEnumerationSizeSystemMetric
          t -> fail ("Failed to parse SizeSystemEnumeration: " <> show t)
      )

instance ToJSON SizeSystemEnumeration where
  toJSON =
    (toJSON :: Text -> Value)
      . ( \case
            SizeSystemEnumerationSizeSystemImperial ->
              "https://schema.org/SizeSystemImperial"
            SizeSystemEnumerationSizeSystemMetric ->
              "https://schema.org/SizeSystemMetric"
        )

data SkiResort

classSkiResort ::
  Class
    SkiResort
    '[ SportsActivityLocation,
       Resort,
       LocalBusiness,
       Place,
       Organization,
       Thing,
       Thing,
       LodgingBusiness,
       LocalBusiness,
       Place,
       Organization,
       Thing,
       Thing
     ]
classSkiResort = Class "SkiResort"

data SocialEvent

classSocialEvent :: Class SocialEvent '[Event, Thing]
classSocialEvent = Class "SocialEvent"

data SocialMediaPosting

classSocialMediaPosting ::
  Class SocialMediaPosting '[Article, CreativeWork, Thing]
classSocialMediaPosting = Class "SocialMediaPosting"

data SoftwareApplication

classSoftwareApplication ::
  Class SoftwareApplication '[CreativeWork, Thing]
classSoftwareApplication = Class "SoftwareApplication"

data SoftwareSourceCode

classSoftwareSourceCode ::
  Class SoftwareSourceCode '[CreativeWork, Thing]
classSoftwareSourceCode = Class "SoftwareSourceCode"

data SolveMathAction

classSolveMathAction :: Class SolveMathAction '[Action, Thing]
classSolveMathAction = Class "SolveMathAction"

data SomeProducts

classSomeProducts :: Class SomeProducts '[Product, Thing]
classSomeProducts = Class "SomeProducts"

data SpeakableSpecification

classSpeakableSpecification ::
  Class SpeakableSpecification '[Intangible, Thing]
classSpeakableSpecification = Class "SpeakableSpecification"

data SpecialAnnouncement

classSpecialAnnouncement ::
  Class SpecialAnnouncement '[CreativeWork, Thing]
classSpecialAnnouncement = Class "SpecialAnnouncement"

type Specialty = Text

data SportingGoodsStore

classSportingGoodsStore ::
  Class
    SportingGoodsStore
    '[ Store,
       LocalBusiness,
       Place,
       Organization,
       Thing,
       Thing
     ]
classSportingGoodsStore = Class "SportingGoodsStore"

data SportsActivityLocation

classSportsActivityLocation ::
  Class
    SportsActivityLocation
    '[ LocalBusiness,
       Place,
       Organization,
       Thing,
       Thing
     ]
classSportsActivityLocation = Class "SportsActivityLocation"

data SportsClub

classSportsClub ::
  Class
    SportsClub
    '[ SportsActivityLocation,
       LocalBusiness,
       Place,
       Organization,
       Thing,
       Thing
     ]
classSportsClub = Class "SportsClub"

data SportsEvent

classSportsEvent :: Class SportsEvent '[Event, Thing]
classSportsEvent = Class "SportsEvent"

data SportsOrganization

classSportsOrganization ::
  Class SportsOrganization '[Organization, Thing]
classSportsOrganization = Class "SportsOrganization"

data SportsTeam

classSportsTeam ::
  Class SportsTeam '[SportsOrganization, Organization, Thing]
classSportsTeam = Class "SportsTeam"

data SpreadsheetDigitalDocument

classSpreadsheetDigitalDocument ::
  Class
    SpreadsheetDigitalDocument
    '[ DigitalDocument,
       CreativeWork,
       Thing
     ]
classSpreadsheetDigitalDocument =
  Class "SpreadsheetDigitalDocument"

data StadiumOrArena

classStadiumOrArena ::
  Class
    StadiumOrArena
    '[ SportsActivityLocation,
       CivicStructure,
       LocalBusiness,
       Place,
       Organization,
       Thing,
       Thing,
       Place,
       Thing
     ]
classStadiumOrArena = Class "StadiumOrArena"

data State

classState :: Class State '[AdministrativeArea, Place, Thing]
classState = Class "State"

data Statement

classStatement :: Class Statement '[CreativeWork, Thing]
classStatement = Class "Statement"

data StatisticalPopulation

classStatisticalPopulation ::
  Class StatisticalPopulation '[Intangible, Thing]
classStatisticalPopulation = Class "StatisticalPopulation"

type StatusEnumeration = Text

data SteeringPositionValue
  = SteeringPositionValueLeftHandDriving
  | SteeringPositionValueRightHandDriving
  deriving (Show, Eq, Ord, Generic)

instance FromJSON SteeringPositionValue where
  parseJSON =
    withText
      "SteeringPositionValue"
      ( \case
          "https://schema.org/LeftHandDriving" ->
            pure SteeringPositionValueLeftHandDriving
          "https://schema.org/RightHandDriving" ->
            pure SteeringPositionValueRightHandDriving
          t -> fail ("Failed to parse SteeringPositionValue: " <> show t)
      )

instance ToJSON SteeringPositionValue where
  toJSON =
    (toJSON :: Text -> Value)
      . ( \case
            SteeringPositionValueLeftHandDriving ->
              "https://schema.org/LeftHandDriving"
            SteeringPositionValueRightHandDriving ->
              "https://schema.org/RightHandDriving"
        )

data Store

classStore ::
  Class Store '[LocalBusiness, Place, Organization, Thing, Thing]
classStore = Class "Store"

data StructuredValue

classStructuredValue :: Class StructuredValue '[Intangible, Thing]
classStructuredValue = Class "StructuredValue"

data SubscribeAction

classSubscribeAction ::
  Class SubscribeAction '[InteractAction, Action, Thing]
classSubscribeAction = Class "SubscribeAction"

data Substance

classSubstance :: Class Substance '[MedicalEntity, Thing]
classSubstance = Class "Substance"

data SubwayStation

classSubwayStation ::
  Class SubwayStation '[CivicStructure, Place, Thing]
classSubwayStation = Class "SubwayStation"

data Suite

classSuite :: Class Suite '[Accommodation, Place, Thing]
classSuite = Class "Suite"

data SuperficialAnatomy

classSuperficialAnatomy ::
  Class SuperficialAnatomy '[MedicalEntity, Thing]
classSuperficialAnatomy = Class "SuperficialAnatomy"

data SurgicalProcedure

classSurgicalProcedure ::
  Class SurgicalProcedure '[MedicalProcedure, MedicalEntity, Thing]
classSurgicalProcedure = Class "SurgicalProcedure"

data SuspendAction

classSuspendAction ::
  Class SuspendAction '[ControlAction, Action, Thing]
classSuspendAction = Class "SuspendAction"

data Synagogue

classSynagogue ::
  Class Synagogue '[PlaceOfWorship, CivicStructure, Place, Thing]
classSynagogue = Class "Synagogue"

data TVClip

classTVClip :: Class TVClip '[Clip, CreativeWork, Thing]
classTVClip = Class "TVClip"

data TVEpisode

classTVEpisode :: Class TVEpisode '[Episode, CreativeWork, Thing]
classTVEpisode = Class "TVEpisode"

data TVSeason

classTVSeason ::
  Class
    TVSeason
    '[ CreativeWorkSeason,
       CreativeWork,
       CreativeWork,
       Thing,
       Thing
     ]
classTVSeason = Class "TVSeason"

data TVSeries

classTVSeries ::
  Class
    TVSeries
    '[ CreativeWork,
       CreativeWorkSeries,
       Thing,
       Series,
       CreativeWork,
       Intangible,
       Thing,
       Thing
     ]
classTVSeries = Class "TVSeries"

data Table

classTable :: Class Table '[WebPageElement, CreativeWork, Thing]
classTable = Class "Table"

data TakeAction

classTakeAction ::
  Class TakeAction '[TransferAction, Action, Thing]
classTakeAction = Class "TakeAction"

data TattooParlor

classTattooParlor ::
  Class
    TattooParlor
    '[ HealthAndBeautyBusiness,
       LocalBusiness,
       Place,
       Organization,
       Thing,
       Thing
     ]
classTattooParlor = Class "TattooParlor"

data Taxi

classTaxi :: Class Taxi '[Service, Intangible, Thing]
classTaxi = Class "Taxi"

data TaxiReservation

classTaxiReservation ::
  Class TaxiReservation '[Reservation, Intangible, Thing]
classTaxiReservation = Class "TaxiReservation"

data TaxiService

classTaxiService :: Class TaxiService '[Service, Intangible, Thing]
classTaxiService = Class "TaxiService"

data TaxiStand

classTaxiStand :: Class TaxiStand '[CivicStructure, Place, Thing]
classTaxiStand = Class "TaxiStand"

data Taxon

classTaxon :: Class Taxon '[Thing]
classTaxon = Class "Taxon"

data TechArticle

classTechArticle ::
  Class TechArticle '[Article, CreativeWork, Thing]
classTechArticle = Class "TechArticle"

data TelevisionChannel

classTelevisionChannel ::
  Class TelevisionChannel '[BroadcastChannel, Intangible, Thing]
classTelevisionChannel = Class "TelevisionChannel"

data TelevisionStation

classTelevisionStation ::
  Class
    TelevisionStation
    '[ LocalBusiness,
       Place,
       Organization,
       Thing,
       Thing
     ]
classTelevisionStation = Class "TelevisionStation"

data TennisComplex

classTennisComplex ::
  Class
    TennisComplex
    '[ SportsActivityLocation,
       LocalBusiness,
       Place,
       Organization,
       Thing,
       Thing
     ]
classTennisComplex = Class "TennisComplex"

data TextDigitalDocument

classTextDigitalDocument ::
  Class TextDigitalDocument '[DigitalDocument, CreativeWork, Thing]
classTextDigitalDocument = Class "TextDigitalDocument"

data TheaterEvent

classTheaterEvent :: Class TheaterEvent '[Event, Thing]
classTheaterEvent = Class "TheaterEvent"

data TheaterGroup

classTheaterGroup ::
  Class TheaterGroup '[PerformingGroup, Organization, Thing]
classTheaterGroup = Class "TheaterGroup"

data TherapeuticProcedure

classTherapeuticProcedure ::
  Class
    TherapeuticProcedure
    '[ MedicalProcedure,
       MedicalEntity,
       Thing
     ]
classTherapeuticProcedure = Class "TherapeuticProcedure"

data Thesis

classThesis :: Class Thesis '[CreativeWork, Thing]
classThesis = Class "Thesis"

data Thing

classThing :: Class Thing '[]
classThing = Class "Thing"

data Ticket

classTicket :: Class Ticket '[Intangible, Thing]
classTicket = Class "Ticket"

data TieAction

classTieAction :: Class TieAction '[AchieveAction, Action, Thing]
classTieAction = Class "TieAction"

data TipAction

classTipAction :: Class TipAction '[TradeAction, Action, Thing]
classTipAction = Class "TipAction"

data TireShop

classTireShop ::
  Class
    TireShop
    '[ Store,
       LocalBusiness,
       Place,
       Organization,
       Thing,
       Thing
     ]
classTireShop = Class "TireShop"

data TouristAttraction

classTouristAttraction :: Class TouristAttraction '[Place, Thing]
classTouristAttraction = Class "TouristAttraction"

data TouristDestination

classTouristDestination :: Class TouristDestination '[Place, Thing]
classTouristDestination = Class "TouristDestination"

data TouristInformationCenter

classTouristInformationCenter ::
  Class
    TouristInformationCenter
    '[ LocalBusiness,
       Place,
       Organization,
       Thing,
       Thing
     ]
classTouristInformationCenter = Class "TouristInformationCenter"

data TouristTrip

classTouristTrip :: Class TouristTrip '[Trip, Intangible, Thing]
classTouristTrip = Class "TouristTrip"

data ToyStore

classToyStore ::
  Class
    ToyStore
    '[ Store,
       LocalBusiness,
       Place,
       Organization,
       Thing,
       Thing
     ]
classToyStore = Class "ToyStore"

data TrackAction

classTrackAction :: Class TrackAction '[FindAction, Action, Thing]
classTrackAction = Class "TrackAction"

data TradeAction

classTradeAction :: Class TradeAction '[Action, Thing]
classTradeAction = Class "TradeAction"

data TrainReservation

classTrainReservation ::
  Class TrainReservation '[Reservation, Intangible, Thing]
classTrainReservation = Class "TrainReservation"

data TrainStation

classTrainStation ::
  Class TrainStation '[CivicStructure, Place, Thing]
classTrainStation = Class "TrainStation"

data TrainTrip

classTrainTrip :: Class TrainTrip '[Trip, Intangible, Thing]
classTrainTrip = Class "TrainTrip"

data TransferAction

classTransferAction :: Class TransferAction '[Action, Thing]
classTransferAction = Class "TransferAction"

data TravelAction

classTravelAction ::
  Class TravelAction '[MoveAction, Action, Thing]
classTravelAction = Class "TravelAction"

data TravelAgency

classTravelAgency ::
  Class
    TravelAgency
    '[ LocalBusiness,
       Place,
       Organization,
       Thing,
       Thing
     ]
classTravelAgency = Class "TravelAgency"

data TreatmentIndication

classTreatmentIndication ::
  Class
    TreatmentIndication
    '[ MedicalIndication,
       MedicalEntity,
       Thing
     ]
classTreatmentIndication = Class "TreatmentIndication"

data Trip

classTrip :: Class Trip '[Intangible, Thing]
classTrip = Class "Trip"

data TypeAndQuantityNode

classTypeAndQuantityNode ::
  Class TypeAndQuantityNode '[StructuredValue, Intangible, Thing]
classTypeAndQuantityNode = Class "TypeAndQuantityNode"

data UKNonprofitType
  = UKNonprofitTypeCharitableIncorporatedOrganization
  | UKNonprofitTypeLimitedByGuaranteeCharity
  | UKNonprofitTypeUKTrust
  | UKNonprofitTypeUnincorporatedAssociationCharity
  deriving (Show, Eq, Ord, Generic)

instance FromJSON UKNonprofitType where
  parseJSON =
    withText
      "UKNonprofitType"
      ( \case
          "https://schema.org/CharitableIncorporatedOrganization" ->
            pure UKNonprofitTypeCharitableIncorporatedOrganization
          "https://schema.org/LimitedByGuaranteeCharity" ->
            pure UKNonprofitTypeLimitedByGuaranteeCharity
          "https://schema.org/UKTrust" -> pure UKNonprofitTypeUKTrust
          "https://schema.org/UnincorporatedAssociationCharity" ->
            pure UKNonprofitTypeUnincorporatedAssociationCharity
          t -> fail ("Failed to parse UKNonprofitType: " <> show t)
      )

instance ToJSON UKNonprofitType where
  toJSON =
    (toJSON :: Text -> Value)
      . ( \case
            UKNonprofitTypeCharitableIncorporatedOrganization ->
              "https://schema.org/CharitableIncorporatedOrganization"
            UKNonprofitTypeLimitedByGuaranteeCharity ->
              "https://schema.org/LimitedByGuaranteeCharity"
            UKNonprofitTypeUKTrust -> "https://schema.org/UKTrust"
            UKNonprofitTypeUnincorporatedAssociationCharity ->
              "https://schema.org/UnincorporatedAssociationCharity"
        )

type URL = Text

data USNonprofitType
  = USNonprofitTypeNonprofit501a
  | USNonprofitTypeNonprofit501c1
  | USNonprofitTypeNonprofit501c10
  | USNonprofitTypeNonprofit501c11
  | USNonprofitTypeNonprofit501c12
  | USNonprofitTypeNonprofit501c13
  | USNonprofitTypeNonprofit501c14
  | USNonprofitTypeNonprofit501c15
  | USNonprofitTypeNonprofit501c16
  | USNonprofitTypeNonprofit501c17
  | USNonprofitTypeNonprofit501c18
  | USNonprofitTypeNonprofit501c19
  | USNonprofitTypeNonprofit501c2
  | USNonprofitTypeNonprofit501c20
  | USNonprofitTypeNonprofit501c21
  | USNonprofitTypeNonprofit501c22
  | USNonprofitTypeNonprofit501c23
  | USNonprofitTypeNonprofit501c24
  | USNonprofitTypeNonprofit501c25
  | USNonprofitTypeNonprofit501c26
  | USNonprofitTypeNonprofit501c27
  | USNonprofitTypeNonprofit501c28
  | USNonprofitTypeNonprofit501c3
  | USNonprofitTypeNonprofit501c4
  | USNonprofitTypeNonprofit501c5
  | USNonprofitTypeNonprofit501c6
  | USNonprofitTypeNonprofit501c7
  | USNonprofitTypeNonprofit501c8
  | USNonprofitTypeNonprofit501c9
  | USNonprofitTypeNonprofit501d
  | USNonprofitTypeNonprofit501e
  | USNonprofitTypeNonprofit501f
  | USNonprofitTypeNonprofit501k
  | USNonprofitTypeNonprofit501n
  | USNonprofitTypeNonprofit501q
  | USNonprofitTypeNonprofit527
  deriving (Show, Eq, Ord, Generic)

instance FromJSON USNonprofitType where
  parseJSON =
    withText
      "USNonprofitType"
      ( \case
          "https://schema.org/Nonprofit501a" ->
            pure USNonprofitTypeNonprofit501a
          "https://schema.org/Nonprofit501c1" ->
            pure USNonprofitTypeNonprofit501c1
          "https://schema.org/Nonprofit501c10" ->
            pure USNonprofitTypeNonprofit501c10
          "https://schema.org/Nonprofit501c11" ->
            pure USNonprofitTypeNonprofit501c11
          "https://schema.org/Nonprofit501c12" ->
            pure USNonprofitTypeNonprofit501c12
          "https://schema.org/Nonprofit501c13" ->
            pure USNonprofitTypeNonprofit501c13
          "https://schema.org/Nonprofit501c14" ->
            pure USNonprofitTypeNonprofit501c14
          "https://schema.org/Nonprofit501c15" ->
            pure USNonprofitTypeNonprofit501c15
          "https://schema.org/Nonprofit501c16" ->
            pure USNonprofitTypeNonprofit501c16
          "https://schema.org/Nonprofit501c17" ->
            pure USNonprofitTypeNonprofit501c17
          "https://schema.org/Nonprofit501c18" ->
            pure USNonprofitTypeNonprofit501c18
          "https://schema.org/Nonprofit501c19" ->
            pure USNonprofitTypeNonprofit501c19
          "https://schema.org/Nonprofit501c2" ->
            pure USNonprofitTypeNonprofit501c2
          "https://schema.org/Nonprofit501c20" ->
            pure USNonprofitTypeNonprofit501c20
          "https://schema.org/Nonprofit501c21" ->
            pure USNonprofitTypeNonprofit501c21
          "https://schema.org/Nonprofit501c22" ->
            pure USNonprofitTypeNonprofit501c22
          "https://schema.org/Nonprofit501c23" ->
            pure USNonprofitTypeNonprofit501c23
          "https://schema.org/Nonprofit501c24" ->
            pure USNonprofitTypeNonprofit501c24
          "https://schema.org/Nonprofit501c25" ->
            pure USNonprofitTypeNonprofit501c25
          "https://schema.org/Nonprofit501c26" ->
            pure USNonprofitTypeNonprofit501c26
          "https://schema.org/Nonprofit501c27" ->
            pure USNonprofitTypeNonprofit501c27
          "https://schema.org/Nonprofit501c28" ->
            pure USNonprofitTypeNonprofit501c28
          "https://schema.org/Nonprofit501c3" ->
            pure USNonprofitTypeNonprofit501c3
          "https://schema.org/Nonprofit501c4" ->
            pure USNonprofitTypeNonprofit501c4
          "https://schema.org/Nonprofit501c5" ->
            pure USNonprofitTypeNonprofit501c5
          "https://schema.org/Nonprofit501c6" ->
            pure USNonprofitTypeNonprofit501c6
          "https://schema.org/Nonprofit501c7" ->
            pure USNonprofitTypeNonprofit501c7
          "https://schema.org/Nonprofit501c8" ->
            pure USNonprofitTypeNonprofit501c8
          "https://schema.org/Nonprofit501c9" ->
            pure USNonprofitTypeNonprofit501c9
          "https://schema.org/Nonprofit501d" ->
            pure USNonprofitTypeNonprofit501d
          "https://schema.org/Nonprofit501e" ->
            pure USNonprofitTypeNonprofit501e
          "https://schema.org/Nonprofit501f" ->
            pure USNonprofitTypeNonprofit501f
          "https://schema.org/Nonprofit501k" ->
            pure USNonprofitTypeNonprofit501k
          "https://schema.org/Nonprofit501n" ->
            pure USNonprofitTypeNonprofit501n
          "https://schema.org/Nonprofit501q" ->
            pure USNonprofitTypeNonprofit501q
          "https://schema.org/Nonprofit527" ->
            pure USNonprofitTypeNonprofit527
          t -> fail ("Failed to parse USNonprofitType: " <> show t)
      )

instance ToJSON USNonprofitType where
  toJSON =
    (toJSON :: Text -> Value)
      . ( \case
            USNonprofitTypeNonprofit501a -> "https://schema.org/Nonprofit501a"
            USNonprofitTypeNonprofit501c1 ->
              "https://schema.org/Nonprofit501c1"
            USNonprofitTypeNonprofit501c10 ->
              "https://schema.org/Nonprofit501c10"
            USNonprofitTypeNonprofit501c11 ->
              "https://schema.org/Nonprofit501c11"
            USNonprofitTypeNonprofit501c12 ->
              "https://schema.org/Nonprofit501c12"
            USNonprofitTypeNonprofit501c13 ->
              "https://schema.org/Nonprofit501c13"
            USNonprofitTypeNonprofit501c14 ->
              "https://schema.org/Nonprofit501c14"
            USNonprofitTypeNonprofit501c15 ->
              "https://schema.org/Nonprofit501c15"
            USNonprofitTypeNonprofit501c16 ->
              "https://schema.org/Nonprofit501c16"
            USNonprofitTypeNonprofit501c17 ->
              "https://schema.org/Nonprofit501c17"
            USNonprofitTypeNonprofit501c18 ->
              "https://schema.org/Nonprofit501c18"
            USNonprofitTypeNonprofit501c19 ->
              "https://schema.org/Nonprofit501c19"
            USNonprofitTypeNonprofit501c2 ->
              "https://schema.org/Nonprofit501c2"
            USNonprofitTypeNonprofit501c20 ->
              "https://schema.org/Nonprofit501c20"
            USNonprofitTypeNonprofit501c21 ->
              "https://schema.org/Nonprofit501c21"
            USNonprofitTypeNonprofit501c22 ->
              "https://schema.org/Nonprofit501c22"
            USNonprofitTypeNonprofit501c23 ->
              "https://schema.org/Nonprofit501c23"
            USNonprofitTypeNonprofit501c24 ->
              "https://schema.org/Nonprofit501c24"
            USNonprofitTypeNonprofit501c25 ->
              "https://schema.org/Nonprofit501c25"
            USNonprofitTypeNonprofit501c26 ->
              "https://schema.org/Nonprofit501c26"
            USNonprofitTypeNonprofit501c27 ->
              "https://schema.org/Nonprofit501c27"
            USNonprofitTypeNonprofit501c28 ->
              "https://schema.org/Nonprofit501c28"
            USNonprofitTypeNonprofit501c3 ->
              "https://schema.org/Nonprofit501c3"
            USNonprofitTypeNonprofit501c4 ->
              "https://schema.org/Nonprofit501c4"
            USNonprofitTypeNonprofit501c5 ->
              "https://schema.org/Nonprofit501c5"
            USNonprofitTypeNonprofit501c6 ->
              "https://schema.org/Nonprofit501c6"
            USNonprofitTypeNonprofit501c7 ->
              "https://schema.org/Nonprofit501c7"
            USNonprofitTypeNonprofit501c8 ->
              "https://schema.org/Nonprofit501c8"
            USNonprofitTypeNonprofit501c9 ->
              "https://schema.org/Nonprofit501c9"
            USNonprofitTypeNonprofit501d -> "https://schema.org/Nonprofit501d"
            USNonprofitTypeNonprofit501e -> "https://schema.org/Nonprofit501e"
            USNonprofitTypeNonprofit501f -> "https://schema.org/Nonprofit501f"
            USNonprofitTypeNonprofit501k -> "https://schema.org/Nonprofit501k"
            USNonprofitTypeNonprofit501n -> "https://schema.org/Nonprofit501n"
            USNonprofitTypeNonprofit501q -> "https://schema.org/Nonprofit501q"
            USNonprofitTypeNonprofit527 -> "https://schema.org/Nonprofit527"
        )

data UnRegisterAction

classUnRegisterAction ::
  Class UnRegisterAction '[InteractAction, Action, Thing]
classUnRegisterAction = Class "UnRegisterAction"

data UnitPriceSpecification

classUnitPriceSpecification ::
  Class
    UnitPriceSpecification
    '[ PriceSpecification,
       StructuredValue,
       Intangible,
       Thing
     ]
classUnitPriceSpecification = Class "UnitPriceSpecification"

data UpdateAction

classUpdateAction :: Class UpdateAction '[Action, Thing]
classUpdateAction = Class "UpdateAction"

data UseAction

classUseAction :: Class UseAction '[ConsumeAction, Action, Thing]
classUseAction = Class "UseAction"

data UserBlocks

classUserBlocks ::
  Class UserBlocks '[UserInteraction, Event, Thing]
classUserBlocks = Class "UserBlocks"

data UserCheckins

classUserCheckins ::
  Class UserCheckins '[UserInteraction, Event, Thing]
classUserCheckins = Class "UserCheckins"

data UserComments

classUserComments ::
  Class UserComments '[UserInteraction, Event, Thing]
classUserComments = Class "UserComments"

data UserDownloads

classUserDownloads ::
  Class UserDownloads '[UserInteraction, Event, Thing]
classUserDownloads = Class "UserDownloads"

data UserInteraction

classUserInteraction :: Class UserInteraction '[Event, Thing]
classUserInteraction = Class "UserInteraction"

data UserLikes

classUserLikes :: Class UserLikes '[UserInteraction, Event, Thing]
classUserLikes = Class "UserLikes"

data UserPageVisits

classUserPageVisits ::
  Class UserPageVisits '[UserInteraction, Event, Thing]
classUserPageVisits = Class "UserPageVisits"

data UserPlays

classUserPlays :: Class UserPlays '[UserInteraction, Event, Thing]
classUserPlays = Class "UserPlays"

data UserPlusOnes

classUserPlusOnes ::
  Class UserPlusOnes '[UserInteraction, Event, Thing]
classUserPlusOnes = Class "UserPlusOnes"

data UserReview

classUserReview :: Class UserReview '[Review, CreativeWork, Thing]
classUserReview = Class "UserReview"

data UserTweets

classUserTweets ::
  Class UserTweets '[UserInteraction, Event, Thing]
classUserTweets = Class "UserTweets"

data Vehicle

classVehicle :: Class Vehicle '[Product, Thing]
classVehicle = Class "Vehicle"

data Vein

classVein ::
  Class Vein '[Vessel, AnatomicalStructure, MedicalEntity, Thing]
classVein = Class "Vein"

data Vessel

classVessel ::
  Class Vessel '[AnatomicalStructure, MedicalEntity, Thing]
classVessel = Class "Vessel"

data VeterinaryCare

classVeterinaryCare ::
  Class VeterinaryCare '[MedicalOrganization, Organization, Thing]
classVeterinaryCare = Class "VeterinaryCare"

data VideoGallery

classVideoGallery ::
  Class
    VideoGallery
    '[ MediaGallery,
       CollectionPage,
       WebPage,
       CreativeWork,
       Thing
     ]
classVideoGallery = Class "VideoGallery"

data VideoGame

classVideoGame ::
  Class
    VideoGame
    '[ SoftwareApplication,
       Game,
       CreativeWork,
       Thing,
       CreativeWork,
       Thing
     ]
classVideoGame = Class "VideoGame"

data VideoGameClip

classVideoGameClip ::
  Class VideoGameClip '[Clip, CreativeWork, Thing]
classVideoGameClip = Class "VideoGameClip"

data VideoGameSeries

classVideoGameSeries ::
  Class
    VideoGameSeries
    '[ CreativeWorkSeries,
       Series,
       CreativeWork,
       Intangible,
       Thing,
       Thing
     ]
classVideoGameSeries = Class "VideoGameSeries"

data VideoObject

classVideoObject ::
  Class VideoObject '[MediaObject, CreativeWork, Thing]
classVideoObject = Class "VideoObject"

data VideoObjectSnapshot

classVideoObjectSnapshot ::
  Class
    VideoObjectSnapshot
    '[ VideoObject,
       MediaObject,
       CreativeWork,
       Thing
     ]
classVideoObjectSnapshot = Class "VideoObjectSnapshot"

data ViewAction

classViewAction :: Class ViewAction '[ConsumeAction, Action, Thing]
classViewAction = Class "ViewAction"

data VirtualLocation

classVirtualLocation :: Class VirtualLocation '[Intangible, Thing]
classVirtualLocation = Class "VirtualLocation"

data VisualArtsEvent

classVisualArtsEvent :: Class VisualArtsEvent '[Event, Thing]
classVisualArtsEvent = Class "VisualArtsEvent"

data VisualArtwork

classVisualArtwork :: Class VisualArtwork '[CreativeWork, Thing]
classVisualArtwork = Class "VisualArtwork"

data VitalSign

classVitalSign ::
  Class
    VitalSign
    '[ MedicalSign,
       MedicalSignOrSymptom,
       MedicalCondition,
       MedicalEntity,
       Thing
     ]
classVitalSign = Class "VitalSign"

data Volcano

classVolcano :: Class Volcano '[Landform, Place, Thing]
classVolcano = Class "Volcano"

data VoteAction

classVoteAction ::
  Class VoteAction '[ChooseAction, AssessAction, Action, Thing]
classVoteAction = Class "VoteAction"

data WPAdBlock

classWPAdBlock ::
  Class WPAdBlock '[WebPageElement, CreativeWork, Thing]
classWPAdBlock = Class "WPAdBlock"

data WPFooter

classWPFooter ::
  Class WPFooter '[WebPageElement, CreativeWork, Thing]
classWPFooter = Class "WPFooter"

data WPHeader

classWPHeader ::
  Class WPHeader '[WebPageElement, CreativeWork, Thing]
classWPHeader = Class "WPHeader"

data WPSideBar

classWPSideBar ::
  Class WPSideBar '[WebPageElement, CreativeWork, Thing]
classWPSideBar = Class "WPSideBar"

data WantAction

classWantAction ::
  Class WantAction '[ReactAction, AssessAction, Action, Thing]
classWantAction = Class "WantAction"

data WarrantyPromise

classWarrantyPromise ::
  Class WarrantyPromise '[StructuredValue, Intangible, Thing]
classWarrantyPromise = Class "WarrantyPromise"

type WarrantyScope = Text

data WatchAction

classWatchAction ::
  Class WatchAction '[ConsumeAction, Action, Thing]
classWatchAction = Class "WatchAction"

data Waterfall

classWaterfall ::
  Class Waterfall '[BodyOfWater, Landform, Place, Thing]
classWaterfall = Class "Waterfall"

data WearAction

classWearAction ::
  Class WearAction '[UseAction, ConsumeAction, Action, Thing]
classWearAction = Class "WearAction"

data WearableMeasurementTypeEnumeration
  = WearableMeasurementTypeEnumerationWearableMeasurementBack
  | WearableMeasurementTypeEnumerationWearableMeasurementChestOrBust
  | WearableMeasurementTypeEnumerationWearableMeasurementCollar
  | WearableMeasurementTypeEnumerationWearableMeasurementCup
  | WearableMeasurementTypeEnumerationWearableMeasurementHeight
  | WearableMeasurementTypeEnumerationWearableMeasurementHips
  | WearableMeasurementTypeEnumerationWearableMeasurementInseam
  | WearableMeasurementTypeEnumerationWearableMeasurementLength
  | WearableMeasurementTypeEnumerationWearableMeasurementOutsideLeg
  | WearableMeasurementTypeEnumerationWearableMeasurementSleeve
  | WearableMeasurementTypeEnumerationWearableMeasurementWaist
  | WearableMeasurementTypeEnumerationWearableMeasurementWidth
  deriving (Show, Eq, Ord, Generic)

instance FromJSON WearableMeasurementTypeEnumeration where
  parseJSON =
    withText
      "WearableMeasurementTypeEnumeration"
      ( \case
          "https://schema.org/WearableMeasurementBack" ->
            pure WearableMeasurementTypeEnumerationWearableMeasurementBack
          "https://schema.org/WearableMeasurementChestOrBust" ->
            pure
              WearableMeasurementTypeEnumerationWearableMeasurementChestOrBust
          "https://schema.org/WearableMeasurementCollar" ->
            pure WearableMeasurementTypeEnumerationWearableMeasurementCollar
          "https://schema.org/WearableMeasurementCup" ->
            pure WearableMeasurementTypeEnumerationWearableMeasurementCup
          "https://schema.org/WearableMeasurementHeight" ->
            pure WearableMeasurementTypeEnumerationWearableMeasurementHeight
          "https://schema.org/WearableMeasurementHips" ->
            pure WearableMeasurementTypeEnumerationWearableMeasurementHips
          "https://schema.org/WearableMeasurementInseam" ->
            pure WearableMeasurementTypeEnumerationWearableMeasurementInseam
          "https://schema.org/WearableMeasurementLength" ->
            pure WearableMeasurementTypeEnumerationWearableMeasurementLength
          "https://schema.org/WearableMeasurementOutsideLeg" ->
            pure
              WearableMeasurementTypeEnumerationWearableMeasurementOutsideLeg
          "https://schema.org/WearableMeasurementSleeve" ->
            pure WearableMeasurementTypeEnumerationWearableMeasurementSleeve
          "https://schema.org/WearableMeasurementWaist" ->
            pure WearableMeasurementTypeEnumerationWearableMeasurementWaist
          "https://schema.org/WearableMeasurementWidth" ->
            pure WearableMeasurementTypeEnumerationWearableMeasurementWidth
          t ->
            fail
              ("Failed to parse WearableMeasurementTypeEnumeration: " <> show t)
      )

instance ToJSON WearableMeasurementTypeEnumeration where
  toJSON =
    (toJSON :: Text -> Value)
      . ( \case
            WearableMeasurementTypeEnumerationWearableMeasurementBack ->
              "https://schema.org/WearableMeasurementBack"
            WearableMeasurementTypeEnumerationWearableMeasurementChestOrBust ->
              "https://schema.org/WearableMeasurementChestOrBust"
            WearableMeasurementTypeEnumerationWearableMeasurementCollar ->
              "https://schema.org/WearableMeasurementCollar"
            WearableMeasurementTypeEnumerationWearableMeasurementCup ->
              "https://schema.org/WearableMeasurementCup"
            WearableMeasurementTypeEnumerationWearableMeasurementHeight ->
              "https://schema.org/WearableMeasurementHeight"
            WearableMeasurementTypeEnumerationWearableMeasurementHips ->
              "https://schema.org/WearableMeasurementHips"
            WearableMeasurementTypeEnumerationWearableMeasurementInseam ->
              "https://schema.org/WearableMeasurementInseam"
            WearableMeasurementTypeEnumerationWearableMeasurementLength ->
              "https://schema.org/WearableMeasurementLength"
            WearableMeasurementTypeEnumerationWearableMeasurementOutsideLeg ->
              "https://schema.org/WearableMeasurementOutsideLeg"
            WearableMeasurementTypeEnumerationWearableMeasurementSleeve ->
              "https://schema.org/WearableMeasurementSleeve"
            WearableMeasurementTypeEnumerationWearableMeasurementWaist ->
              "https://schema.org/WearableMeasurementWaist"
            WearableMeasurementTypeEnumerationWearableMeasurementWidth ->
              "https://schema.org/WearableMeasurementWidth"
        )

data WearableSizeGroupEnumeration
  = WearableSizeGroupEnumerationWearableSizeGroupBig
  | WearableSizeGroupEnumerationWearableSizeGroupBoys
  | WearableSizeGroupEnumerationWearableSizeGroupExtraShort
  | WearableSizeGroupEnumerationWearableSizeGroupExtraTall
  | WearableSizeGroupEnumerationWearableSizeGroupGirls
  | WearableSizeGroupEnumerationWearableSizeGroupHusky
  | WearableSizeGroupEnumerationWearableSizeGroupInfants
  | WearableSizeGroupEnumerationWearableSizeGroupJuniors
  | WearableSizeGroupEnumerationWearableSizeGroupMaternity
  | WearableSizeGroupEnumerationWearableSizeGroupMens
  | WearableSizeGroupEnumerationWearableSizeGroupMisses
  | WearableSizeGroupEnumerationWearableSizeGroupPetite
  | WearableSizeGroupEnumerationWearableSizeGroupPlus
  | WearableSizeGroupEnumerationWearableSizeGroupRegular
  | WearableSizeGroupEnumerationWearableSizeGroupShort
  | WearableSizeGroupEnumerationWearableSizeGroupTall
  | WearableSizeGroupEnumerationWearableSizeGroupWomens
  deriving (Show, Eq, Ord, Generic)

instance FromJSON WearableSizeGroupEnumeration where
  parseJSON =
    withText
      "WearableSizeGroupEnumeration"
      ( \case
          "https://schema.org/WearableSizeGroupBig" ->
            pure WearableSizeGroupEnumerationWearableSizeGroupBig
          "https://schema.org/WearableSizeGroupBoys" ->
            pure WearableSizeGroupEnumerationWearableSizeGroupBoys
          "https://schema.org/WearableSizeGroupExtraShort" ->
            pure WearableSizeGroupEnumerationWearableSizeGroupExtraShort
          "https://schema.org/WearableSizeGroupExtraTall" ->
            pure WearableSizeGroupEnumerationWearableSizeGroupExtraTall
          "https://schema.org/WearableSizeGroupGirls" ->
            pure WearableSizeGroupEnumerationWearableSizeGroupGirls
          "https://schema.org/WearableSizeGroupHusky" ->
            pure WearableSizeGroupEnumerationWearableSizeGroupHusky
          "https://schema.org/WearableSizeGroupInfants" ->
            pure WearableSizeGroupEnumerationWearableSizeGroupInfants
          "https://schema.org/WearableSizeGroupJuniors" ->
            pure WearableSizeGroupEnumerationWearableSizeGroupJuniors
          "https://schema.org/WearableSizeGroupMaternity" ->
            pure WearableSizeGroupEnumerationWearableSizeGroupMaternity
          "https://schema.org/WearableSizeGroupMens" ->
            pure WearableSizeGroupEnumerationWearableSizeGroupMens
          "https://schema.org/WearableSizeGroupMisses" ->
            pure WearableSizeGroupEnumerationWearableSizeGroupMisses
          "https://schema.org/WearableSizeGroupPetite" ->
            pure WearableSizeGroupEnumerationWearableSizeGroupPetite
          "https://schema.org/WearableSizeGroupPlus" ->
            pure WearableSizeGroupEnumerationWearableSizeGroupPlus
          "https://schema.org/WearableSizeGroupRegular" ->
            pure WearableSizeGroupEnumerationWearableSizeGroupRegular
          "https://schema.org/WearableSizeGroupShort" ->
            pure WearableSizeGroupEnumerationWearableSizeGroupShort
          "https://schema.org/WearableSizeGroupTall" ->
            pure WearableSizeGroupEnumerationWearableSizeGroupTall
          "https://schema.org/WearableSizeGroupWomens" ->
            pure WearableSizeGroupEnumerationWearableSizeGroupWomens
          t ->
            fail
              ("Failed to parse WearableSizeGroupEnumeration: " <> show t)
      )

instance ToJSON WearableSizeGroupEnumeration where
  toJSON =
    (toJSON :: Text -> Value)
      . ( \case
            WearableSizeGroupEnumerationWearableSizeGroupBig ->
              "https://schema.org/WearableSizeGroupBig"
            WearableSizeGroupEnumerationWearableSizeGroupBoys ->
              "https://schema.org/WearableSizeGroupBoys"
            WearableSizeGroupEnumerationWearableSizeGroupExtraShort ->
              "https://schema.org/WearableSizeGroupExtraShort"
            WearableSizeGroupEnumerationWearableSizeGroupExtraTall ->
              "https://schema.org/WearableSizeGroupExtraTall"
            WearableSizeGroupEnumerationWearableSizeGroupGirls ->
              "https://schema.org/WearableSizeGroupGirls"
            WearableSizeGroupEnumerationWearableSizeGroupHusky ->
              "https://schema.org/WearableSizeGroupHusky"
            WearableSizeGroupEnumerationWearableSizeGroupInfants ->
              "https://schema.org/WearableSizeGroupInfants"
            WearableSizeGroupEnumerationWearableSizeGroupJuniors ->
              "https://schema.org/WearableSizeGroupJuniors"
            WearableSizeGroupEnumerationWearableSizeGroupMaternity ->
              "https://schema.org/WearableSizeGroupMaternity"
            WearableSizeGroupEnumerationWearableSizeGroupMens ->
              "https://schema.org/WearableSizeGroupMens"
            WearableSizeGroupEnumerationWearableSizeGroupMisses ->
              "https://schema.org/WearableSizeGroupMisses"
            WearableSizeGroupEnumerationWearableSizeGroupPetite ->
              "https://schema.org/WearableSizeGroupPetite"
            WearableSizeGroupEnumerationWearableSizeGroupPlus ->
              "https://schema.org/WearableSizeGroupPlus"
            WearableSizeGroupEnumerationWearableSizeGroupRegular ->
              "https://schema.org/WearableSizeGroupRegular"
            WearableSizeGroupEnumerationWearableSizeGroupShort ->
              "https://schema.org/WearableSizeGroupShort"
            WearableSizeGroupEnumerationWearableSizeGroupTall ->
              "https://schema.org/WearableSizeGroupTall"
            WearableSizeGroupEnumerationWearableSizeGroupWomens ->
              "https://schema.org/WearableSizeGroupWomens"
        )

data WearableSizeSystemEnumeration
  = WearableSizeSystemEnumerationWearableSizeSystemAU
  | WearableSizeSystemEnumerationWearableSizeSystemBR
  | WearableSizeSystemEnumerationWearableSizeSystemCN
  | WearableSizeSystemEnumerationWearableSizeSystemContinental
  | WearableSizeSystemEnumerationWearableSizeSystemDE
  | WearableSizeSystemEnumerationWearableSizeSystemEN13402
  | WearableSizeSystemEnumerationWearableSizeSystemEurope
  | WearableSizeSystemEnumerationWearableSizeSystemFR
  | WearableSizeSystemEnumerationWearableSizeSystemGS1
  | WearableSizeSystemEnumerationWearableSizeSystemIT
  | WearableSizeSystemEnumerationWearableSizeSystemJP
  | WearableSizeSystemEnumerationWearableSizeSystemMX
  | WearableSizeSystemEnumerationWearableSizeSystemUK
  | WearableSizeSystemEnumerationWearableSizeSystemUS
  deriving (Show, Eq, Ord, Generic)

instance FromJSON WearableSizeSystemEnumeration where
  parseJSON =
    withText
      "WearableSizeSystemEnumeration"
      ( \case
          "https://schema.org/WearableSizeSystemAU" ->
            pure WearableSizeSystemEnumerationWearableSizeSystemAU
          "https://schema.org/WearableSizeSystemBR" ->
            pure WearableSizeSystemEnumerationWearableSizeSystemBR
          "https://schema.org/WearableSizeSystemCN" ->
            pure WearableSizeSystemEnumerationWearableSizeSystemCN
          "https://schema.org/WearableSizeSystemContinental" ->
            pure WearableSizeSystemEnumerationWearableSizeSystemContinental
          "https://schema.org/WearableSizeSystemDE" ->
            pure WearableSizeSystemEnumerationWearableSizeSystemDE
          "https://schema.org/WearableSizeSystemEN13402" ->
            pure WearableSizeSystemEnumerationWearableSizeSystemEN13402
          "https://schema.org/WearableSizeSystemEurope" ->
            pure WearableSizeSystemEnumerationWearableSizeSystemEurope
          "https://schema.org/WearableSizeSystemFR" ->
            pure WearableSizeSystemEnumerationWearableSizeSystemFR
          "https://schema.org/WearableSizeSystemGS1" ->
            pure WearableSizeSystemEnumerationWearableSizeSystemGS1
          "https://schema.org/WearableSizeSystemIT" ->
            pure WearableSizeSystemEnumerationWearableSizeSystemIT
          "https://schema.org/WearableSizeSystemJP" ->
            pure WearableSizeSystemEnumerationWearableSizeSystemJP
          "https://schema.org/WearableSizeSystemMX" ->
            pure WearableSizeSystemEnumerationWearableSizeSystemMX
          "https://schema.org/WearableSizeSystemUK" ->
            pure WearableSizeSystemEnumerationWearableSizeSystemUK
          "https://schema.org/WearableSizeSystemUS" ->
            pure WearableSizeSystemEnumerationWearableSizeSystemUS
          t ->
            fail
              ("Failed to parse WearableSizeSystemEnumeration: " <> show t)
      )

instance ToJSON WearableSizeSystemEnumeration where
  toJSON =
    (toJSON :: Text -> Value)
      . ( \case
            WearableSizeSystemEnumerationWearableSizeSystemAU ->
              "https://schema.org/WearableSizeSystemAU"
            WearableSizeSystemEnumerationWearableSizeSystemBR ->
              "https://schema.org/WearableSizeSystemBR"
            WearableSizeSystemEnumerationWearableSizeSystemCN ->
              "https://schema.org/WearableSizeSystemCN"
            WearableSizeSystemEnumerationWearableSizeSystemContinental ->
              "https://schema.org/WearableSizeSystemContinental"
            WearableSizeSystemEnumerationWearableSizeSystemDE ->
              "https://schema.org/WearableSizeSystemDE"
            WearableSizeSystemEnumerationWearableSizeSystemEN13402 ->
              "https://schema.org/WearableSizeSystemEN13402"
            WearableSizeSystemEnumerationWearableSizeSystemEurope ->
              "https://schema.org/WearableSizeSystemEurope"
            WearableSizeSystemEnumerationWearableSizeSystemFR ->
              "https://schema.org/WearableSizeSystemFR"
            WearableSizeSystemEnumerationWearableSizeSystemGS1 ->
              "https://schema.org/WearableSizeSystemGS1"
            WearableSizeSystemEnumerationWearableSizeSystemIT ->
              "https://schema.org/WearableSizeSystemIT"
            WearableSizeSystemEnumerationWearableSizeSystemJP ->
              "https://schema.org/WearableSizeSystemJP"
            WearableSizeSystemEnumerationWearableSizeSystemMX ->
              "https://schema.org/WearableSizeSystemMX"
            WearableSizeSystemEnumerationWearableSizeSystemUK ->
              "https://schema.org/WearableSizeSystemUK"
            WearableSizeSystemEnumerationWearableSizeSystemUS ->
              "https://schema.org/WearableSizeSystemUS"
        )

data WebAPI

classWebAPI :: Class WebAPI '[Service, Intangible, Thing]
classWebAPI = Class "WebAPI"

data WebApplication

classWebApplication ::
  Class WebApplication '[SoftwareApplication, CreativeWork, Thing]
classWebApplication = Class "WebApplication"

data WebContent

classWebContent :: Class WebContent '[CreativeWork, Thing]
classWebContent = Class "WebContent"

data WebPage

classWebPage :: Class WebPage '[CreativeWork, Thing]
classWebPage = Class "WebPage"

data WebPageElement

classWebPageElement :: Class WebPageElement '[CreativeWork, Thing]
classWebPageElement = Class "WebPageElement"

data WebSite

classWebSite :: Class WebSite '[CreativeWork, Thing]
classWebSite = Class "WebSite"

data WholesaleStore

classWholesaleStore ::
  Class
    WholesaleStore
    '[ Store,
       LocalBusiness,
       Place,
       Organization,
       Thing,
       Thing
     ]
classWholesaleStore = Class "WholesaleStore"

data WinAction

classWinAction :: Class WinAction '[AchieveAction, Action, Thing]
classWinAction = Class "WinAction"

data Winery

classWinery ::
  Class
    Winery
    '[ FoodEstablishment,
       LocalBusiness,
       Place,
       Organization,
       Thing,
       Thing
     ]
classWinery = Class "Winery"

data WorkBasedProgram

classWorkBasedProgram ::
  Class
    WorkBasedProgram
    '[ EducationalOccupationalProgram,
       Intangible,
       Thing
     ]
classWorkBasedProgram = Class "WorkBasedProgram"

data WorkersUnion

classWorkersUnion :: Class WorkersUnion '[Organization, Thing]
classWorkersUnion = Class "WorkersUnion"

data WriteAction

classWriteAction ::
  Class WriteAction '[CreateAction, Action, Thing]
classWriteAction = Class "WriteAction"

type XPathType = Text

data Zoo

classZoo :: Class Zoo '[CivicStructure, Place, Thing]
classZoo = Class "Zoo"

propertyCommunicateActionAbout ::
  Property CommunicateAction '[Thing]
propertyCommunicateActionAbout = Property "about"

propertyCreativeWorkAbout :: Property CreativeWork '[Thing]
propertyCreativeWorkAbout = Property "about"

propertyEventAbout :: Property Event '[Thing]
propertyEventAbout = Property "about"

propertyBookAbridged :: Property Book '[Boolean]
propertyBookAbridged = Property "abridged"

propertyCreativeWorkAbstract :: Property CreativeWork '[Text]
propertyCreativeWorkAbstract = Property "abstract"

propertyVehicleAccelerationTime ::
  Property Vehicle '[QuantitativeValue]
propertyVehicleAccelerationTime = Property "accelerationTime"

propertyQuestionAcceptedAnswer ::
  Property Question '[ItemList, Answer]
propertyQuestionAcceptedAnswer = Property "acceptedAnswer"

propertyOrderAcceptedOffer :: Property Order '[Offer]
propertyOrderAcceptedOffer = Property "acceptedOffer"

propertyDemandAcceptedPaymentMethod ::
  Property Demand '[PaymentMethod, LoanOrCredit]
propertyDemandAcceptedPaymentMethod =
  Property "acceptedPaymentMethod"

propertyOfferAcceptedPaymentMethod ::
  Property Offer '[PaymentMethod, LoanOrCredit]
propertyOfferAcceptedPaymentMethod =
  Property "acceptedPaymentMethod"

propertyFoodEstablishmentAcceptsReservations ::
  Property FoodEstablishment '[URL, Text, Boolean]
propertyFoodEstablishmentAcceptsReservations =
  Property "acceptsReservations"

propertyDeliveryEventAccessCode :: Property DeliveryEvent '[Text]
propertyDeliveryEventAccessCode = Property "accessCode"

propertyCreativeWorkAccessMode :: Property CreativeWork '[Text]
propertyCreativeWorkAccessMode = Property "accessMode"

propertyCreativeWorkAccessModeSufficient ::
  Property CreativeWork '[ItemList]
propertyCreativeWorkAccessModeSufficient =
  Property "accessModeSufficient"

propertyCreativeWorkAccessibilityAPI ::
  Property CreativeWork '[Text]
propertyCreativeWorkAccessibilityAPI = Property "accessibilityAPI"

propertyCreativeWorkAccessibilityControl ::
  Property CreativeWork '[Text]
propertyCreativeWorkAccessibilityControl =
  Property "accessibilityControl"

propertyCreativeWorkAccessibilityFeature ::
  Property CreativeWork '[Text]
propertyCreativeWorkAccessibilityFeature =
  Property "accessibilityFeature"

propertyCreativeWorkAccessibilityHazard ::
  Property CreativeWork '[Text]
propertyCreativeWorkAccessibilityHazard =
  Property "accessibilityHazard"

propertyCreativeWorkAccessibilitySummary ::
  Property CreativeWork '[Text]
propertyCreativeWorkAccessibilitySummary =
  Property "accessibilitySummary"

propertyAccommodationAccommodationCategory ::
  Property Accommodation '[Text]
propertyAccommodationAccommodationCategory =
  Property "accommodationCategory"

propertyAccommodationAccommodationFloorPlan ::
  Property Accommodation '[FloorPlan]
propertyAccommodationAccommodationFloorPlan =
  Property "accommodationFloorPlan"

propertyResidenceAccommodationFloorPlan ::
  Property Residence '[FloorPlan]
propertyResidenceAccommodationFloorPlan =
  Property "accommodationFloorPlan"

propertyInvoiceAccountId :: Property Invoice '[Text]
propertyInvoiceAccountId = Property "accountId"

propertyBankAccountAccountMinimumInflow ::
  Property BankAccount '[MonetaryAmount]
propertyBankAccountAccountMinimumInflow =
  Property "accountMinimumInflow"

propertyBankAccountAccountOverdraftLimit ::
  Property BankAccount '[MonetaryAmount]
propertyBankAccountAccountOverdraftLimit =
  Property "accountOverdraftLimit"

propertyCreativeWorkAccountablePerson ::
  Property CreativeWork '[Person]
propertyCreativeWorkAccountablePerson =
  Property "accountablePerson"

propertyCreativeWorkAcquireLicensePage ::
  Property CreativeWork '[CreativeWork, URL]
propertyCreativeWorkAcquireLicensePage =
  Property "acquireLicensePage"

propertyOwnershipInfoAcquiredFrom ::
  Property OwnershipInfo '[Person, Organization]
propertyOwnershipInfoAcquiredFrom = Property "acquiredFrom"

propertyBusOrCoachAcrissCode :: Property BusOrCoach '[Text]
propertyBusOrCoachAcrissCode = Property "acrissCode"

propertyCarAcrissCode :: Property Car '[Text]
propertyCarAcrissCode = Property "acrissCode"

propertyConsumeActionActionAccessibilityRequirement ::
  Property ConsumeAction '[ActionAccessSpecification]
propertyConsumeActionActionAccessibilityRequirement =
  Property "actionAccessibilityRequirement"

propertyEntryPointActionApplication ::
  Property EntryPoint '[SoftwareApplication]
propertyEntryPointActionApplication = Property "actionApplication"

propertyChooseActionActionOption ::
  Property ChooseAction '[Text, Thing]
propertyChooseActionActionOption = Property "actionOption"

propertyEntryPointActionPlatform ::
  Property EntryPoint '[URL, DigitalPlatformEnumeration, Text]
propertyEntryPointActionPlatform = Property "actionPlatform"

propertyActionActionStatus :: Property Action '[ActionStatusType]
propertyActionActionStatus = Property "actionStatus"

propertyOrganizationActionableFeedbackPolicy ::
  Property Organization '[CreativeWork, URL]
propertyOrganizationActionableFeedbackPolicy =
  Property "actionableFeedbackPolicy"

propertyNewsMediaOrganizationActionableFeedbackPolicy ::
  Property NewsMediaOrganization '[CreativeWork, URL]
propertyNewsMediaOrganizationActionableFeedbackPolicy =
  Property "actionableFeedbackPolicy"

propertyDrugStrengthActiveIngredient ::
  Property DrugStrength '[Text]
propertyDrugStrengthActiveIngredient = Property "activeIngredient"

propertyDietarySupplementActiveIngredient ::
  Property DietarySupplement '[Text]
propertyDietarySupplementActiveIngredient =
  Property "activeIngredient"

propertyDrugActiveIngredient :: Property Drug '[Text]
propertyDrugActiveIngredient = Property "activeIngredient"

propertySubstanceActiveIngredient :: Property Substance '[Text]
propertySubstanceActiveIngredient = Property "activeIngredient"

propertyExercisePlanActivityDuration ::
  Property ExercisePlan '[QuantitativeValue, Duration]
propertyExercisePlanActivityDuration = Property "activityDuration"

propertyExercisePlanActivityFrequency ::
  Property ExercisePlan '[QuantitativeValue, Text]
propertyExercisePlanActivityFrequency =
  Property "activityFrequency"

propertyVideoGameSeriesActor :: Property VideoGameSeries '[Person]
propertyVideoGameSeriesActor = Property "actor"

propertyMovieActor :: Property Movie '[Person]
propertyMovieActor = Property "actor"

propertyMovieSeriesActor :: Property MovieSeries '[Person]
propertyMovieSeriesActor = Property "actor"

propertyTVSeriesActor :: Property TVSeries '[Person]
propertyTVSeriesActor = Property "actor"

propertyVideoGameActor :: Property VideoGame '[Person]
propertyVideoGameActor = Property "actor"

propertyClipActor :: Property Clip '[Person]
propertyClipActor = Property "actor"

propertyVideoObjectActor :: Property VideoObject '[Person]
propertyVideoObjectActor = Property "actor"

propertyEventActor :: Property Event '[Person]
propertyEventActor = Property "actor"

propertyPodcastSeriesActor :: Property PodcastSeries '[Person]
propertyPodcastSeriesActor = Property "actor"

propertyCreativeWorkSeasonActor ::
  Property CreativeWorkSeason '[Person]
propertyCreativeWorkSeasonActor = Property "actor"

propertyRadioSeriesActor :: Property RadioSeries '[Person]
propertyRadioSeriesActor = Property "actor"

propertyEpisodeActor :: Property Episode '[Person]
propertyEpisodeActor = Property "actor"

propertyRadioSeriesActors :: Property RadioSeries '[Person]
propertyRadioSeriesActors = Property "actors"

propertyEpisodeActors :: Property Episode '[Person]
propertyEpisodeActors = Property "actors"

propertyVideoGameSeriesActors :: Property VideoGameSeries '[Person]
propertyVideoGameSeriesActors = Property "actors"

propertyMovieActors :: Property Movie '[Person]
propertyMovieActors = Property "actors"

propertyMovieSeriesActors :: Property MovieSeries '[Person]
propertyMovieSeriesActors = Property "actors"

propertyVideoGameActors :: Property VideoGame '[Person]
propertyVideoGameActors = Property "actors"

propertyClipActors :: Property Clip '[Person]
propertyClipActors = Property "actors"

propertyVideoObjectActors :: Property VideoObject '[Person]
propertyVideoObjectActors = Property "actors"

propertyTVSeriesActors :: Property TVSeries '[Person]
propertyTVSeriesActors = Property "actors"

propertyOfferAddOn :: Property Offer '[Offer]
propertyOfferAddOn = Property "addOn"

propertyPersonAdditionalName :: Property Person '[Text]
propertyPersonAdditionalName = Property "additionalName"

propertyRsvpActionAdditionalNumberOfGuests ::
  Property RsvpAction '[Number]
propertyRsvpActionAdditionalNumberOfGuests =
  Property "additionalNumberOfGuests"

propertyQuantitativeValueAdditionalProperty ::
  Property QuantitativeValue '[PropertyValue]
propertyQuantitativeValueAdditionalProperty =
  Property "additionalProperty"

propertyPlaceAdditionalProperty :: Property Place '[PropertyValue]
propertyPlaceAdditionalProperty = Property "additionalProperty"

propertyMerchantReturnPolicyAdditionalProperty ::
  Property MerchantReturnPolicy '[PropertyValue]
propertyMerchantReturnPolicyAdditionalProperty =
  Property "additionalProperty"

propertyProductAdditionalProperty ::
  Property Product '[PropertyValue]
propertyProductAdditionalProperty = Property "additionalProperty"

propertyQualitativeValueAdditionalProperty ::
  Property QualitativeValue '[PropertyValue]
propertyQualitativeValueAdditionalProperty =
  Property "additionalProperty"

propertyThingAdditionalType :: Property Thing '[URL]
propertyThingAdditionalType = Property "additionalType"

propertyExercisePlanAdditionalVariable ::
  Property ExercisePlan '[Text]
propertyExercisePlanAdditionalVariable =
  Property "additionalVariable"

propertyPlaceAddress :: Property Place '[Text, PostalAddress]
propertyPlaceAddress = Property "address"

propertyGeoCoordinatesAddress ::
  Property GeoCoordinates '[Text, PostalAddress]
propertyGeoCoordinatesAddress = Property "address"

propertyGeoShapeAddress :: Property GeoShape '[Text, PostalAddress]
propertyGeoShapeAddress = Property "address"

propertyOrganizationAddress ::
  Property Organization '[Text, PostalAddress]
propertyOrganizationAddress = Property "address"

propertyPersonAddress :: Property Person '[Text, PostalAddress]
propertyPersonAddress = Property "address"

propertyGeoCoordinatesAddressCountry ::
  Property GeoCoordinates '[Country, Text]
propertyGeoCoordinatesAddressCountry = Property "addressCountry"

propertyDefinedRegionAddressCountry ::
  Property DefinedRegion '[Country, Text]
propertyDefinedRegionAddressCountry = Property "addressCountry"

propertyGeoShapeAddressCountry ::
  Property GeoShape '[Country, Text]
propertyGeoShapeAddressCountry = Property "addressCountry"

propertyPostalAddressAddressCountry ::
  Property PostalAddress '[Country, Text]
propertyPostalAddressAddressCountry = Property "addressCountry"

propertyPostalAddressAddressLocality ::
  Property PostalAddress '[Text]
propertyPostalAddressAddressLocality = Property "addressLocality"

propertyDefinedRegionAddressRegion ::
  Property DefinedRegion '[Text]
propertyDefinedRegionAddressRegion = Property "addressRegion"

propertyPostalAddressAddressRegion ::
  Property PostalAddress '[Text]
propertyPostalAddressAddressRegion = Property "addressRegion"

propertyDrugAdministrationRoute :: Property Drug '[Text]
propertyDrugAdministrationRoute = Property "administrationRoute"

propertyDemandAdvanceBookingRequirement ::
  Property Demand '[QuantitativeValue]
propertyDemandAdvanceBookingRequirement =
  Property "advanceBookingRequirement"

propertyOfferAdvanceBookingRequirement ::
  Property Offer '[QuantitativeValue]
propertyOfferAdvanceBookingRequirement =
  Property "advanceBookingRequirement"

propertyTherapeuticProcedureAdverseOutcome ::
  Property TherapeuticProcedure '[MedicalEntity]
propertyTherapeuticProcedureAdverseOutcome =
  Property "adverseOutcome"

propertyMedicalDeviceAdverseOutcome ::
  Property MedicalDevice '[MedicalEntity]
propertyMedicalDeviceAdverseOutcome = Property "adverseOutcome"

propertyMedicalTestAffectedBy :: Property MedicalTest '[Drug]
propertyMedicalTestAffectedBy = Property "affectedBy"

propertyPersonAffiliation :: Property Person '[Organization]
propertyPersonAffiliation = Property "affiliation"

propertyHowToDirectionAfterMedia ::
  Property HowToDirection '[MediaObject, URL]
propertyHowToDirectionAfterMedia = Property "afterMedia"

propertyActionAgent :: Property Action '[Organization, Person]
propertyActionAgent = Property "agent"

propertyOfferAggregateRating :: Property Offer '[AggregateRating]
propertyOfferAggregateRating = Property "aggregateRating"

propertyBrandAggregateRating :: Property Brand '[AggregateRating]
propertyBrandAggregateRating = Property "aggregateRating"

propertyEventAggregateRating :: Property Event '[AggregateRating]
propertyEventAggregateRating = Property "aggregateRating"

propertyPlaceAggregateRating :: Property Place '[AggregateRating]
propertyPlaceAggregateRating = Property "aggregateRating"

propertyProductAggregateRating ::
  Property Product '[AggregateRating]
propertyProductAggregateRating = Property "aggregateRating"

propertyServiceAggregateRating ::
  Property Service '[AggregateRating]
propertyServiceAggregateRating = Property "aggregateRating"

propertyCreativeWorkAggregateRating ::
  Property CreativeWork '[AggregateRating]
propertyCreativeWorkAggregateRating = Property "aggregateRating"

propertyOrganizationAggregateRating ::
  Property Organization '[AggregateRating]
propertyOrganizationAggregateRating = Property "aggregateRating"

propertyFlightAircraft :: Property Flight '[Text, Vehicle]
propertyFlightAircraft = Property "aircraft"

propertyMusicGroupAlbum :: Property MusicGroup '[MusicAlbum]
propertyMusicGroupAlbum = Property "album"

propertyMusicAlbumAlbumProductionType ::
  Property MusicAlbum '[MusicAlbumProductionType]
propertyMusicAlbumAlbumProductionType =
  Property "albumProductionType"

propertyMusicAlbumAlbumRelease ::
  Property MusicAlbum '[MusicRelease]
propertyMusicAlbumAlbumRelease = Property "albumRelease"

propertyMusicAlbumAlbumReleaseType ::
  Property MusicAlbum '[MusicAlbumReleaseType]
propertyMusicAlbumAlbumReleaseType = Property "albumReleaseType"

propertyMusicGroupAlbums :: Property MusicGroup '[MusicAlbum]
propertyMusicGroupAlbums = Property "albums"

propertyDrugAlcoholWarning :: Property Drug '[Text]
propertyDrugAlcoholWarning = Property "alcoholWarning"

propertyMedicalRiskScoreAlgorithm ::
  Property MedicalRiskScore '[Text]
propertyMedicalRiskScoreAlgorithm = Property "algorithm"

propertyAlignmentObjectAlignmentType ::
  Property AlignmentObject '[Text]
propertyAlignmentObjectAlignmentType = Property "alignmentType"

propertyThingAlternateName :: Property Thing '[Text]
propertyThingAlternateName = Property "alternateName"

propertyCreativeWorkAlternativeHeadline ::
  Property CreativeWork '[Text]
propertyCreativeWorkAlternativeHeadline =
  Property "alternativeHeadline"

propertyGeneAlternativeOf :: Property Gene '[Gene]
propertyGeneAlternativeOf = Property "alternativeOf"

propertyOrganizationAlumni :: Property Organization '[Person]
propertyOrganizationAlumni = Property "alumni"

propertyEducationalOrganizationAlumni ::
  Property EducationalOrganization '[Person]
propertyEducationalOrganizationAlumni = Property "alumni"

propertyPersonAlumniOf ::
  Property Person '[Organization, EducationalOrganization]
propertyPersonAlumniOf = Property "alumniOf"

propertyFloorPlanAmenityFeature ::
  Property FloorPlan '[LocationFeatureSpecification]
propertyFloorPlanAmenityFeature = Property "amenityFeature"

propertyLodgingBusinessAmenityFeature ::
  Property LodgingBusiness '[LocationFeatureSpecification]
propertyLodgingBusinessAmenityFeature = Property "amenityFeature"

propertyAccommodationAmenityFeature ::
  Property Accommodation '[LocationFeatureSpecification]
propertyAccommodationAmenityFeature = Property "amenityFeature"

propertyPlaceAmenityFeature ::
  Property Place '[LocationFeatureSpecification]
propertyPlaceAmenityFeature = Property "amenityFeature"

propertyMonetaryGrantAmount ::
  Property MonetaryGrant '[MonetaryAmount, Number]
propertyMonetaryGrantAmount = Property "amount"

propertyInvestmentOrDepositAmount ::
  Property InvestmentOrDeposit '[MonetaryAmount, Number]
propertyInvestmentOrDepositAmount = Property "amount"

propertyLoanOrCreditAmount ::
  Property LoanOrCredit '[MonetaryAmount, Number]
propertyLoanOrCreditAmount = Property "amount"

propertyMoneyTransferAmount ::
  Property MoneyTransfer '[MonetaryAmount, Number]
propertyMoneyTransferAmount = Property "amount"

propertyDatedMoneySpecificationAmount ::
  Property DatedMoneySpecification '[MonetaryAmount, Number]
propertyDatedMoneySpecificationAmount = Property "amount"

propertyTypeAndQuantityNodeAmountOfThisGood ::
  Property TypeAndQuantityNode '[Number]
propertyTypeAndQuantityNodeAmountOfThisGood =
  Property "amountOfThisGood"

propertySpecialAnnouncementAnnouncementLocation ::
  Property SpecialAnnouncement '[CivicStructure, LocalBusiness]
propertySpecialAnnouncementAnnouncementLocation =
  Property "announcementLocation"

propertyFinancialProductAnnualPercentageRate ::
  Property FinancialProduct '[Number, QuantitativeValue]
propertyFinancialProductAnnualPercentageRate =
  Property "annualPercentageRate"

propertyQuestionAnswerCount :: Property Question '[Integer]
propertyQuestionAnswerCount = Property "answerCount"

propertyAnswerAnswerExplanation ::
  Property Answer '[Comment, WebContent]
propertyAnswerAnswerExplanation = Property "answerExplanation"

propertyMuscleAntagonist :: Property Muscle '[Muscle]
propertyMuscleAntagonist = Property "antagonist"

propertyClaimAppearance :: Property Claim '[CreativeWork]
propertyClaimAppearance = Property "appearance"

propertyMerchantReturnPolicyApplicableCountry ::
  Property MerchantReturnPolicy '[Country, Text]
propertyMerchantReturnPolicyApplicableCountry =
  Property "applicableCountry"

propertyDrugCostApplicableLocation ::
  Property DrugCost '[AdministrativeArea]
propertyDrugCostApplicableLocation = Property "applicableLocation"

propertyDrugLegalStatusApplicableLocation ::
  Property DrugLegalStatus '[AdministrativeArea]
propertyDrugLegalStatusApplicableLocation =
  Property "applicableLocation"

propertyJobPostingApplicantLocationRequirements ::
  Property JobPosting '[AdministrativeArea]
propertyJobPostingApplicantLocationRequirements =
  Property "applicantLocationRequirements"

propertyEntryPointApplication ::
  Property EntryPoint '[SoftwareApplication]
propertyEntryPointApplication = Property "application"

propertySoftwareApplicationApplicationCategory ::
  Property SoftwareApplication '[URL, Text]
propertySoftwareApplicationApplicationCategory =
  Property "applicationCategory"

propertyJobPostingApplicationContact ::
  Property JobPosting '[ContactPoint]
propertyJobPostingApplicationContact =
  Property "applicationContact"

propertyEducationalOccupationalProgramApplicationDeadline ::
  Property EducationalOccupationalProgram '[Date]
propertyEducationalOccupationalProgramApplicationDeadline =
  Property "applicationDeadline"

propertyEducationalOccupationalProgramApplicationStartDate ::
  Property EducationalOccupationalProgram '[Date]
propertyEducationalOccupationalProgramApplicationStartDate =
  Property "applicationStartDate"

propertySoftwareApplicationApplicationSubCategory ::
  Property SoftwareApplication '[URL, Text]
propertySoftwareApplicationApplicationSubCategory =
  Property "applicationSubCategory"

propertySoftwareApplicationApplicationSuite ::
  Property SoftwareApplication '[Text]
propertySoftwareApplicationApplicationSuite =
  Property "applicationSuite"

propertyDeliveryChargeSpecificationAppliesToDeliveryMethod ::
  Property DeliveryChargeSpecification '[DeliveryMethod]
propertyDeliveryChargeSpecificationAppliesToDeliveryMethod =
  Property "appliesToDeliveryMethod"

propertyPaymentChargeSpecificationAppliesToDeliveryMethod ::
  Property PaymentChargeSpecification '[DeliveryMethod]
propertyPaymentChargeSpecificationAppliesToDeliveryMethod =
  Property "appliesToDeliveryMethod"

propertyPaymentChargeSpecificationAppliesToPaymentMethod ::
  Property PaymentChargeSpecification '[PaymentMethod]
propertyPaymentChargeSpecificationAppliesToPaymentMethod =
  Property "appliesToPaymentMethod"

propertyArchiveOrganizationArchiveHeld ::
  Property ArchiveOrganization '[ArchiveComponent]
propertyArchiveOrganizationArchiveHeld = Property "archiveHeld"

propertyCreativeWorkArchivedAt ::
  Property CreativeWork '[WebPage, URL]
propertyCreativeWorkArchivedAt = Property "archivedAt"

propertyBroadcastServiceArea :: Property BroadcastService '[Place]
propertyBroadcastServiceArea = Property "area"

propertyContactPointAreaServed ::
  Property ContactPoint '[Text, Place, GeoShape, AdministrativeArea]
propertyContactPointAreaServed = Property "areaServed"

propertyDemandAreaServed ::
  Property Demand '[Text, Place, GeoShape, AdministrativeArea]
propertyDemandAreaServed = Property "areaServed"

propertyOfferAreaServed ::
  Property Offer '[Text, Place, GeoShape, AdministrativeArea]
propertyOfferAreaServed = Property "areaServed"

propertyDeliveryChargeSpecificationAreaServed ::
  Property
    DeliveryChargeSpecification
    '[ Text,
       Place,
       GeoShape,
       AdministrativeArea
     ]
propertyDeliveryChargeSpecificationAreaServed =
  Property "areaServed"

propertyServiceAreaServed ::
  Property Service '[Text, Place, GeoShape, AdministrativeArea]
propertyServiceAreaServed = Property "areaServed"

propertyOrganizationAreaServed ::
  Property Organization '[Text, Place, GeoShape, AdministrativeArea]
propertyOrganizationAreaServed = Property "areaServed"

propertyFlightArrivalAirport :: Property Flight '[Airport]
propertyFlightArrivalAirport = Property "arrivalAirport"

propertyBoatTripArrivalBoatTerminal ::
  Property BoatTrip '[BoatTerminal]
propertyBoatTripArrivalBoatTerminal =
  Property "arrivalBoatTerminal"

propertyBusTripArrivalBusStop ::
  Property BusTrip '[BusStop, BusStation]
propertyBusTripArrivalBusStop = Property "arrivalBusStop"

propertyFlightArrivalGate :: Property Flight '[Text]
propertyFlightArrivalGate = Property "arrivalGate"

propertyTrainTripArrivalPlatform :: Property TrainTrip '[Text]
propertyTrainTripArrivalPlatform = Property "arrivalPlatform"

propertyTrainTripArrivalStation ::
  Property TrainTrip '[TrainStation]
propertyTrainTripArrivalStation = Property "arrivalStation"

propertyFlightArrivalTerminal :: Property Flight '[Text]
propertyFlightArrivalTerminal = Property "arrivalTerminal"

propertyTripArrivalTime :: Property Trip '[DateTime, Time]
propertyTripArrivalTime = Property "arrivalTime"

propertyVisualArtworkArtEdition ::
  Property VisualArtwork '[Integer, Text]
propertyVisualArtworkArtEdition = Property "artEdition"

propertyVisualArtworkArtMedium ::
  Property VisualArtwork '[URL, Text]
propertyVisualArtworkArtMedium = Property "artMedium"

propertyArteryArterialBranch ::
  Property Artery '[AnatomicalStructure]
propertyArteryArterialBranch = Property "arterialBranch"

propertyVisualArtworkArtform :: Property VisualArtwork '[URL, Text]
propertyVisualArtworkArtform = Property "artform"

propertyArticleArticleBody :: Property Article '[Text]
propertyArticleArticleBody = Property "articleBody"

propertyArticleArticleSection :: Property Article '[Text]
propertyArticleArticleSection = Property "articleSection"

propertyComicStoryArtist :: Property ComicStory '[Person]
propertyComicStoryArtist = Property "artist"

propertyComicIssueArtist :: Property ComicIssue '[Person]
propertyComicIssueArtist = Property "artist"

propertyVisualArtworkArtist :: Property VisualArtwork '[Person]
propertyVisualArtworkArtist = Property "artist"

propertyVisualArtworkArtworkSurface ::
  Property VisualArtwork '[URL, Text]
propertyVisualArtworkArtworkSurface = Property "artworkSurface"

propertyProductAsin :: Property Product '[Text, URL]
propertyProductAsin = Property "asin"

propertyDemandAsin :: Property Demand '[Text, URL]
propertyDemandAsin = Property "asin"

propertyOfferAsin :: Property Offer '[Text, URL]
propertyOfferAsin = Property "asin"

propertyMedicalWebPageAspect :: Property MedicalWebPage '[Text]
propertyMedicalWebPageAspect = Property "aspect"

propertyAPIReferenceAssembly :: Property APIReference '[Text]
propertyAPIReferenceAssembly = Property "assembly"

propertyAPIReferenceAssemblyVersion ::
  Property APIReference '[Text]
propertyAPIReferenceAssemblyVersion = Property "assemblyVersion"

propertyCreativeWorkAssesses ::
  Property CreativeWork '[DefinedTerm, Text]
propertyCreativeWorkAssesses = Property "assesses"

propertyEducationEventAssesses ::
  Property EducationEvent '[DefinedTerm, Text]
propertyEducationEventAssesses = Property "assesses"

propertyLearningResourceAssesses ::
  Property LearningResource '[DefinedTerm, Text]
propertyLearningResourceAssesses = Property "assesses"

propertyPhysicalActivityAssociatedAnatomy ::
  Property
    PhysicalActivity
    '[ SuperficialAnatomy,
       AnatomicalSystem,
       AnatomicalStructure
     ]
propertyPhysicalActivityAssociatedAnatomy =
  Property "associatedAnatomy"

propertyMedicalConditionAssociatedAnatomy ::
  Property
    MedicalCondition
    '[ SuperficialAnatomy,
       AnatomicalSystem,
       AnatomicalStructure
     ]
propertyMedicalConditionAssociatedAnatomy =
  Property "associatedAnatomy"

propertyMediaObjectAssociatedArticle ::
  Property MediaObject '[NewsArticle]
propertyMediaObjectAssociatedArticle = Property "associatedArticle"

propertyReviewAssociatedClaimReview :: Property Review '[Review]
propertyReviewAssociatedClaimReview =
  Property "associatedClaimReview"

propertyBioChemEntityAssociatedDisease ::
  Property BioChemEntity '[MedicalCondition, URL, PropertyValue]
propertyBioChemEntityAssociatedDisease =
  Property "associatedDisease"

propertyHyperTocAssociatedMedia :: Property HyperToc '[MediaObject]
propertyHyperTocAssociatedMedia = Property "associatedMedia"

propertyCreativeWorkAssociatedMedia ::
  Property CreativeWork '[MediaObject]
propertyCreativeWorkAssociatedMedia = Property "associatedMedia"

propertyHyperTocEntryAssociatedMedia ::
  Property HyperTocEntry '[MediaObject]
propertyHyperTocEntryAssociatedMedia = Property "associatedMedia"

propertyReviewAssociatedMediaReview :: Property Review '[Review]
propertyReviewAssociatedMediaReview =
  Property "associatedMediaReview"

propertySuperficialAnatomyAssociatedPathophysiology ::
  Property SuperficialAnatomy '[Text]
propertySuperficialAnatomyAssociatedPathophysiology =
  Property "associatedPathophysiology"

propertyAnatomicalSystemAssociatedPathophysiology ::
  Property AnatomicalSystem '[Text]
propertyAnatomicalSystemAssociatedPathophysiology =
  Property "associatedPathophysiology"

propertyAnatomicalStructureAssociatedPathophysiology ::
  Property AnatomicalStructure '[Text]
propertyAnatomicalStructureAssociatedPathophysiology =
  Property "associatedPathophysiology"

propertyReviewAssociatedReview :: Property Review '[Review]
propertyReviewAssociatedReview = Property "associatedReview"

propertySportsTeamAthlete :: Property SportsTeam '[Person]
propertySportsTeamAthlete = Property "athlete"

propertyEventAttendee :: Property Event '[Organization, Person]
propertyEventAttendee = Property "attendee"

propertyEventAttendees :: Property Event '[Organization, Person]
propertyEventAttendees = Property "attendees"

propertyProductAudience :: Property Product '[Audience]
propertyProductAudience = Property "audience"

propertyServiceAudience :: Property Service '[Audience]
propertyServiceAudience = Property "audience"

propertyCreativeWorkAudience :: Property CreativeWork '[Audience]
propertyCreativeWorkAudience = Property "audience"

propertyLodgingBusinessAudience ::
  Property LodgingBusiness '[Audience]
propertyLodgingBusinessAudience = Property "audience"

propertyEventAudience :: Property Event '[Audience]
propertyEventAudience = Property "audience"

propertyPlayActionAudience :: Property PlayAction '[Audience]
propertyPlayActionAudience = Property "audience"

propertyAudienceAudienceType :: Property Audience '[Text]
propertyAudienceAudienceType = Property "audienceType"

propertyCreativeWorkAudio ::
  Property CreativeWork '[MusicRecording, AudioObject, Clip]
propertyCreativeWorkAudio = Property "audio"

propertyMediaSubscriptionAuthenticator ::
  Property MediaSubscription '[Organization]
propertyMediaSubscriptionAuthenticator = Property "authenticator"

propertyRatingAuthor :: Property Rating '[Organization, Person]
propertyRatingAuthor = Property "author"

propertyCreativeWorkAuthor ::
  Property CreativeWork '[Organization, Person]
propertyCreativeWorkAuthor = Property "author"

propertyDemandAvailability :: Property Demand '[ItemAvailability]
propertyDemandAvailability = Property "availability"

propertyOfferAvailability :: Property Offer '[ItemAvailability]
propertyOfferAvailability = Property "availability"

propertyDemandAvailabilityEnds ::
  Property Demand '[DateTime, Time, Date]
propertyDemandAvailabilityEnds = Property "availabilityEnds"

propertyOfferAvailabilityEnds ::
  Property Offer '[DateTime, Time, Date]
propertyOfferAvailabilityEnds = Property "availabilityEnds"

propertyActionAccessSpecificationAvailabilityEnds ::
  Property ActionAccessSpecification '[DateTime, Time, Date]
propertyActionAccessSpecificationAvailabilityEnds =
  Property "availabilityEnds"

propertyDemandAvailabilityStarts ::
  Property Demand '[Date, Time, DateTime]
propertyDemandAvailabilityStarts = Property "availabilityStarts"

propertyOfferAvailabilityStarts ::
  Property Offer '[Date, Time, DateTime]
propertyOfferAvailabilityStarts = Property "availabilityStarts"

propertyActionAccessSpecificationAvailabilityStarts ::
  Property ActionAccessSpecification '[Date, Time, DateTime]
propertyActionAccessSpecificationAvailabilityStarts =
  Property "availabilityStarts"

propertyOfferAvailableAtOrFrom :: Property Offer '[Place]
propertyOfferAvailableAtOrFrom = Property "availableAtOrFrom"

propertyDemandAvailableAtOrFrom :: Property Demand '[Place]
propertyDemandAvailableAtOrFrom = Property "availableAtOrFrom"

propertyServiceAvailableChannel ::
  Property Service '[ServiceChannel]
propertyServiceAvailableChannel = Property "availableChannel"

propertyDemandAvailableDeliveryMethod ::
  Property Demand '[DeliveryMethod]
propertyDemandAvailableDeliveryMethod =
  Property "availableDeliveryMethod"

propertyOfferAvailableDeliveryMethod ::
  Property Offer '[DeliveryMethod]
propertyOfferAvailableDeliveryMethod =
  Property "availableDeliveryMethod"

propertyDeliveryEventAvailableFrom ::
  Property DeliveryEvent '[DateTime]
propertyDeliveryEventAvailableFrom = Property "availableFrom"

propertyDrugStrengthAvailableIn ::
  Property DrugStrength '[AdministrativeArea]
propertyDrugStrengthAvailableIn = Property "availableIn"

propertyTouristAttractionAvailableLanguage ::
  Property TouristAttraction '[Text, Language]
propertyTouristAttractionAvailableLanguage =
  Property "availableLanguage"

propertyContactPointAvailableLanguage ::
  Property ContactPoint '[Text, Language]
propertyContactPointAvailableLanguage =
  Property "availableLanguage"

propertyLodgingBusinessAvailableLanguage ::
  Property LodgingBusiness '[Text, Language]
propertyLodgingBusinessAvailableLanguage =
  Property "availableLanguage"

propertyServiceChannelAvailableLanguage ::
  Property ServiceChannel '[Text, Language]
propertyServiceChannelAvailableLanguage =
  Property "availableLanguage"

propertySoftwareApplicationAvailableOnDevice ::
  Property SoftwareApplication '[Text]
propertySoftwareApplicationAvailableOnDevice =
  Property "availableOnDevice"

propertyPhysicianAvailableService ::
  Property Physician '[MedicalTherapy, MedicalTest, MedicalProcedure]
propertyPhysicianAvailableService = Property "availableService"

propertyHospitalAvailableService ::
  Property Hospital '[MedicalTherapy, MedicalTest, MedicalProcedure]
propertyHospitalAvailableService = Property "availableService"

propertyMedicalClinicAvailableService ::
  Property
    MedicalClinic
    '[ MedicalTherapy,
       MedicalTest,
       MedicalProcedure
     ]
propertyMedicalClinicAvailableService = Property "availableService"

propertyDrugAvailableStrength :: Property Drug '[DrugStrength]
propertyDrugAvailableStrength = Property "availableStrength"

propertyDiagnosticLabAvailableTest ::
  Property DiagnosticLab '[MedicalTest]
propertyDiagnosticLabAvailableTest = Property "availableTest"

propertyDeliveryEventAvailableThrough ::
  Property DeliveryEvent '[DateTime]
propertyDeliveryEventAvailableThrough = Property "availableThrough"

propertyServiceAward :: Property Service '[Text]
propertyServiceAward = Property "award"

propertyCreativeWorkAward :: Property CreativeWork '[Text]
propertyCreativeWorkAward = Property "award"

propertyOrganizationAward :: Property Organization '[Text]
propertyOrganizationAward = Property "award"

propertyPersonAward :: Property Person '[Text]
propertyPersonAward = Property "award"

propertyProductAward :: Property Product '[Text]
propertyProductAward = Property "award"

propertyCreativeWorkAwards :: Property CreativeWork '[Text]
propertyCreativeWorkAwards = Property "awards"

propertyOrganizationAwards :: Property Organization '[Text]
propertyOrganizationAwards = Property "awards"

propertyPersonAwards :: Property Person '[Text]
propertyPersonAwards = Property "awards"

propertyProductAwards :: Property Product '[Text]
propertyProductAwards = Property "awards"

propertySportsEventAwayTeam ::
  Property SportsEvent '[SportsTeam, Person]
propertySportsEventAwayTeam = Property "awayTeam"

propertyArticleBackstory :: Property Article '[CreativeWork, Text]
propertyArticleBackstory = Property "backstory"

propertyBankAccountBankAccountType ::
  Property BankAccount '[Text, URL]
propertyBankAccountBankAccountType = Property "bankAccountType"

propertyJobPostingBaseSalary ::
  Property JobPosting '[MonetaryAmount, Number, PriceSpecification]
propertyJobPostingBaseSalary = Property "baseSalary"

propertyEmployeeRoleBaseSalary ::
  Property EmployeeRole '[MonetaryAmount, Number, PriceSpecification]
propertyEmployeeRoleBaseSalary = Property "baseSalary"

propertyMessageBccRecipient ::
  Property Message '[Organization, ContactPoint, Person]
propertyMessageBccRecipient = Property "bccRecipient"

propertyHotelRoomBed ::
  Property HotelRoom '[Text, BedDetails, BedType]
propertyHotelRoomBed = Property "bed"

propertySuiteBed :: Property Suite '[Text, BedDetails, BedType]
propertySuiteBed = Property "bed"

propertyHowToDirectionBeforeMedia ::
  Property HowToDirection '[MediaObject, URL]
propertyHowToDirectionBeforeMedia = Property "beforeMedia"

propertyMoneyTransferBeneficiaryBank ::
  Property MoneyTransfer '[Text, BankOrCreditUnion]
propertyMoneyTransferBeneficiaryBank = Property "beneficiaryBank"

propertyJobPostingBenefits :: Property JobPosting '[Text]
propertyJobPostingBenefits = Property "benefits"

propertyHealthInsurancePlanBenefitsSummaryUrl ::
  Property HealthInsurancePlan '[URL]
propertyHealthInsurancePlanBenefitsSummaryUrl =
  Property "benefitsSummaryUrl"

propertyRatingBestRating :: Property Rating '[Text, Number]
propertyRatingBestRating = Property "bestRating"

propertyOrderBillingAddress :: Property Order '[PostalAddress]
propertyOrderBillingAddress = Property "billingAddress"

propertyUnitPriceSpecificationBillingDuration ::
  Property
    UnitPriceSpecification
    '[ Number,
       QuantitativeValue,
       Duration
     ]
propertyUnitPriceSpecificationBillingDuration =
  Property "billingDuration"

propertyUnitPriceSpecificationBillingIncrement ::
  Property UnitPriceSpecification '[Number]
propertyUnitPriceSpecificationBillingIncrement =
  Property "billingIncrement"

propertyInvoiceBillingPeriod :: Property Invoice '[Duration]
propertyInvoiceBillingPeriod = Property "billingPeriod"

propertyUnitPriceSpecificationBillingStart ::
  Property UnitPriceSpecification '[Number]
propertyUnitPriceSpecificationBillingStart =
  Property "billingStart"

propertyBioChemEntityBioChemInteraction ::
  Property BioChemEntity '[BioChemEntity]
propertyBioChemEntityBioChemInteraction =
  Property "bioChemInteraction"

propertyBioChemEntityBioChemSimilarity ::
  Property BioChemEntity '[BioChemEntity]
propertyBioChemEntityBioChemSimilarity =
  Property "bioChemSimilarity"

propertyBioChemEntityBiologicalRole ::
  Property BioChemEntity '[DefinedTerm]
propertyBioChemEntityBiologicalRole = Property "biologicalRole"

propertyJointBiomechnicalClass :: Property Joint '[Text]
propertyJointBiomechnicalClass = Property "biomechnicalClass"

propertyPersonBirthDate :: Property Person '[Date]
propertyPersonBirthDate = Property "birthDate"

propertyPersonBirthPlace :: Property Person '[Place]
propertyPersonBirthPlace = Property "birthPlace"

propertyMediaObjectBitrate :: Property MediaObject '[Text]
propertyMediaObjectBitrate = Property "bitrate"

propertyBlogBlogPost :: Property Blog '[BlogPosting]
propertyBlogBlogPost = Property "blogPost"

propertyBlogBlogPosts :: Property Blog '[BlogPosting]
propertyBlogBlogPosts = Property "blogPosts"

propertyMuscleBloodSupply :: Property Muscle '[Vessel]
propertyMuscleBloodSupply = Property "bloodSupply"

propertyFlightReservationBoardingGroup ::
  Property FlightReservation '[Text]
propertyFlightReservationBoardingGroup = Property "boardingGroup"

propertyAirlineBoardingPolicy ::
  Property Airline '[BoardingPolicyType]
propertyAirlineBoardingPolicy = Property "boardingPolicy"

propertyFlightBoardingPolicy ::
  Property Flight '[BoardingPolicyType]
propertyFlightBoardingPolicy = Property "boardingPolicy"

propertyMedicalProcedureBodyLocation ::
  Property MedicalProcedure '[Text]
propertyMedicalProcedureBodyLocation = Property "bodyLocation"

propertyAnatomicalStructureBodyLocation ::
  Property AnatomicalStructure '[Text]
propertyAnatomicalStructureBodyLocation = Property "bodyLocation"

propertyVehicleBodyType ::
  Property Vehicle '[URL, QualitativeValue, Text]
propertyVehicleBodyType = Property "bodyType"

propertyBookBookEdition :: Property Book '[Text]
propertyBookBookEdition = Property "bookEdition"

propertyBookBookFormat :: Property Book '[BookFormatType]
propertyBookBookFormat = Property "bookFormat"

propertyReservationBookingAgent ::
  Property Reservation '[Organization, Person]
propertyReservationBookingAgent = Property "bookingAgent"

propertyReservationBookingTime :: Property Reservation '[DateTime]
propertyReservationBookingTime = Property "bookingTime"

propertyLendActionBorrower :: Property LendAction '[Person]
propertyLendActionBorrower = Property "borrower"

propertyGeoShapeBox :: Property GeoShape '[Text]
propertyGeoShapeBox = Property "box"

propertyNerveBranch :: Property Nerve '[AnatomicalStructure]
propertyNerveBranch = Property "branch"

propertyPlaceBranchCode :: Property Place '[Text]
propertyPlaceBranchCode = Property "branchCode"

propertyLocalBusinessBranchOf ::
  Property LocalBusiness '[Organization]
propertyLocalBusinessBranchOf = Property "branchOf"

propertyProductBrand :: Property Product '[Brand, Organization]
propertyProductBrand = Property "brand"

propertyServiceBrand :: Property Service '[Brand, Organization]
propertyServiceBrand = Property "brand"

propertyOrganizationBrand ::
  Property Organization '[Brand, Organization]
propertyOrganizationBrand = Property "brand"

propertyPersonBrand :: Property Person '[Brand, Organization]
propertyPersonBrand = Property "brand"

propertyWebPageBreadcrumb ::
  Property WebPage '[BreadcrumbList, Text]
propertyWebPageBreadcrumb = Property "breadcrumb"

propertyDrugBreastfeedingWarning :: Property Drug '[Text]
propertyDrugBreastfeedingWarning = Property "breastfeedingWarning"

propertyBroadcastServiceBroadcastAffiliateOf ::
  Property BroadcastService '[Organization]
propertyBroadcastServiceBroadcastAffiliateOf =
  Property "broadcastAffiliateOf"

propertyBroadcastChannelBroadcastChannelId ::
  Property BroadcastChannel '[Text]
propertyBroadcastChannelBroadcastChannelId =
  Property "broadcastChannelId"

propertyBroadcastServiceBroadcastDisplayName ::
  Property BroadcastService '[Text]
propertyBroadcastServiceBroadcastDisplayName =
  Property "broadcastDisplayName"

propertyBroadcastServiceBroadcastFrequency ::
  Property BroadcastService '[Text, BroadcastFrequencySpecification]
propertyBroadcastServiceBroadcastFrequency =
  Property "broadcastFrequency"

propertyBroadcastChannelBroadcastFrequency ::
  Property BroadcastChannel '[Text, BroadcastFrequencySpecification]
propertyBroadcastChannelBroadcastFrequency =
  Property "broadcastFrequency"

propertyBroadcastFrequencySpecificationBroadcastFrequencyValue ::
  Property
    BroadcastFrequencySpecification
    '[ Number,
       QuantitativeValue
     ]
propertyBroadcastFrequencySpecificationBroadcastFrequencyValue =
  Property "broadcastFrequencyValue"

propertyBroadcastEventBroadcastOfEvent ::
  Property BroadcastEvent '[Event]
propertyBroadcastEventBroadcastOfEvent =
  Property "broadcastOfEvent"

propertyBroadcastChannelBroadcastServiceTier ::
  Property BroadcastChannel '[Text]
propertyBroadcastChannelBroadcastServiceTier =
  Property "broadcastServiceTier"

propertyBroadcastFrequencySpecificationBroadcastSignalModulation ::
  Property BroadcastFrequencySpecification '[Text, QualitativeValue]
propertyBroadcastFrequencySpecificationBroadcastSignalModulation =
  Property "broadcastSignalModulation"

propertyBroadcastFrequencySpecificationBroadcastSubChannel ::
  Property BroadcastFrequencySpecification '[Text]
propertyBroadcastFrequencySpecificationBroadcastSubChannel =
  Property "broadcastSubChannel"

propertyBroadcastServiceBroadcastTimezone ::
  Property BroadcastService '[Text]
propertyBroadcastServiceBroadcastTimezone =
  Property "broadcastTimezone"

propertyBroadcastServiceBroadcaster ::
  Property BroadcastService '[Organization]
propertyBroadcastServiceBroadcaster = Property "broadcaster"

propertyServiceBroker :: Property Service '[Person, Organization]
propertyServiceBroker = Property "broker"

propertyReservationBroker ::
  Property Reservation '[Person, Organization]
propertyReservationBroker = Property "broker"

propertyOrderBroker :: Property Order '[Person, Organization]
propertyOrderBroker = Property "broker"

propertyInvoiceBroker :: Property Invoice '[Person, Organization]
propertyInvoiceBroker = Property "broker"

propertyWebApplicationBrowserRequirements ::
  Property WebApplication '[Text]
propertyWebApplicationBrowserRequirements =
  Property "browserRequirements"

propertyBusTripBusName :: Property BusTrip '[Text]
propertyBusTripBusName = Property "busName"

propertyBusTripBusNumber :: Property BusTrip '[Text]
propertyBusTripBusNumber = Property "busNumber"

propertyShippingDeliveryTimeBusinessDays ::
  Property ShippingDeliveryTime '[OpeningHoursSpecification]
propertyShippingDeliveryTimeBusinessDays = Property "businessDays"

propertyTypeAndQuantityNodeBusinessFunction ::
  Property TypeAndQuantityNode '[BusinessFunction]
propertyTypeAndQuantityNodeBusinessFunction =
  Property "businessFunction"

propertyDemandBusinessFunction ::
  Property Demand '[BusinessFunction]
propertyDemandBusinessFunction = Property "businessFunction"

propertyOfferBusinessFunction :: Property Offer '[BusinessFunction]
propertyOfferBusinessFunction = Property "businessFunction"

propertySellActionBuyer ::
  Property SellAction '[Organization, Person]
propertySellActionBuyer = Property "buyer"

propertyMusicAlbumByArtist ::
  Property MusicAlbum '[MusicGroup, Person]
propertyMusicAlbumByArtist = Property "byArtist"

propertyMusicRecordingByArtist ::
  Property MusicRecording '[MusicGroup, Person]
propertyMusicRecordingByArtist = Property "byArtist"

propertyScheduleByDay :: Property Schedule '[Text, DayOfWeek]
propertyScheduleByDay = Property "byDay"

propertyScheduleByMonth :: Property Schedule '[Integer]
propertyScheduleByMonth = Property "byMonth"

propertyScheduleByMonthDay :: Property Schedule '[Integer]
propertyScheduleByMonthDay = Property "byMonthDay"

propertyScheduleByMonthWeek :: Property Schedule '[Integer]
propertyScheduleByMonthWeek = Property "byMonthWeek"

propertyBroadcastServiceCallSign ::
  Property BroadcastService '[Text]
propertyBroadcastServiceCallSign = Property "callSign"

propertyVehicleCallSign :: Property Vehicle '[Text]
propertyVehicleCallSign = Property "callSign"

propertyPersonCallSign :: Property Person '[Text]
propertyPersonCallSign = Property "callSign"

propertyNutritionInformationCalories ::
  Property NutritionInformation '[Energy]
propertyNutritionInformationCalories = Property "calories"

propertyVoteActionCandidate :: Property VoteAction '[Person]
propertyVoteActionCandidate = Property "candidate"

propertyVideoObjectCaption ::
  Property VideoObject '[MediaObject, Text]
propertyVideoObjectCaption = Property "caption"

propertyImageObjectCaption ::
  Property ImageObject '[MediaObject, Text]
propertyImageObjectCaption = Property "caption"

propertyAudioObjectCaption ::
  Property AudioObject '[MediaObject, Text]
propertyAudioObjectCaption = Property "caption"

propertyNutritionInformationCarbohydrateContent ::
  Property NutritionInformation '[Mass]
propertyNutritionInformationCarbohydrateContent =
  Property "carbohydrateContent"

propertyVehicleCargoVolume :: Property Vehicle '[QuantitativeValue]
propertyVehicleCargoVolume = Property "cargoVolume"

propertyParcelDeliveryCarrier ::
  Property ParcelDelivery '[Organization]
propertyParcelDeliveryCarrier = Property "carrier"

propertyFlightCarrier :: Property Flight '[Organization]
propertyFlightCarrier = Property "carrier"

propertyMobileApplicationCarrierRequirements ::
  Property MobileApplication '[Text]
propertyMobileApplicationCarrierRequirements =
  Property "carrierRequirements"

propertyPaymentCardCashBack ::
  Property PaymentCard '[Boolean, Number]
propertyPaymentCardCashBack = Property "cashBack"

propertyDatasetCatalog :: Property Dataset '[DataCatalog]
propertyDatasetCatalog = Property "catalog"

propertyMusicReleaseCatalogNumber :: Property MusicRelease '[Text]
propertyMusicReleaseCatalogNumber = Property "catalogNumber"

propertySpecialAnnouncementCategory ::
  Property
    SpecialAnnouncement
    '[ URL,
       CategoryCode,
       Text,
       Thing,
       PhysicalActivityCategory
     ]
propertySpecialAnnouncementCategory = Property "category"

propertyOfferCategory ::
  Property
    Offer
    '[ URL,
       CategoryCode,
       Text,
       Thing,
       PhysicalActivityCategory
     ]
propertyOfferCategory = Property "category"

propertyActionAccessSpecificationCategory ::
  Property
    ActionAccessSpecification
    '[ URL,
       CategoryCode,
       Text,
       Thing,
       PhysicalActivityCategory
     ]
propertyActionAccessSpecificationCategory = Property "category"

propertyInvoiceCategory ::
  Property
    Invoice
    '[ URL,
       CategoryCode,
       Text,
       Thing,
       PhysicalActivityCategory
     ]
propertyInvoiceCategory = Property "category"

propertyRecommendationCategory ::
  Property
    Recommendation
    '[ URL,
       CategoryCode,
       Text,
       Thing,
       PhysicalActivityCategory
     ]
propertyRecommendationCategory = Property "category"

propertyProductCategory ::
  Property
    Product
    '[ URL,
       CategoryCode,
       Text,
       Thing,
       PhysicalActivityCategory
     ]
propertyProductCategory = Property "category"

propertyServiceCategory ::
  Property
    Service
    '[ URL,
       CategoryCode,
       Text,
       Thing,
       PhysicalActivityCategory
     ]
propertyServiceCategory = Property "category"

propertyPhysicalActivityCategory ::
  Property
    PhysicalActivity
    '[ URL,
       CategoryCode,
       Text,
       Thing,
       PhysicalActivityCategory
     ]
propertyPhysicalActivityCategory = Property "category"

propertyMedicalCauseCauseOf ::
  Property MedicalCause '[MedicalEntity]
propertyMedicalCauseCauseOf = Property "causeOf"

propertyMessageCcRecipient ::
  Property Message '[Organization, ContactPoint, Person]
propertyMessageCcRecipient = Property "ccRecipient"

propertyCreativeWorkCharacter :: Property CreativeWork '[Person]
propertyCreativeWorkCharacter = Property "character"

propertyVideoGameSeriesCharacterAttribute ::
  Property VideoGameSeries '[Thing]
propertyVideoGameSeriesCharacterAttribute =
  Property "characterAttribute"

propertyGameCharacterAttribute :: Property Game '[Thing]
propertyGameCharacterAttribute = Property "characterAttribute"

propertyPerformanceRoleCharacterName ::
  Property PerformanceRole '[Text]
propertyPerformanceRoleCharacterName = Property "characterName"

propertyVideoGameSeriesCheatCode ::
  Property VideoGameSeries '[CreativeWork]
propertyVideoGameSeriesCheatCode = Property "cheatCode"

propertyVideoGameCheatCode :: Property VideoGame '[CreativeWork]
propertyVideoGameCheatCode = Property "cheatCode"

propertyLodgingBusinessCheckinTime ::
  Property LodgingBusiness '[DateTime, Time]
propertyLodgingBusinessCheckinTime = Property "checkinTime"

propertyLodgingReservationCheckinTime ::
  Property LodgingReservation '[DateTime, Time]
propertyLodgingReservationCheckinTime = Property "checkinTime"

propertyOfferCheckoutPageURLTemplate :: Property Offer '[Text]
propertyOfferCheckoutPageURLTemplate =
  Property "checkoutPageURLTemplate"

propertyLodgingBusinessCheckoutTime ::
  Property LodgingBusiness '[DateTime, Time]
propertyLodgingBusinessCheckoutTime = Property "checkoutTime"

propertyLodgingReservationCheckoutTime ::
  Property LodgingReservation '[DateTime, Time]
propertyLodgingReservationCheckoutTime = Property "checkoutTime"

propertyChemicalSubstanceChemicalComposition ::
  Property ChemicalSubstance '[Text]
propertyChemicalSubstanceChemicalComposition =
  Property "chemicalComposition"

propertyMolecularEntityChemicalRole ::
  Property MolecularEntity '[DefinedTerm]
propertyMolecularEntityChemicalRole = Property "chemicalRole"

propertyChemicalSubstanceChemicalRole ::
  Property ChemicalSubstance '[DefinedTerm]
propertyChemicalSubstanceChemicalRole = Property "chemicalRole"

propertyParentAudienceChildMaxAge ::
  Property ParentAudience '[Number]
propertyParentAudienceChildMaxAge = Property "childMaxAge"

propertyParentAudienceChildMinAge ::
  Property ParentAudience '[Number]
propertyParentAudienceChildMinAge = Property "childMinAge"

propertyTaxonChildTaxon :: Property Taxon '[Text, Taxon, URL]
propertyTaxonChildTaxon = Property "childTaxon"

propertyPersonChildren :: Property Person '[Person]
propertyPersonChildren = Property "children"

propertyNutritionInformationCholesterolContent ::
  Property NutritionInformation '[Mass]
propertyNutritionInformationCholesterolContent =
  Property "cholesterolContent"

propertyGeoShapeCircle :: Property GeoShape '[Text]
propertyGeoShapeCircle = Property "circle"

propertyCreativeWorkCitation ::
  Property CreativeWork '[CreativeWork, Text]
propertyCreativeWorkCitation = Property "citation"

propertyClaimClaimInterpreter ::
  Property Claim '[Organization, Person]
propertyClaimClaimInterpreter = Property "claimInterpreter"

propertyClaimReviewClaimReviewed :: Property ClaimReview '[Text]
propertyClaimReviewClaimReviewed = Property "claimReviewed"

propertyDrugClincalPharmacology :: Property Drug '[Text]
propertyDrugClincalPharmacology = Property "clincalPharmacology"

propertyDrugClinicalPharmacology :: Property Drug '[Text]
propertyDrugClinicalPharmacology = Property "clinicalPharmacology"

propertyClipClipNumber :: Property Clip '[Integer, Text]
propertyClipClipNumber = Property "clipNumber"

propertyOpeningHoursSpecificationCloses ::
  Property OpeningHoursSpecification '[Time]
propertyOpeningHoursSpecificationCloses = Property "closes"

propertySportsTeamCoach :: Property SportsTeam '[Person]
propertySportsTeamCoach = Property "coach"

propertyMedicalEntityCode :: Property MedicalEntity '[MedicalCode]
propertyMedicalEntityCode = Property "code"

propertySoftwareSourceCodeCodeRepository ::
  Property SoftwareSourceCode '[URL]
propertySoftwareSourceCodeCodeRepository =
  Property "codeRepository"

propertySoftwareSourceCodeCodeSampleType ::
  Property SoftwareSourceCode '[Text]
propertySoftwareSourceCodeCodeSampleType =
  Property "codeSampleType"

propertyCategoryCodeCodeValue :: Property CategoryCode '[Text]
propertyCategoryCodeCodeValue = Property "codeValue"

propertyMedicalCodeCodeValue :: Property MedicalCode '[Text]
propertyMedicalCodeCodeValue = Property "codeValue"

propertyMedicalCodeCodingSystem :: Property MedicalCode '[Text]
propertyMedicalCodeCodingSystem = Property "codingSystem"

propertyPersonColleague :: Property Person '[Person, URL]
propertyPersonColleague = Property "colleague"

propertyPersonColleagues :: Property Person '[Person]
propertyPersonColleagues = Property "colleagues"

propertyUpdateActionCollection :: Property UpdateAction '[Thing]
propertyUpdateActionCollection = Property "collection"

propertyCollectionCollectionSize :: Property Collection '[Integer]
propertyCollectionCollectionSize = Property "collectionSize"

propertyProductColor :: Property Product '[Text]
propertyProductColor = Property "color"

propertyComicStoryColorist :: Property ComicStory '[Person]
propertyComicStoryColorist = Property "colorist"

propertyComicIssueColorist :: Property ComicIssue '[Person]
propertyComicIssueColorist = Property "colorist"

propertyVisualArtworkColorist :: Property VisualArtwork '[Person]
propertyVisualArtworkColorist = Property "colorist"

propertyCreativeWorkComment :: Property CreativeWork '[Comment]
propertyCreativeWorkComment = Property "comment"

propertyRsvpActionComment :: Property RsvpAction '[Comment]
propertyRsvpActionComment = Property "comment"

propertyCreativeWorkCommentCount ::
  Property CreativeWork '[Integer]
propertyCreativeWorkCommentCount = Property "commentCount"

propertyUserCommentsCommentText :: Property UserComments '[Text]
propertyUserCommentsCommentText = Property "commentText"

propertyUserCommentsCommentTime ::
  Property UserComments '[DateTime, Date]
propertyUserCommentsCommentTime = Property "commentTime"

propertyEducationalOccupationalCredentialCompetencyRequired ::
  Property
    EducationalOccupationalCredential
    '[ Text,
       URL,
       DefinedTerm
     ]
propertyEducationalOccupationalCredentialCompetencyRequired =
  Property "competencyRequired"

propertyLearningResourceCompetencyRequired ::
  Property LearningResource '[Text, URL, DefinedTerm]
propertyLearningResourceCompetencyRequired =
  Property "competencyRequired"

propertySportsEventCompetitor ::
  Property SportsEvent '[SportsTeam, Person]
propertySportsEventCompetitor = Property "competitor"

propertyEventComposer :: Property Event '[Organization, Person]
propertyEventComposer = Property "composer"

propertyMusicCompositionComposer ::
  Property MusicComposition '[Organization, Person]
propertyMusicCompositionComposer = Property "composer"

propertyAnatomicalSystemComprisedOf ::
  Property AnatomicalSystem '[AnatomicalStructure, AnatomicalSystem]
propertyAnatomicalSystemComprisedOf = Property "comprisedOf"

propertyCreativeWorkConditionsOfAccess ::
  Property CreativeWork '[Text]
propertyCreativeWorkConditionsOfAccess =
  Property "conditionsOfAccess"

propertyInvoiceConfirmationNumber :: Property Invoice '[Text]
propertyInvoiceConfirmationNumber = Property "confirmationNumber"

propertyOrderConfirmationNumber :: Property Order '[Text]
propertyOrderConfirmationNumber = Property "confirmationNumber"

propertyAnatomicalStructureConnectedTo ::
  Property AnatomicalStructure '[AnatomicalStructure]
propertyAnatomicalStructureConnectedTo = Property "connectedTo"

propertyStatisticalPopulationConstrainingProperty ::
  Property StatisticalPopulation '[Integer]
propertyStatisticalPopulationConstrainingProperty =
  Property "constrainingProperty"

propertyContactPointContactOption ::
  Property ContactPoint '[ContactPointOption]
propertyContactPointContactOption = Property "contactOption"

propertyOrganizationContactPoint ::
  Property Organization '[ContactPoint]
propertyOrganizationContactPoint = Property "contactPoint"

propertyPersonContactPoint :: Property Person '[ContactPoint]
propertyPersonContactPoint = Property "contactPoint"

propertyHealthInsurancePlanContactPoint ::
  Property HealthInsurancePlan '[ContactPoint]
propertyHealthInsurancePlanContactPoint = Property "contactPoint"

propertyOrganizationContactPoints ::
  Property Organization '[ContactPoint]
propertyOrganizationContactPoints = Property "contactPoints"

propertyPersonContactPoints :: Property Person '[ContactPoint]
propertyPersonContactPoints = Property "contactPoints"

propertyContactPointContactType :: Property ContactPoint '[Text]
propertyContactPointContactType = Property "contactType"

propertyPaymentCardContactlessPayment ::
  Property PaymentCard '[Boolean]
propertyPaymentCardContactlessPayment =
  Property "contactlessPayment"

propertyPlaceContainedIn :: Property Place '[Place]
propertyPlaceContainedIn = Property "containedIn"

propertyPlaceContainedInPlace :: Property Place '[Place]
propertyPlaceContainedInPlace = Property "containedInPlace"

propertyPlaceContainsPlace :: Property Place '[Place]
propertyPlaceContainsPlace = Property "containsPlace"

propertyTVSeriesContainsSeason ::
  Property TVSeries '[CreativeWorkSeason]
propertyTVSeriesContainsSeason = Property "containsSeason"

propertyRadioSeriesContainsSeason ::
  Property RadioSeries '[CreativeWorkSeason]
propertyRadioSeriesContainsSeason = Property "containsSeason"

propertyVideoGameSeriesContainsSeason ::
  Property VideoGameSeries '[CreativeWorkSeason]
propertyVideoGameSeriesContainsSeason = Property "containsSeason"

propertyCreativeWorkContentLocation ::
  Property CreativeWork '[Place]
propertyCreativeWorkContentLocation = Property "contentLocation"

propertyCreativeWorkContentRating ::
  Property CreativeWork '[Text, Rating]
propertyCreativeWorkContentRating = Property "contentRating"

propertyCreativeWorkContentReferenceTime ::
  Property CreativeWork '[DateTime]
propertyCreativeWorkContentReferenceTime =
  Property "contentReferenceTime"

propertyMediaObjectContentSize :: Property MediaObject '[Text]
propertyMediaObjectContentSize = Property "contentSize"

propertyEntryPointContentType :: Property EntryPoint '[Text]
propertyEntryPointContentType = Property "contentType"

propertyMediaObjectContentUrl :: Property MediaObject '[URL]
propertyMediaObjectContentUrl = Property "contentUrl"

propertyMedicalDeviceContraindication ::
  Property MedicalDevice '[Text, MedicalContraindication]
propertyMedicalDeviceContraindication = Property "contraindication"

propertyMedicalTherapyContraindication ::
  Property MedicalTherapy '[Text, MedicalContraindication]
propertyMedicalTherapyContraindication =
  Property "contraindication"

propertyEventContributor :: Property Event '[Organization, Person]
propertyEventContributor = Property "contributor"

propertyCreativeWorkContributor ::
  Property CreativeWork '[Organization, Person]
propertyCreativeWorkContributor = Property "contributor"

propertyRecipeCookTime :: Property Recipe '[Duration]
propertyRecipeCookTime = Property "cookTime"

propertyRecipeCookingMethod :: Property Recipe '[Text]
propertyRecipeCookingMethod = Property "cookingMethod"

propertyCreativeWorkCopyrightHolder ::
  Property CreativeWork '[Organization, Person]
propertyCreativeWorkCopyrightHolder = Property "copyrightHolder"

propertyCreativeWorkCopyrightNotice ::
  Property CreativeWork '[Text]
propertyCreativeWorkCopyrightNotice = Property "copyrightNotice"

propertyCreativeWorkCopyrightYear ::
  Property CreativeWork '[Number]
propertyCreativeWorkCopyrightYear = Property "copyrightYear"

propertyCreativeWorkCorrection ::
  Property CreativeWork '[URL, Text, CorrectionComment]
propertyCreativeWorkCorrection = Property "correction"

propertyNewsMediaOrganizationCorrectionsPolicy ::
  Property NewsMediaOrganization '[URL, CreativeWork]
propertyNewsMediaOrganizationCorrectionsPolicy =
  Property "correctionsPolicy"

propertyOrganizationCorrectionsPolicy ::
  Property Organization '[URL, CreativeWork]
propertyOrganizationCorrectionsPolicy =
  Property "correctionsPolicy"

propertyDrugCostCostCategory ::
  Property DrugCost '[DrugCostCategory]
propertyDrugCostCostCategory = Property "costCategory"

propertyDrugCostCostCurrency :: Property DrugCost '[Text]
propertyDrugCostCostCurrency = Property "costCurrency"

propertyDrugCostCostOrigin :: Property DrugCost '[Text]
propertyDrugCostCostOrigin = Property "costOrigin"

propertyDrugCostCostPerUnit ::
  Property DrugCost '[Text, QualitativeValue, Number]
propertyDrugCostCostPerUnit = Property "costPerUnit"

propertySoftwareApplicationCountriesNotSupported ::
  Property SoftwareApplication '[Text]
propertySoftwareApplicationCountriesNotSupported =
  Property "countriesNotSupported"

propertySoftwareApplicationCountriesSupported ::
  Property SoftwareApplication '[Text]
propertySoftwareApplicationCountriesSupported =
  Property "countriesSupported"

propertyProductCountryOfAssembly :: Property Product '[Text]
propertyProductCountryOfAssembly = Property "countryOfAssembly"

propertyProductCountryOfLastProcessing :: Property Product '[Text]
propertyProductCountryOfLastProcessing =
  Property "countryOfLastProcessing"

propertyTVSeriesCountryOfOrigin :: Property TVSeries '[Country]
propertyTVSeriesCountryOfOrigin = Property "countryOfOrigin"

propertyTVSeasonCountryOfOrigin :: Property TVSeason '[Country]
propertyTVSeasonCountryOfOrigin = Property "countryOfOrigin"

propertyCreativeWorkCountryOfOrigin ::
  Property CreativeWork '[Country]
propertyCreativeWorkCountryOfOrigin = Property "countryOfOrigin"

propertyMovieCountryOfOrigin :: Property Movie '[Country]
propertyMovieCountryOfOrigin = Property "countryOfOrigin"

propertyTVEpisodeCountryOfOrigin :: Property TVEpisode '[Country]
propertyTVEpisodeCountryOfOrigin = Property "countryOfOrigin"

propertyProductCountryOfOrigin :: Property Product '[Country]
propertyProductCountryOfOrigin = Property "countryOfOrigin"

propertyExerciseActionCourse :: Property ExerciseAction '[Place]
propertyExerciseActionCourse = Property "course"

propertyCourseCourseCode :: Property Course '[Text]
propertyCourseCourseCode = Property "courseCode"

propertyCourseInstanceCourseMode ::
  Property CourseInstance '[Text, URL]
propertyCourseInstanceCourseMode = Property "courseMode"

propertyCourseCoursePrerequisites ::
  Property Course '[Course, AlignmentObject, Text]
propertyCourseCoursePrerequisites = Property "coursePrerequisites"

propertyCourseInstanceCourseWorkload ::
  Property CourseInstance '[Text]
propertyCourseInstanceCourseWorkload = Property "courseWorkload"

propertyLiveBlogPostingCoverageEndTime ::
  Property LiveBlogPosting '[DateTime]
propertyLiveBlogPostingCoverageEndTime = Property "coverageEndTime"

propertyLiveBlogPostingCoverageStartTime ::
  Property LiveBlogPosting '[DateTime]
propertyLiveBlogPostingCoverageStartTime =
  Property "coverageStartTime"

propertyCreativeWorkCreativeWorkStatus ::
  Property CreativeWork '[DefinedTerm, Text]
propertyCreativeWorkCreativeWorkStatus =
  Property "creativeWorkStatus"

propertyCreativeWorkCreator ::
  Property CreativeWork '[Person, Organization]
propertyCreativeWorkCreator = Property "creator"

propertyUserCommentsCreator ::
  Property UserComments '[Person, Organization]
propertyUserCommentsCreator = Property "creator"

propertyEducationalOccupationalCredentialCredentialCategory ::
  Property
    EducationalOccupationalCredential
    '[ URL,
       DefinedTerm,
       Text
     ]
propertyEducationalOccupationalCredentialCredentialCategory =
  Property "credentialCategory"

propertyCreativeWorkCreditText :: Property CreativeWork '[Text]
propertyCreativeWorkCreditText = Property "creditText"

propertyMusicReleaseCreditedTo ::
  Property MusicRelease '[Person, Organization]
propertyMusicReleaseCreditedTo = Property "creditedTo"

propertySpeakableSpecificationCssSelector ::
  Property SpeakableSpecification '[CssSelectorType]
propertySpeakableSpecificationCssSelector = Property "cssSelector"

propertyWebPageElementCssSelector ::
  Property WebPageElement '[CssSelectorType]
propertyWebPageElementCssSelector = Property "cssSelector"

propertyLocalBusinessCurrenciesAccepted ::
  Property LocalBusiness '[Text]
propertyLocalBusinessCurrenciesAccepted =
  Property "currenciesAccepted"

propertyLoanOrCreditCurrency :: Property LoanOrCredit '[Text]
propertyLoanOrCreditCurrency = Property "currency"

propertyMonetaryAmountCurrency :: Property MonetaryAmount '[Text]
propertyMonetaryAmountCurrency = Property "currency"

propertyExchangeRateSpecificationCurrency ::
  Property ExchangeRateSpecification '[Text]
propertyExchangeRateSpecificationCurrency = Property "currency"

propertyDatedMoneySpecificationCurrency ::
  Property DatedMoneySpecification '[Text]
propertyDatedMoneySpecificationCurrency = Property "currency"

propertyMonetaryAmountDistributionCurrency ::
  Property MonetaryAmountDistribution '[Text]
propertyMonetaryAmountDistributionCurrency = Property "currency"

propertyExchangeRateSpecificationCurrentExchangeRate ::
  Property ExchangeRateSpecification '[UnitPriceSpecification]
propertyExchangeRateSpecificationCurrentExchangeRate =
  Property "currentExchangeRate"

propertyInvoiceCustomer :: Property Invoice '[Organization, Person]
propertyInvoiceCustomer = Property "customer"

propertyOrderCustomer :: Property Order '[Organization, Person]
propertyOrderCustomer = Property "customer"

propertyMerchantReturnPolicyCustomerRemorseReturnFees ::
  Property MerchantReturnPolicy '[ReturnFeesEnumeration]
propertyMerchantReturnPolicyCustomerRemorseReturnFees =
  Property "customerRemorseReturnFees"

propertyMerchantReturnPolicyCustomerRemorseReturnLabelSource ::
  Property MerchantReturnPolicy '[ReturnLabelSourceEnumeration]
propertyMerchantReturnPolicyCustomerRemorseReturnLabelSource =
  Property "customerRemorseReturnLabelSource"

propertyMerchantReturnPolicyCustomerRemorseReturnShippingFeesAmount ::
  Property MerchantReturnPolicy '[MonetaryAmount]
propertyMerchantReturnPolicyCustomerRemorseReturnShippingFeesAmount =
  Property "customerRemorseReturnShippingFeesAmount"

propertyShippingDeliveryTimeCutoffTime ::
  Property ShippingDeliveryTime '[Time]
propertyShippingDeliveryTimeCutoffTime = Property "cutoffTime"

propertyCDCPMDRecordCvdCollectionDate ::
  Property CDCPMDRecord '[DateTime, Text]
propertyCDCPMDRecordCvdCollectionDate =
  Property "cvdCollectionDate"

propertyCDCPMDRecordCvdFacilityCounty ::
  Property CDCPMDRecord '[Text]
propertyCDCPMDRecordCvdFacilityCounty =
  Property "cvdFacilityCounty"

propertyCDCPMDRecordCvdFacilityId :: Property CDCPMDRecord '[Text]
propertyCDCPMDRecordCvdFacilityId = Property "cvdFacilityId"

propertyCDCPMDRecordCvdNumBeds :: Property CDCPMDRecord '[Number]
propertyCDCPMDRecordCvdNumBeds = Property "cvdNumBeds"

propertyCDCPMDRecordCvdNumBedsOcc ::
  Property CDCPMDRecord '[Number]
propertyCDCPMDRecordCvdNumBedsOcc = Property "cvdNumBedsOcc"

propertyCDCPMDRecordCvdNumC19Died ::
  Property CDCPMDRecord '[Number]
propertyCDCPMDRecordCvdNumC19Died = Property "cvdNumC19Died"

propertyCDCPMDRecordCvdNumC19HOPats ::
  Property CDCPMDRecord '[Number]
propertyCDCPMDRecordCvdNumC19HOPats = Property "cvdNumC19HOPats"

propertyCDCPMDRecordCvdNumC19HospPats ::
  Property CDCPMDRecord '[Number]
propertyCDCPMDRecordCvdNumC19HospPats =
  Property "cvdNumC19HospPats"

propertyCDCPMDRecordCvdNumC19MechVentPats ::
  Property CDCPMDRecord '[Number]
propertyCDCPMDRecordCvdNumC19MechVentPats =
  Property "cvdNumC19MechVentPats"

propertyCDCPMDRecordCvdNumC19OFMechVentPats ::
  Property CDCPMDRecord '[Number]
propertyCDCPMDRecordCvdNumC19OFMechVentPats =
  Property "cvdNumC19OFMechVentPats"

propertyCDCPMDRecordCvdNumC19OverflowPats ::
  Property CDCPMDRecord '[Number]
propertyCDCPMDRecordCvdNumC19OverflowPats =
  Property "cvdNumC19OverflowPats"

propertyCDCPMDRecordCvdNumICUBeds ::
  Property CDCPMDRecord '[Number]
propertyCDCPMDRecordCvdNumICUBeds = Property "cvdNumICUBeds"

propertyCDCPMDRecordCvdNumICUBedsOcc ::
  Property CDCPMDRecord '[Number]
propertyCDCPMDRecordCvdNumICUBedsOcc = Property "cvdNumICUBedsOcc"

propertyCDCPMDRecordCvdNumTotBeds ::
  Property CDCPMDRecord '[Number]
propertyCDCPMDRecordCvdNumTotBeds = Property "cvdNumTotBeds"

propertyCDCPMDRecordCvdNumVent :: Property CDCPMDRecord '[Number]
propertyCDCPMDRecordCvdNumVent = Property "cvdNumVent"

propertyCDCPMDRecordCvdNumVentUse ::
  Property CDCPMDRecord '[Number]
propertyCDCPMDRecordCvdNumVentUse = Property "cvdNumVentUse"

propertyDataFeedDataFeedElement ::
  Property DataFeed '[Text, Thing, DataFeedItem]
propertyDataFeedDataFeedElement = Property "dataFeedElement"

propertyDataCatalogDataset :: Property DataCatalog '[Dataset]
propertyDataCatalogDataset = Property "dataset"

propertyDatasetDatasetTimeInterval :: Property Dataset '[DateTime]
propertyDatasetDatasetTimeInterval = Property "datasetTimeInterval"

propertyCreativeWorkDateCreated ::
  Property CreativeWork '[DateTime, Date]
propertyCreativeWorkDateCreated = Property "dateCreated"

propertyDataFeedItemDateCreated ::
  Property DataFeedItem '[DateTime, Date]
propertyDataFeedItemDateCreated = Property "dateCreated"

propertyDataFeedItemDateDeleted ::
  Property DataFeedItem '[Date, DateTime]
propertyDataFeedItemDateDeleted = Property "dateDeleted"

propertyTicketDateIssued :: Property Ticket '[Date, DateTime]
propertyTicketDateIssued = Property "dateIssued"

propertyCreativeWorkDateModified ::
  Property CreativeWork '[DateTime, Date]
propertyCreativeWorkDateModified = Property "dateModified"

propertyDataFeedItemDateModified ::
  Property DataFeedItem '[DateTime, Date]
propertyDataFeedItemDateModified = Property "dateModified"

propertySpecialAnnouncementDatePosted ::
  Property SpecialAnnouncement '[DateTime, Date]
propertySpecialAnnouncementDatePosted = Property "datePosted"

propertyJobPostingDatePosted ::
  Property JobPosting '[DateTime, Date]
propertyJobPostingDatePosted = Property "datePosted"

propertyCDCPMDRecordDatePosted ::
  Property CDCPMDRecord '[DateTime, Date]
propertyCDCPMDRecordDatePosted = Property "datePosted"

propertyRealEstateListingDatePosted ::
  Property RealEstateListing '[DateTime, Date]
propertyRealEstateListingDatePosted = Property "datePosted"

propertyCreativeWorkDatePublished ::
  Property CreativeWork '[DateTime, Date]
propertyCreativeWorkDatePublished = Property "datePublished"

propertyMessageDateRead :: Property Message '[DateTime, Date]
propertyMessageDateRead = Property "dateRead"

propertyMessageDateReceived :: Property Message '[DateTime]
propertyMessageDateReceived = Property "dateReceived"

propertyMessageDateSent :: Property Message '[DateTime]
propertyMessageDateSent = Property "dateSent"

propertyVehicleDateVehicleFirstRegistered ::
  Property Vehicle '[Date]
propertyVehicleDateVehicleFirstRegistered =
  Property "dateVehicleFirstRegistered"

propertyNewsArticleDateline :: Property NewsArticle '[Text]
propertyNewsArticleDateline = Property "dateline"

propertyEducationalOccupationalProgramDayOfWeek ::
  Property EducationalOccupationalProgram '[DayOfWeek]
propertyEducationalOccupationalProgramDayOfWeek =
  Property "dayOfWeek"

propertyOpeningHoursSpecificationDayOfWeek ::
  Property OpeningHoursSpecification '[DayOfWeek]
propertyOpeningHoursSpecificationDayOfWeek = Property "dayOfWeek"

propertyPersonDeathDate :: Property Person '[Date]
propertyPersonDeathDate = Property "deathDate"

propertyPersonDeathPlace :: Property Person '[Place]
propertyPersonDeathPlace = Property "deathPlace"

propertyPropertyValueSpecificationDefaultValue ::
  Property PropertyValueSpecification '[Thing, Text]
propertyPropertyValueSpecificationDefaultValue =
  Property "defaultValue"

propertyParcelDeliveryDeliveryAddress ::
  Property ParcelDelivery '[PostalAddress]
propertyParcelDeliveryDeliveryAddress = Property "deliveryAddress"

propertyOfferDeliveryLeadTime ::
  Property Offer '[QuantitativeValue]
propertyOfferDeliveryLeadTime = Property "deliveryLeadTime"

propertyDemandDeliveryLeadTime ::
  Property Demand '[QuantitativeValue]
propertyDemandDeliveryLeadTime = Property "deliveryLeadTime"

propertyTrackActionDeliveryMethod ::
  Property TrackAction '[DeliveryMethod]
propertyTrackActionDeliveryMethod = Property "deliveryMethod"

propertyReceiveActionDeliveryMethod ::
  Property ReceiveAction '[DeliveryMethod]
propertyReceiveActionDeliveryMethod = Property "deliveryMethod"

propertySendActionDeliveryMethod ::
  Property SendAction '[DeliveryMethod]
propertySendActionDeliveryMethod = Property "deliveryMethod"

propertyOrderActionDeliveryMethod ::
  Property OrderAction '[DeliveryMethod]
propertyOrderActionDeliveryMethod = Property "deliveryMethod"

propertyParcelDeliveryDeliveryStatus ::
  Property ParcelDelivery '[DeliveryEvent]
propertyParcelDeliveryDeliveryStatus = Property "deliveryStatus"

propertyOfferShippingDetailsDeliveryTime ::
  Property OfferShippingDetails '[ShippingDeliveryTime]
propertyOfferShippingDetailsDeliveryTime = Property "deliveryTime"

propertyDeliveryTimeSettingsDeliveryTime ::
  Property DeliveryTimeSettings '[ShippingDeliveryTime]
propertyDeliveryTimeSettingsDeliveryTime = Property "deliveryTime"

propertyOrganizationDepartment ::
  Property Organization '[Organization]
propertyOrganizationDepartment = Property "department"

propertyFlightDepartureAirport :: Property Flight '[Airport]
propertyFlightDepartureAirport = Property "departureAirport"

propertyBoatTripDepartureBoatTerminal ::
  Property BoatTrip '[BoatTerminal]
propertyBoatTripDepartureBoatTerminal =
  Property "departureBoatTerminal"

propertyBusTripDepartureBusStop ::
  Property BusTrip '[BusStop, BusStation]
propertyBusTripDepartureBusStop = Property "departureBusStop"

propertyFlightDepartureGate :: Property Flight '[Text]
propertyFlightDepartureGate = Property "departureGate"

propertyTrainTripDeparturePlatform :: Property TrainTrip '[Text]
propertyTrainTripDeparturePlatform = Property "departurePlatform"

propertyTrainTripDepartureStation ::
  Property TrainTrip '[TrainStation]
propertyTrainTripDepartureStation = Property "departureStation"

propertyFlightDepartureTerminal :: Property Flight '[Text]
propertyFlightDepartureTerminal = Property "departureTerminal"

propertyTripDepartureTime :: Property Trip '[Time, DateTime]
propertyTripDepartureTime = Property "departureTime"

propertyTechArticleDependencies :: Property TechArticle '[Text]
propertyTechArticleDependencies = Property "dependencies"

propertyVisualArtworkDepth ::
  Property VisualArtwork '[QuantitativeValue, Distance]
propertyVisualArtworkDepth = Property "depth"

propertyProductDepth ::
  Property Product '[QuantitativeValue, Distance]
propertyProductDepth = Property "depth"

propertyOfferShippingDetailsDepth ::
  Property OfferShippingDetails '[QuantitativeValue, Distance]
propertyOfferShippingDetailsDepth = Property "depth"

propertyThingDescription :: Property Thing '[Text]
propertyThingDescription = Property "description"

propertySoftwareApplicationDevice ::
  Property SoftwareApplication '[Text]
propertySoftwareApplicationDevice = Property "device"

propertyDDxElementDiagnosis ::
  Property DDxElement '[MedicalCondition]
propertyDDxElementDiagnosis = Property "diagnosis"

propertyPatientDiagnosis :: Property Patient '[MedicalCondition]
propertyPatientDiagnosis = Property "diagnosis"

propertyAnatomicalStructureDiagram ::
  Property AnatomicalStructure '[ImageObject]
propertyAnatomicalStructureDiagram = Property "diagram"

propertyExerciseActionDiet :: Property ExerciseAction '[Diet]
propertyExerciseActionDiet = Property "diet"

propertyDietDietFeatures :: Property Diet '[Text]
propertyDietDietFeatures = Property "dietFeatures"

propertyMedicalConditionDifferentialDiagnosis ::
  Property MedicalCondition '[DDxElement]
propertyMedicalConditionDifferentialDiagnosis =
  Property "differentialDiagnosis"

propertyJobPostingDirectApply :: Property JobPosting '[Boolean]
propertyJobPostingDirectApply = Property "directApply"

propertyTVSeriesDirector :: Property TVSeries '[Person]
propertyTVSeriesDirector = Property "director"

propertyClipDirector :: Property Clip '[Person]
propertyClipDirector = Property "director"

propertyEventDirector :: Property Event '[Person]
propertyEventDirector = Property "director"

propertyCreativeWorkSeasonDirector ::
  Property CreativeWorkSeason '[Person]
propertyCreativeWorkSeasonDirector = Property "director"

propertyMovieDirector :: Property Movie '[Person]
propertyMovieDirector = Property "director"

propertyMovieSeriesDirector :: Property MovieSeries '[Person]
propertyMovieSeriesDirector = Property "director"

propertyRadioSeriesDirector :: Property RadioSeries '[Person]
propertyRadioSeriesDirector = Property "director"

propertyEpisodeDirector :: Property Episode '[Person]
propertyEpisodeDirector = Property "director"

propertyVideoGameDirector :: Property VideoGame '[Person]
propertyVideoGameDirector = Property "director"

propertyVideoGameSeriesDirector ::
  Property VideoGameSeries '[Person]
propertyVideoGameSeriesDirector = Property "director"

propertyVideoObjectDirector :: Property VideoObject '[Person]
propertyVideoObjectDirector = Property "director"

propertyVideoObjectDirectors :: Property VideoObject '[Person]
propertyVideoObjectDirectors = Property "directors"

propertyTVSeriesDirectors :: Property TVSeries '[Person]
propertyTVSeriesDirectors = Property "directors"

propertyClipDirectors :: Property Clip '[Person]
propertyClipDirectors = Property "directors"

propertyMovieDirectors :: Property Movie '[Person]
propertyMovieDirectors = Property "directors"

propertyMovieSeriesDirectors :: Property MovieSeries '[Person]
propertyMovieSeriesDirectors = Property "directors"

propertyRadioSeriesDirectors :: Property RadioSeries '[Person]
propertyRadioSeriesDirectors = Property "directors"

propertyEpisodeDirectors :: Property Episode '[Person]
propertyEpisodeDirectors = Property "directors"

propertyVideoGameDirectors :: Property VideoGame '[Person]
propertyVideoGameDirectors = Property "directors"

propertyVideoGameSeriesDirectors ::
  Property VideoGameSeries '[Person]
propertyVideoGameSeriesDirectors = Property "directors"

propertyThingDisambiguatingDescription :: Property Thing '[Text]
propertyThingDisambiguatingDescription =
  Property "disambiguatingDescription"

propertyOrderDiscount :: Property Order '[Number, Text]
propertyOrderDiscount = Property "discount"

propertyOrderDiscountCode :: Property Order '[Text]
propertyOrderDiscountCode = Property "discountCode"

propertyOrderDiscountCurrency :: Property Order '[Text]
propertyOrderDiscountCurrency = Property "discountCurrency"

propertyUserCommentsDiscusses ::
  Property UserComments '[CreativeWork]
propertyUserCommentsDiscusses = Property "discusses"

propertyCreativeWorkDiscussionUrl :: Property CreativeWork '[URL]
propertyCreativeWorkDiscussionUrl = Property "discussionUrl"

propertySpecialAnnouncementDiseasePreventionInfo ::
  Property SpecialAnnouncement '[WebContent, URL]
propertySpecialAnnouncementDiseasePreventionInfo =
  Property "diseasePreventionInfo"

propertySpecialAnnouncementDiseaseSpreadStatistics ::
  Property
    SpecialAnnouncement
    '[ WebContent,
       Observation,
       Dataset,
       URL
     ]
propertySpecialAnnouncementDiseaseSpreadStatistics =
  Property "diseaseSpreadStatistics"

propertyOrganizationDissolutionDate ::
  Property Organization '[Date]
propertyOrganizationDissolutionDate = Property "dissolutionDate"

propertyExerciseActionDistance ::
  Property ExerciseAction '[Distance]
propertyExerciseActionDistance = Property "distance"

propertyTravelActionDistance :: Property TravelAction '[Distance]
propertyTravelActionDistance = Property "distance"

propertyDDxElementDistinguishingSign ::
  Property DDxElement '[MedicalSignOrSymptom]
propertyDDxElementDistinguishingSign =
  Property "distinguishingSign"

propertyDatasetDistribution :: Property Dataset '[DataDownload]
propertyDatasetDistribution = Property "distribution"

propertyNewsMediaOrganizationDiversityPolicy ::
  Property NewsMediaOrganization '[URL, CreativeWork]
propertyNewsMediaOrganizationDiversityPolicy =
  Property "diversityPolicy"

propertyOrganizationDiversityPolicy ::
  Property Organization '[URL, CreativeWork]
propertyOrganizationDiversityPolicy = Property "diversityPolicy"

propertyNewsMediaOrganizationDiversityStaffingReport ::
  Property NewsMediaOrganization '[Article, URL]
propertyNewsMediaOrganizationDiversityStaffingReport =
  Property "diversityStaffingReport"

propertyOrganizationDiversityStaffingReport ::
  Property Organization '[Article, URL]
propertyOrganizationDiversityStaffingReport =
  Property "diversityStaffingReport"

propertyWebAPIDocumentation :: Property WebAPI '[URL, CreativeWork]
propertyWebAPIDocumentation = Property "documentation"

propertyShippingRateSettingsDoesNotShip ::
  Property ShippingRateSettings '[Boolean]
propertyShippingRateSettingsDoesNotShip = Property "doesNotShip"

propertyOfferShippingDetailsDoesNotShip ::
  Property OfferShippingDetails '[Boolean]
propertyOfferShippingDetailsDoesNotShip = Property "doesNotShip"

propertyMortgageLoanDomiciledMortgage ::
  Property MortgageLoan '[Boolean]
propertyMortgageLoanDomiciledMortgage =
  Property "domiciledMortgage"

propertyEventDoorTime :: Property Event '[Time, DateTime]
propertyEventDoorTime = Property "doorTime"

propertyDrugDosageForm :: Property Drug '[Text]
propertyDrugDosageForm = Property "dosageForm"

propertyDrugDoseSchedule :: Property Drug '[DoseSchedule]
propertyDrugDoseSchedule = Property "doseSchedule"

propertyTherapeuticProcedureDoseSchedule ::
  Property TherapeuticProcedure '[DoseSchedule]
propertyTherapeuticProcedureDoseSchedule = Property "doseSchedule"

propertyDoseScheduleDoseUnit :: Property DoseSchedule '[Text]
propertyDoseScheduleDoseUnit = Property "doseUnit"

propertyDoseScheduleDoseValue ::
  Property DoseSchedule '[Number, QualitativeValue]
propertyDoseScheduleDoseValue = Property "doseValue"

propertyRepaymentSpecificationDownPayment ::
  Property RepaymentSpecification '[Number, MonetaryAmount]
propertyRepaymentSpecificationDownPayment = Property "downPayment"

propertySoftwareApplicationDownloadUrl ::
  Property SoftwareApplication '[URL]
propertySoftwareApplicationDownloadUrl = Property "downloadUrl"

propertyCommentDownvoteCount :: Property Comment '[Integer]
propertyCommentDownvoteCount = Property "downvoteCount"

propertyVeinDrainsTo :: Property Vein '[Vessel]
propertyVeinDrainsTo = Property "drainsTo"

propertyVehicleDriveWheelConfiguration ::
  Property Vehicle '[Text, DriveWheelConfigurationValue]
propertyVehicleDriveWheelConfiguration =
  Property "driveWheelConfiguration"

propertyRentalCarReservationDropoffLocation ::
  Property RentalCarReservation '[Place]
propertyRentalCarReservationDropoffLocation =
  Property "dropoffLocation"

propertyRentalCarReservationDropoffTime ::
  Property RentalCarReservation '[DateTime]
propertyRentalCarReservationDropoffTime = Property "dropoffTime"

propertyTherapeuticProcedureDrug ::
  Property TherapeuticProcedure '[Drug]
propertyTherapeuticProcedureDrug = Property "drug"

propertyMedicalConditionDrug :: Property MedicalCondition '[Drug]
propertyMedicalConditionDrug = Property "drug"

propertyDrugClassDrug :: Property DrugClass '[Drug]
propertyDrugClassDrug = Property "drug"

propertyPatientDrug :: Property Patient '[Drug]
propertyPatientDrug = Property "drug"

propertyDrugDrugClass :: Property Drug '[DrugClass]
propertyDrugDrugClass = Property "drugClass"

propertyDrugDrugUnit :: Property Drug '[Text]
propertyDrugDrugUnit = Property "drugUnit"

propertyDrugCostDrugUnit :: Property DrugCost '[Text]
propertyDrugCostDrugUnit = Property "drugUnit"

propertyPersonDuns :: Property Person '[Text]
propertyPersonDuns = Property "duns"

propertyOrganizationDuns :: Property Organization '[Text]
propertyOrganizationDuns = Property "duns"

propertyMedicalTherapyDuplicateTherapy ::
  Property MedicalTherapy '[MedicalTherapy]
propertyMedicalTherapyDuplicateTherapy =
  Property "duplicateTherapy"

propertyMediaObjectDuration :: Property MediaObject '[Duration]
propertyMediaObjectDuration = Property "duration"

propertyAudiobookDuration :: Property Audiobook '[Duration]
propertyAudiobookDuration = Property "duration"

propertyMusicRecordingDuration ::
  Property MusicRecording '[Duration]
propertyMusicRecordingDuration = Property "duration"

propertyMovieDuration :: Property Movie '[Duration]
propertyMovieDuration = Property "duration"

propertyEpisodeDuration :: Property Episode '[Duration]
propertyEpisodeDuration = Property "duration"

propertyScheduleDuration :: Property Schedule '[Duration]
propertyScheduleDuration = Property "duration"

propertyEventDuration :: Property Event '[Duration]
propertyEventDuration = Property "duration"

propertyMusicReleaseDuration :: Property MusicRelease '[Duration]
propertyMusicReleaseDuration = Property "duration"

propertyQuantitativeValueDistributionDuration ::
  Property QuantitativeValueDistribution '[Duration]
propertyQuantitativeValueDistributionDuration = Property "duration"

propertyWarrantyPromiseDurationOfWarranty ::
  Property WarrantyPromise '[QuantitativeValue]
propertyWarrantyPromiseDurationOfWarranty =
  Property "durationOfWarranty"

propertyHowToDirectionDuringMedia ::
  Property HowToDirection '[MediaObject, URL]
propertyHowToDirectionDuringMedia = Property "duringMedia"

propertyRepaymentSpecificationEarlyPrepaymentPenalty ::
  Property RepaymentSpecification '[MonetaryAmount]
propertyRepaymentSpecificationEarlyPrepaymentPenalty =
  Property "earlyPrepaymentPenalty"

propertyCreativeWorkEditEIDR :: Property CreativeWork '[URL, Text]
propertyCreativeWorkEditEIDR = Property "editEIDR"

propertyCreativeWorkEditor :: Property CreativeWork '[Person]
propertyCreativeWorkEditor = Property "editor"

propertySolveMathActionEduQuestionType ::
  Property SolveMathAction '[Text]
propertySolveMathActionEduQuestionType = Property "eduQuestionType"

propertyQuestionEduQuestionType :: Property Question '[Text]
propertyQuestionEduQuestionType = Property "eduQuestionType"

propertyJobPostingEducationRequirements ::
  Property JobPosting '[EducationalOccupationalCredential, Text]
propertyJobPostingEducationRequirements =
  Property "educationRequirements"

propertyOccupationEducationRequirements ::
  Property Occupation '[EducationalOccupationalCredential, Text]
propertyOccupationEducationRequirements =
  Property "educationRequirements"

propertyLearningResourceEducationalAlignment ::
  Property LearningResource '[AlignmentObject]
propertyLearningResourceEducationalAlignment =
  Property "educationalAlignment"

propertyCreativeWorkEducationalAlignment ::
  Property CreativeWork '[AlignmentObject]
propertyCreativeWorkEducationalAlignment =
  Property "educationalAlignment"

propertyEducationalOccupationalProgramEducationalCredentialAwarded ::
  Property
    EducationalOccupationalProgram
    '[ URL,
       EducationalOccupationalCredential,
       Text
     ]
propertyEducationalOccupationalProgramEducationalCredentialAwarded =
  Property "educationalCredentialAwarded"

propertyCourseEducationalCredentialAwarded ::
  Property Course '[URL, EducationalOccupationalCredential, Text]
propertyCourseEducationalCredentialAwarded =
  Property "educationalCredentialAwarded"

propertyAlignmentObjectEducationalFramework ::
  Property AlignmentObject '[Text]
propertyAlignmentObjectEducationalFramework =
  Property "educationalFramework"

propertyEducationalOccupationalCredentialEducationalLevel ::
  Property
    EducationalOccupationalCredential
    '[ Text,
       URL,
       DefinedTerm
     ]
propertyEducationalOccupationalCredentialEducationalLevel =
  Property "educationalLevel"

propertyCreativeWorkEducationalLevel ::
  Property CreativeWork '[Text, URL, DefinedTerm]
propertyCreativeWorkEducationalLevel = Property "educationalLevel"

propertyEducationEventEducationalLevel ::
  Property EducationEvent '[Text, URL, DefinedTerm]
propertyEducationEventEducationalLevel =
  Property "educationalLevel"

propertyLearningResourceEducationalLevel ::
  Property LearningResource '[Text, URL, DefinedTerm]
propertyLearningResourceEducationalLevel =
  Property "educationalLevel"

propertyEducationalOccupationalProgramEducationalProgramMode ::
  Property EducationalOccupationalProgram '[Text, URL]
propertyEducationalOccupationalProgramEducationalProgramMode =
  Property "educationalProgramMode"

propertyEducationalAudienceEducationalRole ::
  Property EducationalAudience '[Text]
propertyEducationalAudienceEducationalRole =
  Property "educationalRole"

propertyCreativeWorkEducationalUse ::
  Property CreativeWork '[DefinedTerm, Text]
propertyCreativeWorkEducationalUse = Property "educationalUse"

propertyLearningResourceEducationalUse ::
  Property LearningResource '[DefinedTerm, Text]
propertyLearningResourceEducationalUse = Property "educationalUse"

propertyGeoShapeElevation :: Property GeoShape '[Number, Text]
propertyGeoShapeElevation = Property "elevation"

propertyGeoCoordinatesElevation ::
  Property GeoCoordinates '[Number, Text]
propertyGeoCoordinatesElevation = Property "elevation"

propertyJobPostingEligibilityToWorkRequirement ::
  Property JobPosting '[Text]
propertyJobPostingEligibilityToWorkRequirement =
  Property "eligibilityToWorkRequirement"

propertyOfferEligibleCustomerType ::
  Property Offer '[BusinessEntityType]
propertyOfferEligibleCustomerType = Property "eligibleCustomerType"

propertyDemandEligibleCustomerType ::
  Property Demand '[BusinessEntityType]
propertyDemandEligibleCustomerType =
  Property "eligibleCustomerType"

propertyDemandEligibleDuration ::
  Property Demand '[QuantitativeValue]
propertyDemandEligibleDuration = Property "eligibleDuration"

propertyOfferEligibleDuration ::
  Property Offer '[QuantitativeValue]
propertyOfferEligibleDuration = Property "eligibleDuration"

propertyDemandEligibleQuantity ::
  Property Demand '[QuantitativeValue]
propertyDemandEligibleQuantity = Property "eligibleQuantity"

propertyPriceSpecificationEligibleQuantity ::
  Property PriceSpecification '[QuantitativeValue]
propertyPriceSpecificationEligibleQuantity =
  Property "eligibleQuantity"

propertyOfferEligibleQuantity ::
  Property Offer '[QuantitativeValue]
propertyOfferEligibleQuantity = Property "eligibleQuantity"

propertyDeliveryChargeSpecificationEligibleRegion ::
  Property DeliveryChargeSpecification '[Place, Text, GeoShape]
propertyDeliveryChargeSpecificationEligibleRegion =
  Property "eligibleRegion"

propertyDemandEligibleRegion ::
  Property Demand '[Place, Text, GeoShape]
propertyDemandEligibleRegion = Property "eligibleRegion"

propertyOfferEligibleRegion ::
  Property Offer '[Place, Text, GeoShape]
propertyOfferEligibleRegion = Property "eligibleRegion"

propertyActionAccessSpecificationEligibleRegion ::
  Property ActionAccessSpecification '[Place, Text, GeoShape]
propertyActionAccessSpecificationEligibleRegion =
  Property "eligibleRegion"

propertyDemandEligibleTransactionVolume ::
  Property Demand '[PriceSpecification]
propertyDemandEligibleTransactionVolume =
  Property "eligibleTransactionVolume"

propertyPriceSpecificationEligibleTransactionVolume ::
  Property PriceSpecification '[PriceSpecification]
propertyPriceSpecificationEligibleTransactionVolume =
  Property "eligibleTransactionVolume"

propertyOfferEligibleTransactionVolume ::
  Property Offer '[PriceSpecification]
propertyOfferEligibleTransactionVolume =
  Property "eligibleTransactionVolume"

propertyOrganizationEmail :: Property Organization '[Text]
propertyOrganizationEmail = Property "email"

propertyContactPointEmail :: Property ContactPoint '[Text]
propertyContactPointEmail = Property "email"

propertyPersonEmail :: Property Person '[Text]
propertyPersonEmail = Property "email"

propertyMediaObjectEmbedUrl :: Property MediaObject '[URL]
propertyMediaObjectEmbedUrl = Property "embedUrl"

propertyVideoObjectEmbeddedTextCaption ::
  Property VideoObject '[Text]
propertyVideoObjectEmbeddedTextCaption =
  Property "embeddedTextCaption"

propertyImageObjectEmbeddedTextCaption ::
  Property ImageObject '[Text]
propertyImageObjectEmbeddedTextCaption =
  Property "embeddedTextCaption"

propertyAudioObjectEmbeddedTextCaption ::
  Property AudioObject '[Text]
propertyAudioObjectEmbeddedTextCaption =
  Property "embeddedTextCaption"

propertyVehicleEmissionsCO2 :: Property Vehicle '[Number]
propertyVehicleEmissionsCO2 = Property "emissionsCO2"

propertyOrganizationEmployee :: Property Organization '[Person]
propertyOrganizationEmployee = Property "employee"

propertyOrganizationEmployees :: Property Organization '[Person]
propertyOrganizationEmployees = Property "employees"

propertyJobPostingEmployerOverview :: Property JobPosting '[Text]
propertyJobPostingEmployerOverview = Property "employerOverview"

propertyJobPostingEmploymentType :: Property JobPosting '[Text]
propertyJobPostingEmploymentType = Property "employmentType"

propertyJobPostingEmploymentUnit ::
  Property JobPosting '[Organization]
propertyJobPostingEmploymentUnit = Property "employmentUnit"

propertyGeneEncodesBioChemEntity :: Property Gene '[BioChemEntity]
propertyGeneEncodesBioChemEntity = Property "encodesBioChemEntity"

propertyMediaObjectEncodesCreativeWork ::
  Property MediaObject '[CreativeWork]
propertyMediaObjectEncodesCreativeWork =
  Property "encodesCreativeWork"

propertyCreativeWorkEncoding ::
  Property CreativeWork '[MediaObject]
propertyCreativeWorkEncoding = Property "encoding"

propertyCreativeWorkEncodingFormat ::
  Property CreativeWork '[URL, Text]
propertyCreativeWorkEncodingFormat = Property "encodingFormat"

propertyMediaObjectEncodingFormat ::
  Property MediaObject '[URL, Text]
propertyMediaObjectEncodingFormat = Property "encodingFormat"

propertyEntryPointEncodingType :: Property EntryPoint '[Text]
propertyEntryPointEncodingType = Property "encodingType"

propertyCreativeWorkEncodings ::
  Property CreativeWork '[MediaObject]
propertyCreativeWorkEncodings = Property "encodings"

propertyCreativeWorkSeasonEndDate ::
  Property CreativeWorkSeason '[DateTime, Date]
propertyCreativeWorkSeasonEndDate = Property "endDate"

propertyMerchantReturnPolicySeasonalOverrideEndDate ::
  Property MerchantReturnPolicySeasonalOverride '[DateTime, Date]
propertyMerchantReturnPolicySeasonalOverrideEndDate =
  Property "endDate"

propertyEducationalOccupationalProgramEndDate ::
  Property EducationalOccupationalProgram '[DateTime, Date]
propertyEducationalOccupationalProgramEndDate = Property "endDate"

propertyCreativeWorkSeriesEndDate ::
  Property CreativeWorkSeries '[DateTime, Date]
propertyCreativeWorkSeriesEndDate = Property "endDate"

propertyDatedMoneySpecificationEndDate ::
  Property DatedMoneySpecification '[DateTime, Date]
propertyDatedMoneySpecificationEndDate = Property "endDate"

propertyRoleEndDate :: Property Role '[DateTime, Date]
propertyRoleEndDate = Property "endDate"

propertyScheduleEndDate :: Property Schedule '[DateTime, Date]
propertyScheduleEndDate = Property "endDate"

propertyEventEndDate :: Property Event '[DateTime, Date]
propertyEventEndDate = Property "endDate"

propertyClipEndOffset :: Property Clip '[Number, HyperTocEntry]
propertyClipEndOffset = Property "endOffset"

propertyInteractionCounterEndTime ::
  Property InteractionCounter '[DateTime, Time]
propertyInteractionCounterEndTime = Property "endTime"

propertyFoodEstablishmentReservationEndTime ::
  Property FoodEstablishmentReservation '[DateTime, Time]
propertyFoodEstablishmentReservationEndTime = Property "endTime"

propertyScheduleEndTime :: Property Schedule '[DateTime, Time]
propertyScheduleEndTime = Property "endTime"

propertyActionEndTime :: Property Action '[DateTime, Time]
propertyActionEndTime = Property "endTime"

propertyMediaObjectEndTime ::
  Property MediaObject '[DateTime, Time]
propertyMediaObjectEndTime = Property "endTime"

propertyEndorseActionEndorsee ::
  Property EndorseAction '[Organization, Person]
propertyEndorseActionEndorsee = Property "endorsee"

propertyDietEndorsers :: Property Diet '[Person, Organization]
propertyDietEndorsers = Property "endorsers"

propertyEnergyConsumptionDetailsEnergyEfficiencyScaleMax ::
  Property EnergyConsumptionDetails '[EUEnergyEfficiencyEnumeration]
propertyEnergyConsumptionDetailsEnergyEfficiencyScaleMax =
  Property "energyEfficiencyScaleMax"

propertyEnergyConsumptionDetailsEnergyEfficiencyScaleMin ::
  Property EnergyConsumptionDetails '[EUEnergyEfficiencyEnumeration]
propertyEnergyConsumptionDetailsEnergyEfficiencyScaleMin =
  Property "energyEfficiencyScaleMin"

propertyEngineSpecificationEngineDisplacement ::
  Property EngineSpecification '[QuantitativeValue]
propertyEngineSpecificationEngineDisplacement =
  Property "engineDisplacement"

propertyEngineSpecificationEnginePower ::
  Property EngineSpecification '[QuantitativeValue]
propertyEngineSpecificationEnginePower = Property "enginePower"

propertyEngineSpecificationEngineType ::
  Property EngineSpecification '[Text, URL, QualitativeValue]
propertyEngineSpecificationEngineType = Property "engineType"

propertyPerformActionEntertainmentBusiness ::
  Property PerformAction '[EntertainmentBusiness]
propertyPerformActionEntertainmentBusiness =
  Property "entertainmentBusiness"

propertyPhysicalActivityEpidemiology ::
  Property PhysicalActivity '[Text]
propertyPhysicalActivityEpidemiology = Property "epidemiology"

propertyMedicalConditionEpidemiology ::
  Property MedicalCondition '[Text]
propertyMedicalConditionEpidemiology = Property "epidemiology"

propertyTVSeriesEpisode :: Property TVSeries '[Episode]
propertyTVSeriesEpisode = Property "episode"

propertyRadioSeriesEpisode :: Property RadioSeries '[Episode]
propertyRadioSeriesEpisode = Property "episode"

propertyVideoGameSeriesEpisode ::
  Property VideoGameSeries '[Episode]
propertyVideoGameSeriesEpisode = Property "episode"

propertyCreativeWorkSeasonEpisode ::
  Property CreativeWorkSeason '[Episode]
propertyCreativeWorkSeasonEpisode = Property "episode"

propertyEpisodeEpisodeNumber :: Property Episode '[Text, Integer]
propertyEpisodeEpisodeNumber = Property "episodeNumber"

propertyCreativeWorkSeasonEpisodes ::
  Property CreativeWorkSeason '[Episode]
propertyCreativeWorkSeasonEpisodes = Property "episodes"

propertyRadioSeriesEpisodes :: Property RadioSeries '[Episode]
propertyRadioSeriesEpisodes = Property "episodes"

propertyVideoGameSeriesEpisodes ::
  Property VideoGameSeries '[Episode]
propertyVideoGameSeriesEpisodes = Property "episodes"

propertyTVSeriesEpisodes :: Property TVSeries '[Episode]
propertyTVSeriesEpisodes = Property "episodes"

propertyQualitativeValueEqual ::
  Property QualitativeValue '[QualitativeValue]
propertyQualitativeValueEqual = Property "equal"

propertyActionError :: Property Action '[Thing]
propertyActionError = Property "error"

propertyHowToSupplyEstimatedCost ::
  Property HowToSupply '[Text, MonetaryAmount]
propertyHowToSupplyEstimatedCost = Property "estimatedCost"

propertyHowToEstimatedCost ::
  Property HowTo '[Text, MonetaryAmount]
propertyHowToEstimatedCost = Property "estimatedCost"

propertyFlightEstimatedFlightDuration ::
  Property Flight '[Duration, Text]
propertyFlightEstimatedFlightDuration =
  Property "estimatedFlightDuration"

propertyOccupationEstimatedSalary ::
  Property
    Occupation
    '[ MonetaryAmount,
       Number,
       MonetaryAmountDistribution
     ]
propertyOccupationEstimatedSalary = Property "estimatedSalary"

propertyJobPostingEstimatedSalary ::
  Property
    JobPosting
    '[ MonetaryAmount,
       Number,
       MonetaryAmountDistribution
     ]
propertyJobPostingEstimatedSalary = Property "estimatedSalary"

propertyMedicalRiskEstimatorEstimatesRiskOf ::
  Property MedicalRiskEstimator '[MedicalEntity]
propertyMedicalRiskEstimatorEstimatesRiskOf =
  Property "estimatesRiskOf"

propertyOrganizationEthicsPolicy ::
  Property Organization '[CreativeWork, URL]
propertyOrganizationEthicsPolicy = Property "ethicsPolicy"

propertyNewsMediaOrganizationEthicsPolicy ::
  Property NewsMediaOrganization '[CreativeWork, URL]
propertyNewsMediaOrganizationEthicsPolicy = Property "ethicsPolicy"

propertyPlayActionEvent :: Property PlayAction '[Event]
propertyPlayActionEvent = Property "event"

propertyOrganizationEvent :: Property Organization '[Event]
propertyOrganizationEvent = Property "event"

propertyInviteActionEvent :: Property InviteAction '[Event]
propertyInviteActionEvent = Property "event"

propertyPlaceEvent :: Property Place '[Event]
propertyPlaceEvent = Property "event"

propertyLeaveActionEvent :: Property LeaveAction '[Event]
propertyLeaveActionEvent = Property "event"

propertyInformActionEvent :: Property InformAction '[Event]
propertyInformActionEvent = Property "event"

propertyJoinActionEvent :: Property JoinAction '[Event]
propertyJoinActionEvent = Property "event"

propertyEventEventAttendanceMode ::
  Property Event '[EventAttendanceModeEnumeration]
propertyEventEventAttendanceMode = Property "eventAttendanceMode"

propertyEventEventSchedule :: Property Event '[Schedule]
propertyEventEventSchedule = Property "eventSchedule"

propertyEventEventStatus :: Property Event '[EventStatusType]
propertyEventEventStatus = Property "eventStatus"

propertyPlaceEvents :: Property Place '[Event]
propertyPlaceEvents = Property "events"

propertyOrganizationEvents :: Property Organization '[Event]
propertyOrganizationEvents = Property "events"

propertyMedicalGuidelineEvidenceLevel ::
  Property MedicalGuideline '[MedicalEvidenceLevel]
propertyMedicalGuidelineEvidenceLevel = Property "evidenceLevel"

propertyMedicalGuidelineEvidenceOrigin ::
  Property MedicalGuideline '[Text]
propertyMedicalGuidelineEvidenceOrigin = Property "evidenceOrigin"

propertyCreativeWorkExampleOfWork ::
  Property CreativeWork '[CreativeWork]
propertyCreativeWorkExampleOfWork = Property "exampleOfWork"

propertyScheduleExceptDate :: Property Schedule '[DateTime, Date]
propertyScheduleExceptDate = Property "exceptDate"

propertyExchangeRateSpecificationExchangeRateSpread ::
  Property ExchangeRateSpecification '[MonetaryAmount, Number]
propertyExchangeRateSpecificationExchangeRateSpread =
  Property "exchangeRateSpread"

propertyAPIReferenceExecutableLibraryName ::
  Property APIReference '[Text]
propertyAPIReferenceExecutableLibraryName =
  Property "executableLibraryName"

propertyExerciseActionExerciseCourse ::
  Property ExerciseAction '[Place]
propertyExerciseActionExerciseCourse = Property "exerciseCourse"

propertyExerciseActionExercisePlan ::
  Property ExerciseAction '[ExercisePlan]
propertyExerciseActionExercisePlan = Property "exercisePlan"

propertyExerciseActionExerciseRelatedDiet ::
  Property ExerciseAction '[Diet]
propertyExerciseActionExerciseRelatedDiet =
  Property "exerciseRelatedDiet"

propertyExerciseActionExerciseType ::
  Property ExerciseAction '[Text]
propertyExerciseActionExerciseType = Property "exerciseType"

propertyExercisePlanExerciseType :: Property ExercisePlan '[Text]
propertyExercisePlanExerciseType = Property "exerciseType"

propertyImageObjectExifData ::
  Property ImageObject '[Text, PropertyValue]
propertyImageObjectExifData = Property "exifData"

propertyParcelDeliveryExpectedArrivalFrom ::
  Property ParcelDelivery '[Date, DateTime]
propertyParcelDeliveryExpectedArrivalFrom =
  Property "expectedArrivalFrom"

propertyParcelDeliveryExpectedArrivalUntil ::
  Property ParcelDelivery '[DateTime, Date]
propertyParcelDeliveryExpectedArrivalUntil =
  Property "expectedArrivalUntil"

propertyMedicalConditionExpectedPrognosis ::
  Property MedicalCondition '[Text]
propertyMedicalConditionExpectedPrognosis =
  Property "expectedPrognosis"

propertyMediaSubscriptionExpectsAcceptanceOf ::
  Property MediaSubscription '[Offer]
propertyMediaSubscriptionExpectsAcceptanceOf =
  Property "expectsAcceptanceOf"

propertyConsumeActionExpectsAcceptanceOf ::
  Property ConsumeAction '[Offer]
propertyConsumeActionExpectsAcceptanceOf =
  Property "expectsAcceptanceOf"

propertyActionAccessSpecificationExpectsAcceptanceOf ::
  Property ActionAccessSpecification '[Offer]
propertyActionAccessSpecificationExpectsAcceptanceOf =
  Property "expectsAcceptanceOf"

propertyJobPostingExperienceInPlaceOfEducation ::
  Property JobPosting '[Boolean]
propertyJobPostingExperienceInPlaceOfEducation =
  Property "experienceInPlaceOfEducation"

propertyOccupationExperienceRequirements ::
  Property Occupation '[OccupationalExperienceRequirements, Text]
propertyOccupationExperienceRequirements =
  Property "experienceRequirements"

propertyJobPostingExperienceRequirements ::
  Property JobPosting '[OccupationalExperienceRequirements, Text]
propertyJobPostingExperienceRequirements =
  Property "experienceRequirements"

propertyDietExpertConsiderations :: Property Diet '[Text]
propertyDietExpertConsiderations = Property "expertConsiderations"

propertyCreativeWorkExpires ::
  Property CreativeWork '[Date, DateTime]
propertyCreativeWorkExpires = Property "expires"

propertyGeneExpressedIn ::
  Property
    Gene
    '[ DefinedTerm,
       BioChemEntity,
       AnatomicalSystem,
       AnatomicalStructure
     ]
propertyGeneExpressedIn = Property "expressedIn"

propertyPersonFamilyName :: Property Person '[Text]
propertyPersonFamilyName = Property "familyName"

propertyNutritionInformationFatContent ::
  Property NutritionInformation '[Mass]
propertyNutritionInformationFatContent = Property "fatContent"

propertyOrganizationFaxNumber :: Property Organization '[Text]
propertyOrganizationFaxNumber = Property "faxNumber"

propertyContactPointFaxNumber :: Property ContactPoint '[Text]
propertyContactPointFaxNumber = Property "faxNumber"

propertyPersonFaxNumber :: Property Person '[Text]
propertyPersonFaxNumber = Property "faxNumber"

propertyPlaceFaxNumber :: Property Place '[Text]
propertyPlaceFaxNumber = Property "faxNumber"

propertySoftwareApplicationFeatureList ::
  Property SoftwareApplication '[URL, Text]
propertySoftwareApplicationFeatureList = Property "featureList"

propertyFinancialServiceFeesAndCommissionsSpecification ::
  Property FinancialService '[URL, Text]
propertyFinancialServiceFeesAndCommissionsSpecification =
  Property "feesAndCommissionsSpecification"

propertyFinancialProductFeesAndCommissionsSpecification ::
  Property FinancialProduct '[URL, Text]
propertyFinancialProductFeesAndCommissionsSpecification =
  Property "feesAndCommissionsSpecification"

propertyNutritionInformationFiberContent ::
  Property NutritionInformation '[Mass]
propertyNutritionInformationFiberContent = Property "fiberContent"

propertyCreativeWorkFileFormat ::
  Property CreativeWork '[Text, URL]
propertyCreativeWorkFileFormat = Property "fileFormat"

propertySoftwareApplicationFileSize ::
  Property SoftwareApplication '[Text]
propertySoftwareApplicationFileSize = Property "fileSize"

propertyEducationalOccupationalProgramFinancialAidEligible ::
  Property EducationalOccupationalProgram '[DefinedTerm, Text]
propertyEducationalOccupationalProgramFinancialAidEligible =
  Property "financialAidEligible"

propertyClaimFirstAppearance :: Property Claim '[CreativeWork]
propertyClaimFirstAppearance = Property "firstAppearance"

propertyMusicCompositionFirstPerformance ::
  Property MusicComposition '[Event]
propertyMusicCompositionFirstPerformance =
  Property "firstPerformance"

propertyFlightFlightDistance :: Property Flight '[Text, Distance]
propertyFlightFlightDistance = Property "flightDistance"

propertyFlightFlightNumber :: Property Flight '[Text]
propertyFlightFlightNumber = Property "flightNumber"

propertyAccommodationFloorLevel :: Property Accommodation '[Text]
propertyAccommodationFloorLevel = Property "floorLevel"

propertyPaymentCardFloorLimit ::
  Property PaymentCard '[MonetaryAmount]
propertyPaymentCardFloorLimit = Property "floorLimit"

propertyAccommodationFloorSize ::
  Property Accommodation '[QuantitativeValue]
propertyAccommodationFloorSize = Property "floorSize"

propertyFloorPlanFloorSize ::
  Property FloorPlan '[QuantitativeValue]
propertyFloorPlanFloorSize = Property "floorSize"

propertyFollowActionFollowee ::
  Property FollowAction '[Organization, Person]
propertyFollowActionFollowee = Property "followee"

propertyPersonFollows :: Property Person '[Person]
propertyPersonFollows = Property "follows"

propertyMedicalProcedureFollowup ::
  Property MedicalProcedure '[Text]
propertyMedicalProcedureFollowup = Property "followup"

propertyCookActionFoodEstablishment ::
  Property CookAction '[FoodEstablishment, Place]
propertyCookActionFoodEstablishment = Property "foodEstablishment"

propertyCookActionFoodEvent :: Property CookAction '[FoodEvent]
propertyCookActionFoodEvent = Property "foodEvent"

propertyDrugFoodWarning :: Property Drug '[Text]
propertyDrugFoodWarning = Property "foodWarning"

propertyOrganizationFounder :: Property Organization '[Person]
propertyOrganizationFounder = Property "founder"

propertyOrganizationFounders :: Property Organization '[Person]
propertyOrganizationFounders = Property "founders"

propertyOrganizationFoundingDate :: Property Organization '[Date]
propertyOrganizationFoundingDate = Property "foundingDate"

propertyOrganizationFoundingLocation ::
  Property Organization '[Place]
propertyOrganizationFoundingLocation = Property "foundingLocation"

propertyPublicationEventFree ::
  Property PublicationEvent '[Boolean]
propertyPublicationEventFree = Property "free"

propertyShippingRateSettingsFreeShippingThreshold ::
  Property
    ShippingRateSettings
    '[ DeliveryChargeSpecification,
       MonetaryAmount
     ]
propertyShippingRateSettingsFreeShippingThreshold =
  Property "freeShippingThreshold"

propertyDoseScheduleFrequency :: Property DoseSchedule '[Text]
propertyDoseScheduleFrequency = Property "frequency"

propertyMoveActionFromLocation :: Property MoveAction '[Place]
propertyMoveActionFromLocation = Property "fromLocation"

propertyExerciseActionFromLocation ::
  Property ExerciseAction '[Place]
propertyExerciseActionFromLocation = Property "fromLocation"

propertyTransferActionFromLocation ::
  Property TransferAction '[Place]
propertyTransferActionFromLocation = Property "fromLocation"

propertyVehicleFuelCapacity ::
  Property Vehicle '[QuantitativeValue]
propertyVehicleFuelCapacity = Property "fuelCapacity"

propertyVehicleFuelConsumption ::
  Property Vehicle '[QuantitativeValue]
propertyVehicleFuelConsumption = Property "fuelConsumption"

propertyVehicleFuelEfficiency ::
  Property Vehicle '[QuantitativeValue]
propertyVehicleFuelEfficiency = Property "fuelEfficiency"

propertyVehicleFuelType ::
  Property Vehicle '[URL, QualitativeValue, Text]
propertyVehicleFuelType = Property "fuelType"

propertyEngineSpecificationFuelType ::
  Property EngineSpecification '[URL, QualitativeValue, Text]
propertyEngineSpecificationFuelType = Property "fuelType"

propertyJointFunctionalClass ::
  Property Joint '[MedicalEntity, Text]
propertyJointFunctionalClass = Property "functionalClass"

propertyGrantFundedItem ::
  Property
    Grant
    '[ Organization,
       BioChemEntity,
       Event,
       Person,
       MedicalEntity,
       Product,
       CreativeWork
     ]
propertyGrantFundedItem = Property "fundedItem"

propertyPersonFunder :: Property Person '[Organization, Person]
propertyPersonFunder = Property "funder"

propertyEventFunder :: Property Event '[Organization, Person]
propertyEventFunder = Property "funder"

propertyGrantFunder :: Property Grant '[Organization, Person]
propertyGrantFunder = Property "funder"

propertyMonetaryGrantFunder ::
  Property MonetaryGrant '[Organization, Person]
propertyMonetaryGrantFunder = Property "funder"

propertyCreativeWorkFunder ::
  Property CreativeWork '[Organization, Person]
propertyCreativeWorkFunder = Property "funder"

propertyOrganizationFunder ::
  Property Organization '[Organization, Person]
propertyOrganizationFunder = Property "funder"

propertyCreativeWorkFunding :: Property CreativeWork '[Grant]
propertyCreativeWorkFunding = Property "funding"

propertyEventFunding :: Property Event '[Grant]
propertyEventFunding = Property "funding"

propertyBioChemEntityFunding :: Property BioChemEntity '[Grant]
propertyBioChemEntityFunding = Property "funding"

propertyOrganizationFunding :: Property Organization '[Grant]
propertyOrganizationFunding = Property "funding"

propertyPersonFunding :: Property Person '[Grant]
propertyPersonFunding = Property "funding"

propertyMedicalEntityFunding :: Property MedicalEntity '[Grant]
propertyMedicalEntityFunding = Property "funding"

propertyProductFunding :: Property Product '[Grant]
propertyProductFunding = Property "funding"

propertyGameServerGame :: Property GameServer '[VideoGame]
propertyGameServerGame = Property "game"

propertyPlayGameActionGameAvailabilityType ::
  Property PlayGameAction '[Text, GameAvailabilityEnumeration]
propertyPlayGameActionGameAvailabilityType =
  Property "gameAvailabilityType"

propertyVideoGameGameEdition :: Property VideoGame '[Text]
propertyVideoGameGameEdition = Property "gameEdition"

propertyGameGameItem :: Property Game '[Thing]
propertyGameGameItem = Property "gameItem"

propertyVideoGameSeriesGameItem ::
  Property VideoGameSeries '[Thing]
propertyVideoGameSeriesGameItem = Property "gameItem"

propertyGameGameLocation ::
  Property Game '[PostalAddress, URL, Place]
propertyGameGameLocation = Property "gameLocation"

propertyVideoGameSeriesGameLocation ::
  Property VideoGameSeries '[PostalAddress, URL, Place]
propertyVideoGameSeriesGameLocation = Property "gameLocation"

propertyVideoGameGamePlatform ::
  Property VideoGame '[Thing, URL, Text]
propertyVideoGameGamePlatform = Property "gamePlatform"

propertyVideoGameSeriesGamePlatform ::
  Property VideoGameSeries '[Thing, URL, Text]
propertyVideoGameSeriesGamePlatform = Property "gamePlatform"

propertyVideoGameGameServer :: Property VideoGame '[GameServer]
propertyVideoGameGameServer = Property "gameServer"

propertyVideoGameGameTip :: Property VideoGame '[CreativeWork]
propertyVideoGameGameTip = Property "gameTip"

propertySportsTeamGender :: Property SportsTeam '[GenderType, Text]
propertySportsTeamGender = Property "gender"

propertyPersonGender :: Property Person '[GenderType, Text]
propertyPersonGender = Property "gender"

propertyBroadcastChannelGenre ::
  Property BroadcastChannel '[Text, URL]
propertyBroadcastChannelGenre = Property "genre"

propertyMusicGroupGenre :: Property MusicGroup '[Text, URL]
propertyMusicGroupGenre = Property "genre"

propertyCreativeWorkGenre :: Property CreativeWork '[Text, URL]
propertyCreativeWorkGenre = Property "genre"

propertyPlaceGeo :: Property Place '[GeoCoordinates, GeoShape]
propertyPlaceGeo = Property "geo"

propertyGeospatialGeometryGeoContains ::
  Property GeospatialGeometry '[Place, GeospatialGeometry]
propertyGeospatialGeometryGeoContains = Property "geoContains"

propertyPlaceGeoContains ::
  Property Place '[Place, GeospatialGeometry]
propertyPlaceGeoContains = Property "geoContains"

propertyGeospatialGeometryGeoCoveredBy ::
  Property GeospatialGeometry '[GeospatialGeometry, Place]
propertyGeospatialGeometryGeoCoveredBy = Property "geoCoveredBy"

propertyPlaceGeoCoveredBy ::
  Property Place '[GeospatialGeometry, Place]
propertyPlaceGeoCoveredBy = Property "geoCoveredBy"

propertyGeospatialGeometryGeoCovers ::
  Property GeospatialGeometry '[GeospatialGeometry, Place]
propertyGeospatialGeometryGeoCovers = Property "geoCovers"

propertyPlaceGeoCovers ::
  Property Place '[GeospatialGeometry, Place]
propertyPlaceGeoCovers = Property "geoCovers"

propertyGeospatialGeometryGeoCrosses ::
  Property GeospatialGeometry '[GeospatialGeometry, Place]
propertyGeospatialGeometryGeoCrosses = Property "geoCrosses"

propertyPlaceGeoCrosses ::
  Property Place '[GeospatialGeometry, Place]
propertyPlaceGeoCrosses = Property "geoCrosses"

propertyGeospatialGeometryGeoDisjoint ::
  Property GeospatialGeometry '[GeospatialGeometry, Place]
propertyGeospatialGeometryGeoDisjoint = Property "geoDisjoint"

propertyPlaceGeoDisjoint ::
  Property Place '[GeospatialGeometry, Place]
propertyPlaceGeoDisjoint = Property "geoDisjoint"

propertyGeospatialGeometryGeoEquals ::
  Property GeospatialGeometry '[GeospatialGeometry, Place]
propertyGeospatialGeometryGeoEquals = Property "geoEquals"

propertyPlaceGeoEquals ::
  Property Place '[GeospatialGeometry, Place]
propertyPlaceGeoEquals = Property "geoEquals"

propertyGeospatialGeometryGeoIntersects ::
  Property GeospatialGeometry '[GeospatialGeometry, Place]
propertyGeospatialGeometryGeoIntersects = Property "geoIntersects"

propertyPlaceGeoIntersects ::
  Property Place '[GeospatialGeometry, Place]
propertyPlaceGeoIntersects = Property "geoIntersects"

propertyGeoCircleGeoMidpoint ::
  Property GeoCircle '[GeoCoordinates]
propertyGeoCircleGeoMidpoint = Property "geoMidpoint"

propertyGeospatialGeometryGeoOverlaps ::
  Property GeospatialGeometry '[GeospatialGeometry, Place]
propertyGeospatialGeometryGeoOverlaps = Property "geoOverlaps"

propertyPlaceGeoOverlaps ::
  Property Place '[GeospatialGeometry, Place]
propertyPlaceGeoOverlaps = Property "geoOverlaps"

propertyGeoCircleGeoRadius ::
  Property GeoCircle '[Number, Text, Distance]
propertyGeoCircleGeoRadius = Property "geoRadius"

propertyGeospatialGeometryGeoTouches ::
  Property GeospatialGeometry '[GeospatialGeometry, Place]
propertyGeospatialGeometryGeoTouches = Property "geoTouches"

propertyPlaceGeoTouches ::
  Property Place '[GeospatialGeometry, Place]
propertyPlaceGeoTouches = Property "geoTouches"

propertyGeospatialGeometryGeoWithin ::
  Property GeospatialGeometry '[GeospatialGeometry, Place]
propertyGeospatialGeometryGeoWithin = Property "geoWithin"

propertyPlaceGeoWithin ::
  Property Place '[GeospatialGeometry, Place]
propertyPlaceGeoWithin = Property "geoWithin"

propertyAudienceGeographicArea ::
  Property Audience '[AdministrativeArea]
propertyAudienceGeographicArea = Property "geographicArea"

propertySpecialAnnouncementGettingTestedInfo ::
  Property SpecialAnnouncement '[WebContent, URL]
propertySpecialAnnouncementGettingTestedInfo =
  Property "gettingTestedInfo"

propertyPersonGivenName :: Property Person '[Text]
propertyPersonGivenName = Property "givenName"

propertyPlaceGlobalLocationNumber :: Property Place '[Text]
propertyPlaceGlobalLocationNumber = Property "globalLocationNumber"

propertyPersonGlobalLocationNumber :: Property Person '[Text]
propertyPersonGlobalLocationNumber =
  Property "globalLocationNumber"

propertyOrganizationGlobalLocationNumber ::
  Property Organization '[Text]
propertyOrganizationGlobalLocationNumber =
  Property "globalLocationNumber"

propertySpecialAnnouncementGovernmentBenefitsInfo ::
  Property SpecialAnnouncement '[GovernmentService]
propertySpecialAnnouncementGovernmentBenefitsInfo =
  Property "governmentBenefitsInfo"

propertyLoanOrCreditGracePeriod ::
  Property LoanOrCredit '[Duration]
propertyLoanOrCreditGracePeriod = Property "gracePeriod"

propertyDigitalDocumentPermissionGrantee ::
  Property
    DigitalDocumentPermission
    '[ Audience,
       Organization,
       ContactPoint,
       Person
     ]
propertyDigitalDocumentPermissionGrantee = Property "grantee"

propertyQualitativeValueGreater ::
  Property QualitativeValue '[QualitativeValue]
propertyQualitativeValueGreater = Property "greater"

propertyQualitativeValueGreaterOrEqual ::
  Property QualitativeValue '[QualitativeValue]
propertyQualitativeValueGreaterOrEqual = Property "greaterOrEqual"

propertyDemandGtin :: Property Demand '[Text, URL]
propertyDemandGtin = Property "gtin"

propertyProductGtin :: Property Product '[Text, URL]
propertyProductGtin = Property "gtin"

propertyOfferGtin :: Property Offer '[Text, URL]
propertyOfferGtin = Property "gtin"

propertyProductGtin12 :: Property Product '[Text]
propertyProductGtin12 = Property "gtin12"

propertyDemandGtin12 :: Property Demand '[Text]
propertyDemandGtin12 = Property "gtin12"

propertyOfferGtin12 :: Property Offer '[Text]
propertyOfferGtin12 = Property "gtin12"

propertyDemandGtin13 :: Property Demand '[Text]
propertyDemandGtin13 = Property "gtin13"

propertyProductGtin13 :: Property Product '[Text]
propertyProductGtin13 = Property "gtin13"

propertyOfferGtin13 :: Property Offer '[Text]
propertyOfferGtin13 = Property "gtin13"

propertyDemandGtin14 :: Property Demand '[Text]
propertyDemandGtin14 = Property "gtin14"

propertyProductGtin14 :: Property Product '[Text]
propertyProductGtin14 = Property "gtin14"

propertyOfferGtin14 :: Property Offer '[Text]
propertyOfferGtin14 = Property "gtin14"

propertyDemandGtin8 :: Property Demand '[Text]
propertyDemandGtin8 = Property "gtin8"

propertyProductGtin8 :: Property Product '[Text]
propertyProductGtin8 = Property "gtin8"

propertyOfferGtin8 :: Property Offer '[Text]
propertyOfferGtin8 = Property "gtin8"

propertyMedicalEntityGuideline ::
  Property MedicalEntity '[MedicalGuideline]
propertyMedicalEntityGuideline = Property "guideline"

propertyMedicalGuidelineGuidelineDate ::
  Property MedicalGuideline '[Date]
propertyMedicalGuidelineGuidelineDate = Property "guidelineDate"

propertyMedicalGuidelineGuidelineSubject ::
  Property MedicalGuideline '[MedicalEntity]
propertyMedicalGuidelineGuidelineSubject =
  Property "guidelineSubject"

propertyShippingDeliveryTimeHandlingTime ::
  Property ShippingDeliveryTime '[QuantitativeValue]
propertyShippingDeliveryTimeHandlingTime = Property "handlingTime"

propertyOfferHasAdultConsideration ::
  Property Offer '[AdultOrientedEnumeration]
propertyOfferHasAdultConsideration =
  Property "hasAdultConsideration"

propertyProductHasAdultConsideration ::
  Property Product '[AdultOrientedEnumeration]
propertyProductHasAdultConsideration =
  Property "hasAdultConsideration"

propertyBioChemEntityHasBioChemEntityPart ::
  Property BioChemEntity '[BioChemEntity]
propertyBioChemEntityHasBioChemEntityPart =
  Property "hasBioChemEntityPart"

propertyProteinHasBioPolymerSequence :: Property Protein '[Text]
propertyProteinHasBioPolymerSequence =
  Property "hasBioPolymerSequence"

propertyGeneHasBioPolymerSequence :: Property Gene '[Text]
propertyGeneHasBioPolymerSequence =
  Property "hasBioPolymerSequence"

propertyBroadcastServiceHasBroadcastChannel ::
  Property BroadcastService '[BroadcastChannel]
propertyBroadcastServiceHasBroadcastChannel =
  Property "hasBroadcastChannel"

propertyCategoryCodeSetHasCategoryCode ::
  Property CategoryCodeSet '[CategoryCode]
propertyCategoryCodeSetHasCategoryCode = Property "hasCategoryCode"

propertyEducationalOccupationalProgramHasCourse ::
  Property EducationalOccupationalProgram '[Course]
propertyEducationalOccupationalProgramHasCourse =
  Property "hasCourse"

propertyCourseHasCourseInstance ::
  Property Course '[CourseInstance]
propertyCourseHasCourseInstance = Property "hasCourseInstance"

propertyOrganizationHasCredential ::
  Property Organization '[EducationalOccupationalCredential]
propertyOrganizationHasCredential = Property "hasCredential"

propertyPersonHasCredential ::
  Property Person '[EducationalOccupationalCredential]
propertyPersonHasCredential = Property "hasCredential"

propertyTaxonHasDefinedTerm :: Property Taxon '[DefinedTerm]
propertyTaxonHasDefinedTerm = Property "hasDefinedTerm"

propertyDefinedTermSetHasDefinedTerm ::
  Property DefinedTermSet '[DefinedTerm]
propertyDefinedTermSetHasDefinedTerm = Property "hasDefinedTerm"

propertyDeliveryEventHasDeliveryMethod ::
  Property DeliveryEvent '[DeliveryMethod]
propertyDeliveryEventHasDeliveryMethod =
  Property "hasDeliveryMethod"

propertyParcelDeliveryHasDeliveryMethod ::
  Property ParcelDelivery '[DeliveryMethod]
propertyParcelDeliveryHasDeliveryMethod =
  Property "hasDeliveryMethod"

propertyDigitalDocumentHasDigitalDocumentPermission ::
  Property DigitalDocument '[DigitalDocumentPermission]
propertyDigitalDocumentHasDigitalDocumentPermission =
  Property "hasDigitalDocumentPermission"

propertyPlaceHasDriveThroughService :: Property Place '[Boolean]
propertyPlaceHasDriveThroughService =
  Property "hasDriveThroughService"

propertyProductHasEnergyConsumptionDetails ::
  Property Product '[EnergyConsumptionDetails]
propertyProductHasEnergyConsumptionDetails =
  Property "hasEnergyConsumptionDetails"

propertyEnergyConsumptionDetailsHasEnergyEfficiencyCategory ::
  Property EnergyConsumptionDetails '[EnergyEfficiencyEnumeration]
propertyEnergyConsumptionDetailsHasEnergyEfficiencyCategory =
  Property "hasEnergyEfficiencyCategory"

propertyHealthTopicContentHasHealthAspect ::
  Property HealthTopicContent '[HealthAspectEnumeration]
propertyHealthTopicContentHasHealthAspect =
  Property "hasHealthAspect"

propertyPlaceHasMap :: Property Place '[URL, Map]
propertyPlaceHasMap = Property "hasMap"

propertySizeSpecificationHasMeasurement ::
  Property SizeSpecification '[QuantitativeValue]
propertySizeSpecificationHasMeasurement = Property "hasMeasurement"

propertyProductHasMeasurement ::
  Property Product '[QuantitativeValue]
propertyProductHasMeasurement = Property "hasMeasurement"

propertyOfferHasMeasurement :: Property Offer '[QuantitativeValue]
propertyOfferHasMeasurement = Property "hasMeasurement"

propertyFoodEstablishmentHasMenu ::
  Property FoodEstablishment '[URL, Text, Menu]
propertyFoodEstablishmentHasMenu = Property "hasMenu"

propertyMenuSectionHasMenuItem :: Property MenuSection '[MenuItem]
propertyMenuSectionHasMenuItem = Property "hasMenuItem"

propertyMenuHasMenuItem :: Property Menu '[MenuItem]
propertyMenuHasMenuItem = Property "hasMenuItem"

propertyMenuSectionHasMenuSection ::
  Property MenuSection '[MenuSection]
propertyMenuSectionHasMenuSection = Property "hasMenuSection"

propertyMenuHasMenuSection :: Property Menu '[MenuSection]
propertyMenuHasMenuSection = Property "hasMenuSection"

propertyOfferHasMerchantReturnPolicy ::
  Property Offer '[MerchantReturnPolicy]
propertyOfferHasMerchantReturnPolicy =
  Property "hasMerchantReturnPolicy"

propertyProductHasMerchantReturnPolicy ::
  Property Product '[MerchantReturnPolicy]
propertyProductHasMerchantReturnPolicy =
  Property "hasMerchantReturnPolicy"

propertyOrganizationHasMerchantReturnPolicy ::
  Property Organization '[MerchantReturnPolicy]
propertyOrganizationHasMerchantReturnPolicy =
  Property "hasMerchantReturnPolicy"

propertyBioChemEntityHasMolecularFunction ::
  Property BioChemEntity '[URL, DefinedTerm, PropertyValue]
propertyBioChemEntityHasMolecularFunction =
  Property "hasMolecularFunction"

propertyPersonHasOccupation :: Property Person '[Occupation]
propertyPersonHasOccupation = Property "hasOccupation"

propertyServiceHasOfferCatalog :: Property Service '[OfferCatalog]
propertyServiceHasOfferCatalog = Property "hasOfferCatalog"

propertyOrganizationHasOfferCatalog ::
  Property Organization '[OfferCatalog]
propertyOrganizationHasOfferCatalog = Property "hasOfferCatalog"

propertyPersonHasOfferCatalog :: Property Person '[OfferCatalog]
propertyPersonHasOfferCatalog = Property "hasOfferCatalog"

propertyPersonHasPOS :: Property Person '[Place]
propertyPersonHasPOS = Property "hasPOS"

propertyOrganizationHasPOS :: Property Organization '[Place]
propertyOrganizationHasPOS = Property "hasPOS"

propertyCreativeWorkHasPart ::
  Property CreativeWork '[CreativeWork]
propertyCreativeWorkHasPart = Property "hasPart"

propertyBioChemEntityHasRepresentation ::
  Property BioChemEntity '[Text, URL, PropertyValue]
propertyBioChemEntityHasRepresentation =
  Property "hasRepresentation"

propertyProductGroupHasVariant :: Property ProductGroup '[Product]
propertyProductGroupHasVariant = Property "hasVariant"

propertyCreativeWorkHeadline :: Property CreativeWork '[Text]
propertyCreativeWorkHeadline = Property "headline"

propertyMedicalStudyHealthCondition ::
  Property MedicalStudy '[MedicalCondition]
propertyMedicalStudyHealthCondition = Property "healthCondition"

propertyPatientHealthCondition ::
  Property Patient '[MedicalCondition]
propertyPatientHealthCondition = Property "healthCondition"

propertyPeopleAudienceHealthCondition ::
  Property PeopleAudience '[MedicalCondition]
propertyPeopleAudienceHealthCondition = Property "healthCondition"

propertyHealthPlanCostSharingSpecificationHealthPlanCoinsuranceOption ::
  Property HealthPlanCostSharingSpecification '[Text]
propertyHealthPlanCostSharingSpecificationHealthPlanCoinsuranceOption =
  Property "healthPlanCoinsuranceOption"

propertyHealthPlanCostSharingSpecificationHealthPlanCoinsuranceRate ::
  Property HealthPlanCostSharingSpecification '[Number]
propertyHealthPlanCostSharingSpecificationHealthPlanCoinsuranceRate =
  Property "healthPlanCoinsuranceRate"

propertyHealthPlanCostSharingSpecificationHealthPlanCopay ::
  Property HealthPlanCostSharingSpecification '[PriceSpecification]
propertyHealthPlanCostSharingSpecificationHealthPlanCopay =
  Property "healthPlanCopay"

propertyHealthPlanCostSharingSpecificationHealthPlanCopayOption ::
  Property HealthPlanCostSharingSpecification '[Text]
propertyHealthPlanCostSharingSpecificationHealthPlanCopayOption =
  Property "healthPlanCopayOption"

propertyHealthPlanFormularyHealthPlanCostSharing ::
  Property HealthPlanFormulary '[Boolean]
propertyHealthPlanFormularyHealthPlanCostSharing =
  Property "healthPlanCostSharing"

propertyHealthPlanNetworkHealthPlanCostSharing ::
  Property HealthPlanNetwork '[Boolean]
propertyHealthPlanNetworkHealthPlanCostSharing =
  Property "healthPlanCostSharing"

propertyHealthInsurancePlanHealthPlanDrugOption ::
  Property HealthInsurancePlan '[Text]
propertyHealthInsurancePlanHealthPlanDrugOption =
  Property "healthPlanDrugOption"

propertyHealthInsurancePlanHealthPlanDrugTier ::
  Property HealthInsurancePlan '[Text]
propertyHealthInsurancePlanHealthPlanDrugTier =
  Property "healthPlanDrugTier"

propertyHealthPlanFormularyHealthPlanDrugTier ::
  Property HealthPlanFormulary '[Text]
propertyHealthPlanFormularyHealthPlanDrugTier =
  Property "healthPlanDrugTier"

propertyHealthInsurancePlanHealthPlanId ::
  Property HealthInsurancePlan '[Text]
propertyHealthInsurancePlanHealthPlanId = Property "healthPlanId"

propertyHealthInsurancePlanHealthPlanMarketingUrl ::
  Property HealthInsurancePlan '[URL]
propertyHealthInsurancePlanHealthPlanMarketingUrl =
  Property "healthPlanMarketingUrl"

propertyHealthPlanNetworkHealthPlanNetworkId ::
  Property HealthPlanNetwork '[Text]
propertyHealthPlanNetworkHealthPlanNetworkId =
  Property "healthPlanNetworkId"

propertyMedicalOrganizationHealthPlanNetworkId ::
  Property MedicalOrganization '[Text]
propertyMedicalOrganizationHealthPlanNetworkId =
  Property "healthPlanNetworkId"

propertyHealthPlanNetworkHealthPlanNetworkTier ::
  Property HealthPlanNetwork '[Text]
propertyHealthPlanNetworkHealthPlanNetworkTier =
  Property "healthPlanNetworkTier"

propertyHealthPlanCostSharingSpecificationHealthPlanPharmacyCategory ::
  Property HealthPlanCostSharingSpecification '[Text]
propertyHealthPlanCostSharingSpecificationHealthPlanPharmacyCategory =
  Property "healthPlanPharmacyCategory"

propertyHospitalHealthcareReportingData ::
  Property Hospital '[CDCPMDRecord, Dataset]
propertyHospitalHealthcareReportingData =
  Property "healthcareReportingData"

propertyOfferShippingDetailsHeight ::
  Property OfferShippingDetails '[QuantitativeValue, Distance]
propertyOfferShippingDetailsHeight = Property "height"

propertyVisualArtworkHeight ::
  Property VisualArtwork '[QuantitativeValue, Distance]
propertyVisualArtworkHeight = Property "height"

propertyPersonHeight ::
  Property Person '[QuantitativeValue, Distance]
propertyPersonHeight = Property "height"

propertyProductHeight ::
  Property Product '[QuantitativeValue, Distance]
propertyProductHeight = Property "height"

propertyMediaObjectHeight ::
  Property MediaObject '[QuantitativeValue, Distance]
propertyMediaObjectHeight = Property "height"

propertyAggregateOfferHighPrice ::
  Property AggregateOffer '[Number, Text]
propertyAggregateOfferHighPrice = Property "highPrice"

propertyJobPostingHiringOrganization ::
  Property JobPosting '[Organization, Person]
propertyJobPostingHiringOrganization =
  Property "hiringOrganization"

propertyArchiveComponentHoldingArchive ::
  Property ArchiveComponent '[ArchiveOrganization]
propertyArchiveComponentHoldingArchive = Property "holdingArchive"

propertyPersonHomeLocation ::
  Property Person '[ContactPoint, Place]
propertyPersonHomeLocation = Property "homeLocation"

propertySportsEventHomeTeam ::
  Property SportsEvent '[SportsTeam, Person]
propertySportsEventHomeTeam = Property "homeTeam"

propertyPersonHonorificPrefix :: Property Person '[Text]
propertyPersonHonorificPrefix = Property "honorificPrefix"

propertyPersonHonorificSuffix :: Property Person '[Text]
propertyPersonHonorificSuffix = Property "honorificSuffix"

propertyPhysicianHospitalAffiliation ::
  Property Physician '[Hospital]
propertyPhysicianHospitalAffiliation =
  Property "hospitalAffiliation"

propertyProgramMembershipHostingOrganization ::
  Property ProgramMembership '[Organization]
propertyProgramMembershipHostingOrganization =
  Property "hostingOrganization"

propertyLocationFeatureSpecificationHoursAvailable ::
  Property LocationFeatureSpecification '[OpeningHoursSpecification]
propertyLocationFeatureSpecificationHoursAvailable =
  Property "hoursAvailable"

propertyServiceHoursAvailable ::
  Property Service '[OpeningHoursSpecification]
propertyServiceHoursAvailable = Property "hoursAvailable"

propertyContactPointHoursAvailable ::
  Property ContactPoint '[OpeningHoursSpecification]
propertyContactPointHoursAvailable = Property "hoursAvailable"

propertyMedicalProcedureHowPerformed ::
  Property MedicalProcedure '[Text]
propertyMedicalProcedureHowPerformed = Property "howPerformed"

propertyEntryPointHttpMethod :: Property EntryPoint '[Text]
propertyEntryPointHttpMethod = Property "httpMethod"

propertyAirlineIataCode :: Property Airline '[Text]
propertyAirlineIataCode = Property "iataCode"

propertyAirportIataCode :: Property Airport '[Text]
propertyAirportIataCode = Property "iataCode"

propertyAirportIcaoCode :: Property Airport '[Text]
propertyAirportIcaoCode = Property "icaoCode"

propertyThingIdentifier ::
  Property Thing '[PropertyValue, URL, Text]
propertyThingIdentifier = Property "identifier"

propertyMedicalSignIdentifyingExam ::
  Property MedicalSign '[PhysicalExam]
propertyMedicalSignIdentifyingExam = Property "identifyingExam"

propertyMedicalSignIdentifyingTest ::
  Property MedicalSign '[MedicalTest]
propertyMedicalSignIdentifyingTest = Property "identifyingTest"

propertyBookIllustrator :: Property Book '[Person]
propertyBookIllustrator = Property "illustrator"

propertyThingImage :: Property Thing '[URL, ImageObject]
propertyThingImage = Property "image"

propertyImagingTestImagingTechnique ::
  Property ImagingTest '[MedicalImagingTechnique]
propertyImagingTestImagingTechnique = Property "imagingTechnique"

propertyMusicRecordingInAlbum ::
  Property MusicRecording '[MusicAlbum]
propertyMusicRecordingInAlbum = Property "inAlbum"

propertyBroadcastChannelInBroadcastLineup ::
  Property BroadcastChannel '[CableOrSatelliteService]
propertyBroadcastChannelInBroadcastLineup =
  Property "inBroadcastLineup"

propertyMolecularEntityInChI :: Property MolecularEntity '[Text]
propertyMolecularEntityInChI = Property "inChI"

propertyMolecularEntityInChIKey :: Property MolecularEntity '[Text]
propertyMolecularEntityInChIKey = Property "inChIKey"

propertyCategoryCodeInCodeSet ::
  Property CategoryCode '[URL, CategoryCodeSet]
propertyCategoryCodeInCodeSet = Property "inCodeSet"

propertyDefinedTermInDefinedTermSet ::
  Property DefinedTerm '[DefinedTermSet, URL]
propertyDefinedTermInDefinedTermSet = Property "inDefinedTermSet"

propertyPronounceableTextInLanguage ::
  Property PronounceableText '[Text, Language]
propertyPronounceableTextInLanguage = Property "inLanguage"

propertyLinkRoleInLanguage :: Property LinkRole '[Text, Language]
propertyLinkRoleInLanguage = Property "inLanguage"

propertyCreativeWorkInLanguage ::
  Property CreativeWork '[Text, Language]
propertyCreativeWorkInLanguage = Property "inLanguage"

propertyWriteActionInLanguage ::
  Property WriteAction '[Text, Language]
propertyWriteActionInLanguage = Property "inLanguage"

propertyEventInLanguage :: Property Event '[Text, Language]
propertyEventInLanguage = Property "inLanguage"

propertyBroadcastServiceInLanguage ::
  Property BroadcastService '[Text, Language]
propertyBroadcastServiceInLanguage = Property "inLanguage"

propertyCommunicateActionInLanguage ::
  Property CommunicateAction '[Text, Language]
propertyCommunicateActionInLanguage = Property "inLanguage"

propertyMusicRecordingInPlaylist ::
  Property MusicRecording '[MusicPlaylist]
propertyMusicRecordingInPlaylist = Property "inPlaylist"

propertyProductInProductGroupWithID :: Property Product '[Text]
propertyProductInProductGroupWithID =
  Property "inProductGroupWithID"

propertyMerchantReturnPolicyInStoreReturnsOffered ::
  Property MerchantReturnPolicy '[Boolean]
propertyMerchantReturnPolicyInStoreReturnsOffered =
  Property "inStoreReturnsOffered"

propertyThesisInSupportOf :: Property Thesis '[Text]
propertyThesisInSupportOf = Property "inSupportOf"

propertyJobPostingIncentiveCompensation ::
  Property JobPosting '[Text]
propertyJobPostingIncentiveCompensation =
  Property "incentiveCompensation"

propertyJobPostingIncentives :: Property JobPosting '[Text]
propertyJobPostingIncentives = Property "incentives"

propertyMusicCompositionIncludedComposition ::
  Property MusicComposition '[MusicComposition]
propertyMusicCompositionIncludedComposition =
  Property "includedComposition"

propertyDatasetIncludedDataCatalog ::
  Property Dataset '[DataCatalog]
propertyDatasetIncludedDataCatalog = Property "includedDataCatalog"

propertyDatasetIncludedInDataCatalog ::
  Property Dataset '[DataCatalog]
propertyDatasetIncludedInDataCatalog =
  Property "includedInDataCatalog"

propertyDrugIncludedInHealthInsurancePlan ::
  Property Drug '[HealthInsurancePlan]
propertyDrugIncludedInHealthInsurancePlan =
  Property "includedInHealthInsurancePlan"

propertyMedicalRiskEstimatorIncludedRiskFactor ::
  Property MedicalRiskEstimator '[MedicalRiskFactor]
propertyMedicalRiskEstimatorIncludedRiskFactor =
  Property "includedRiskFactor"

propertyTouristDestinationIncludesAttraction ::
  Property TouristDestination '[TouristAttraction]
propertyTouristDestinationIncludesAttraction =
  Property "includesAttraction"

propertyHealthInsurancePlanIncludesHealthPlanFormulary ::
  Property HealthInsurancePlan '[HealthPlanFormulary]
propertyHealthInsurancePlanIncludesHealthPlanFormulary =
  Property "includesHealthPlanFormulary"

propertyHealthInsurancePlanIncludesHealthPlanNetwork ::
  Property HealthInsurancePlan '[HealthPlanNetwork]
propertyHealthInsurancePlanIncludesHealthPlanNetwork =
  Property "includesHealthPlanNetwork"

propertyDemandIncludesObject ::
  Property Demand '[TypeAndQuantityNode]
propertyDemandIncludesObject = Property "includesObject"

propertyOfferIncludesObject ::
  Property Offer '[TypeAndQuantityNode]
propertyOfferIncludesObject = Property "includesObject"

propertyProductCollectionIncludesObject ::
  Property ProductCollection '[TypeAndQuantityNode]
propertyProductCollectionIncludesObject = Property "includesObject"

propertyMedicalRiskFactorIncreasesRiskOf ::
  Property MedicalRiskFactor '[MedicalEntity]
propertyMedicalRiskFactorIncreasesRiskOf =
  Property "increasesRiskOf"

propertyJobPostingIndustry ::
  Property JobPosting '[DefinedTerm, Text]
propertyJobPostingIndustry = Property "industry"

propertyDemandIneligibleRegion ::
  Property Demand '[Place, GeoShape, Text]
propertyDemandIneligibleRegion = Property "ineligibleRegion"

propertyOfferIneligibleRegion ::
  Property Offer '[Place, GeoShape, Text]
propertyOfferIneligibleRegion = Property "ineligibleRegion"

propertyMediaObjectIneligibleRegion ::
  Property MediaObject '[Place, GeoShape, Text]
propertyMediaObjectIneligibleRegion = Property "ineligibleRegion"

propertyActionAccessSpecificationIneligibleRegion ::
  Property ActionAccessSpecification '[Place, GeoShape, Text]
propertyActionAccessSpecificationIneligibleRegion =
  Property "ineligibleRegion"

propertyDeliveryChargeSpecificationIneligibleRegion ::
  Property DeliveryChargeSpecification '[Place, GeoShape, Text]
propertyDeliveryChargeSpecificationIneligibleRegion =
  Property "ineligibleRegion"

propertyInfectiousDiseaseInfectiousAgent ::
  Property InfectiousDisease '[Text]
propertyInfectiousDiseaseInfectiousAgent =
  Property "infectiousAgent"

propertyInfectiousDiseaseInfectiousAgentClass ::
  Property InfectiousDisease '[InfectiousAgentClass]
propertyInfectiousDiseaseInfectiousAgentClass =
  Property "infectiousAgentClass"

propertyRecipeIngredients :: Property Recipe '[Text]
propertyRecipeIngredients = Property "ingredients"

propertyComicStoryInker :: Property ComicStory '[Person]
propertyComicStoryInker = Property "inker"

propertyComicIssueInker :: Property ComicIssue '[Person]
propertyComicIssueInker = Property "inker"

propertyVisualArtworkInker :: Property VisualArtwork '[Person]
propertyVisualArtworkInker = Property "inker"

propertyMuscleInsertion :: Property Muscle '[AnatomicalStructure]
propertyMuscleInsertion = Property "insertion"

propertySoftwareApplicationInstallUrl ::
  Property SoftwareApplication '[URL]
propertySoftwareApplicationInstallUrl = Property "installUrl"

propertyCourseInstanceInstructor ::
  Property CourseInstance '[Person]
propertyCourseInstanceInstructor = Property "instructor"

propertyActionInstrument :: Property Action '[Thing]
propertyActionInstrument = Property "instrument"

propertyExercisePlanIntensity ::
  Property ExercisePlan '[QuantitativeValue, Text]
propertyExercisePlanIntensity = Property "intensity"

propertyDrugInteractingDrug :: Property Drug '[Drug]
propertyDrugInteractingDrug = Property "interactingDrug"

propertyInteractionCounterInteractionService ::
  Property InteractionCounter '[WebSite, SoftwareApplication]
propertyInteractionCounterInteractionService =
  Property "interactionService"

propertyOrganizationInteractionStatistic ::
  Property Organization '[InteractionCounter]
propertyOrganizationInteractionStatistic =
  Property "interactionStatistic"

propertyPersonInteractionStatistic ::
  Property Person '[InteractionCounter]
propertyPersonInteractionStatistic =
  Property "interactionStatistic"

propertyCreativeWorkInteractionStatistic ::
  Property CreativeWork '[InteractionCounter]
propertyCreativeWorkInteractionStatistic =
  Property "interactionStatistic"

propertyInteractionCounterInteractionType ::
  Property InteractionCounter '[Action]
propertyInteractionCounterInteractionType =
  Property "interactionType"

propertyCreativeWorkInteractivityType ::
  Property CreativeWork '[Text]
propertyCreativeWorkInteractivityType =
  Property "interactivityType"

propertyFinancialProductInterestRate ::
  Property FinancialProduct '[Number, QuantitativeValue]
propertyFinancialProductInterestRate = Property "interestRate"

propertyMediaObjectInterpretedAsClaim ::
  Property MediaObject '[Claim]
propertyMediaObjectInterpretedAsClaim =
  Property "interpretedAsClaim"

propertyCreativeWorkInterpretedAsClaim ::
  Property CreativeWork '[Claim]
propertyCreativeWorkInterpretedAsClaim =
  Property "interpretedAsClaim"

propertyDemandInventoryLevel ::
  Property Demand '[QuantitativeValue]
propertyDemandInventoryLevel = Property "inventoryLevel"

propertyOfferInventoryLevel :: Property Offer '[QuantitativeValue]
propertyOfferInventoryLevel = Property "inventoryLevel"

propertySomeProductsInventoryLevel ::
  Property SomeProducts '[QuantitativeValue]
propertySomeProductsInventoryLevel = Property "inventoryLevel"

propertyMedicalOrganizationIsAcceptingNewPatients ::
  Property MedicalOrganization '[Boolean]
propertyMedicalOrganizationIsAcceptingNewPatients =
  Property "isAcceptingNewPatients"

propertyEventIsAccessibleForFree :: Property Event '[Boolean]
propertyEventIsAccessibleForFree = Property "isAccessibleForFree"

propertyPlaceIsAccessibleForFree :: Property Place '[Boolean]
propertyPlaceIsAccessibleForFree = Property "isAccessibleForFree"

propertyCreativeWorkIsAccessibleForFree ::
  Property CreativeWork '[Boolean]
propertyCreativeWorkIsAccessibleForFree =
  Property "isAccessibleForFree"

propertyProductIsAccessoryOrSparePartFor ::
  Property Product '[Product]
propertyProductIsAccessoryOrSparePartFor =
  Property "isAccessoryOrSparePartFor"

propertyDrugIsAvailableGenerically :: Property Drug '[Boolean]
propertyDrugIsAvailableGenerically =
  Property "isAvailableGenerically"

propertyCreativeWorkIsBasedOn ::
  Property CreativeWork '[URL, Product, CreativeWork]
propertyCreativeWorkIsBasedOn = Property "isBasedOn"

propertyCreativeWorkIsBasedOnUrl ::
  Property CreativeWork '[URL, Product, CreativeWork]
propertyCreativeWorkIsBasedOnUrl = Property "isBasedOnUrl"

propertyProductIsConsumableFor :: Property Product '[Product]
propertyProductIsConsumableFor = Property "isConsumableFor"

propertyBioChemEntityIsEncodedByBioChemEntity ::
  Property BioChemEntity '[Gene]
propertyBioChemEntityIsEncodedByBioChemEntity =
  Property "isEncodedByBioChemEntity"

propertyProductIsFamilyFriendly :: Property Product '[Boolean]
propertyProductIsFamilyFriendly = Property "isFamilyFriendly"

propertyOfferIsFamilyFriendly :: Property Offer '[Boolean]
propertyOfferIsFamilyFriendly = Property "isFamilyFriendly"

propertyCreativeWorkIsFamilyFriendly ::
  Property CreativeWork '[Boolean]
propertyCreativeWorkIsFamilyFriendly = Property "isFamilyFriendly"

propertyOrderIsGift :: Property Order '[Boolean]
propertyOrderIsGift = Property "isGift"

propertyBioChemEntityIsInvolvedInBiologicalProcess ::
  Property BioChemEntity '[DefinedTerm, PropertyValue, URL]
propertyBioChemEntityIsInvolvedInBiologicalProcess =
  Property "isInvolvedInBiologicalProcess"

propertyBroadcastEventIsLiveBroadcast ::
  Property BroadcastEvent '[Boolean]
propertyBroadcastEventIsLiveBroadcast = Property "isLiveBroadcast"

propertyBioChemEntityIsLocatedInSubcellularLocation ::
  Property BioChemEntity '[URL, DefinedTerm, PropertyValue]
propertyBioChemEntityIsLocatedInSubcellularLocation =
  Property "isLocatedInSubcellularLocation"

propertyCreativeWorkIsPartOf ::
  Property CreativeWork '[URL, CreativeWork]
propertyCreativeWorkIsPartOf = Property "isPartOf"

propertyBioChemEntityIsPartOfBioChemEntity ::
  Property BioChemEntity '[BioChemEntity]
propertyBioChemEntityIsPartOfBioChemEntity =
  Property "isPartOfBioChemEntity"

propertyFloorPlanIsPlanForApartment ::
  Property FloorPlan '[Accommodation]
propertyFloorPlanIsPlanForApartment = Property "isPlanForApartment"

propertyDietarySupplementIsProprietary ::
  Property DietarySupplement '[Boolean]
propertyDietarySupplementIsProprietary = Property "isProprietary"

propertyDrugIsProprietary :: Property Drug '[Boolean]
propertyDrugIsProprietary = Property "isProprietary"

propertyServiceIsRelatedTo :: Property Service '[Product, Service]
propertyServiceIsRelatedTo = Property "isRelatedTo"

propertyProductIsRelatedTo :: Property Product '[Product, Service]
propertyProductIsRelatedTo = Property "isRelatedTo"

propertyDModelIsResizable :: Property DModel '[Boolean]
propertyDModelIsResizable = Property "isResizable"

propertyServiceIsSimilarTo :: Property Service '[Product, Service]
propertyServiceIsSimilarTo = Property "isSimilarTo"

propertyProductIsSimilarTo :: Property Product '[Product, Service]
propertyProductIsSimilarTo = Property "isSimilarTo"

propertyDeliveryTimeSettingsIsUnlabelledFallback ::
  Property DeliveryTimeSettings '[Boolean]
propertyDeliveryTimeSettingsIsUnlabelledFallback =
  Property "isUnlabelledFallback"

propertyShippingRateSettingsIsUnlabelledFallback ::
  Property ShippingRateSettings '[Boolean]
propertyShippingRateSettingsIsUnlabelledFallback =
  Property "isUnlabelledFallback"

propertyProductModelIsVariantOf ::
  Property ProductModel '[ProductModel, ProductGroup]
propertyProductModelIsVariantOf = Property "isVariantOf"

propertyProductIsVariantOf ::
  Property Product '[ProductModel, ProductGroup]
propertyProductIsVariantOf = Property "isVariantOf"

propertyBookIsbn :: Property Book '[Text]
propertyBookIsbn = Property "isbn"

propertyPlaceIsicV4 :: Property Place '[Text]
propertyPlaceIsicV4 = Property "isicV4"

propertyOrganizationIsicV4 :: Property Organization '[Text]
propertyOrganizationIsicV4 = Property "isicV4"

propertyPersonIsicV4 :: Property Person '[Text]
propertyPersonIsicV4 = Property "isicV4"

propertyOrganizationIso6523Code :: Property Organization '[Text]
propertyOrganizationIso6523Code = Property "iso6523Code"

propertyMusicRecordingIsrcCode :: Property MusicRecording '[Text]
propertyMusicRecordingIsrcCode = Property "isrcCode"

propertyDatasetIssn :: Property Dataset '[Text]
propertyDatasetIssn = Property "issn"

propertyWebSiteIssn :: Property WebSite '[Text]
propertyWebSiteIssn = Property "issn"

propertyBlogIssn :: Property Blog '[Text]
propertyBlogIssn = Property "issn"

propertyCreativeWorkSeriesIssn ::
  Property CreativeWorkSeries '[Text]
propertyCreativeWorkSeriesIssn = Property "issn"

propertyPublicationIssueIssueNumber ::
  Property PublicationIssue '[Integer, Text]
propertyPublicationIssueIssueNumber = Property "issueNumber"

propertyPermitIssuedBy :: Property Permit '[Organization]
propertyPermitIssuedBy = Property "issuedBy"

propertyTicketIssuedBy :: Property Ticket '[Organization]
propertyTicketIssuedBy = Property "issuedBy"

propertyPermitIssuedThrough :: Property Permit '[Service]
propertyPermitIssuedThrough = Property "issuedThrough"

propertyMusicCompositionIswcCode ::
  Property MusicComposition '[Text]
propertyMusicCompositionIswcCode = Property "iswcCode"

propertyListItemItem :: Property ListItem '[Thing]
propertyListItemItem = Property "item"

propertyDataFeedItemItem :: Property DataFeedItem '[Thing]
propertyDataFeedItemItem = Property "item"

propertyDemandItemCondition ::
  Property Demand '[OfferItemCondition]
propertyDemandItemCondition = Property "itemCondition"

propertyProductItemCondition ::
  Property Product '[OfferItemCondition]
propertyProductItemCondition = Property "itemCondition"

propertyOfferItemCondition :: Property Offer '[OfferItemCondition]
propertyOfferItemCondition = Property "itemCondition"

propertyMerchantReturnPolicyItemCondition ::
  Property MerchantReturnPolicy '[OfferItemCondition]
propertyMerchantReturnPolicyItemCondition =
  Property "itemCondition"

propertyMerchantReturnPolicyItemDefectReturnFees ::
  Property MerchantReturnPolicy '[ReturnFeesEnumeration]
propertyMerchantReturnPolicyItemDefectReturnFees =
  Property "itemDefectReturnFees"

propertyMerchantReturnPolicyItemDefectReturnLabelSource ::
  Property MerchantReturnPolicy '[ReturnLabelSourceEnumeration]
propertyMerchantReturnPolicyItemDefectReturnLabelSource =
  Property "itemDefectReturnLabelSource"

propertyMerchantReturnPolicyItemDefectReturnShippingFeesAmount ::
  Property MerchantReturnPolicy '[MonetaryAmount]
propertyMerchantReturnPolicyItemDefectReturnShippingFeesAmount =
  Property "itemDefectReturnShippingFeesAmount"

propertyItemListItemListElement ::
  Property ItemList '[ListItem, Text, Thing]
propertyItemListItemListElement = Property "itemListElement"

propertyItemListItemListOrder ::
  Property ItemList '[ItemListOrderType, Text]
propertyItemListItemListOrder = Property "itemListOrder"

propertyArchiveComponentItemLocation ::
  Property ArchiveComponent '[Place, Text, PostalAddress]
propertyArchiveComponentItemLocation = Property "itemLocation"

propertyDemandItemOffered ::
  Property
    Demand
    '[ Product,
       Service,
       MenuItem,
       CreativeWork,
       Event,
       AggregateOffer,
       Trip
     ]
propertyDemandItemOffered = Property "itemOffered"

propertyOfferItemOffered ::
  Property
    Offer
    '[ Product,
       Service,
       MenuItem,
       CreativeWork,
       Event,
       AggregateOffer,
       Trip
     ]
propertyOfferItemOffered = Property "itemOffered"

propertyReviewItemReviewed :: Property Review '[Thing]
propertyReviewItemReviewed = Property "itemReviewed"

propertyAggregateRatingItemReviewed ::
  Property AggregateRating '[Thing]
propertyAggregateRatingItemReviewed = Property "itemReviewed"

propertyParcelDeliveryItemShipped ::
  Property ParcelDelivery '[Product]
propertyParcelDeliveryItemShipped = Property "itemShipped"

propertyTripItinerary :: Property Trip '[ItemList, Place]
propertyTripItinerary = Property "itinerary"

propertyMolecularEntityIupacName ::
  Property MolecularEntity '[Text]
propertyMolecularEntityIupacName = Property "iupacName"

propertyJobPostingJobBenefits :: Property JobPosting '[Text]
propertyJobPostingJobBenefits = Property "jobBenefits"

propertyJobPostingJobImmediateStart ::
  Property JobPosting '[Boolean]
propertyJobPostingJobImmediateStart = Property "jobImmediateStart"

propertyJobPostingJobLocation :: Property JobPosting '[Place]
propertyJobPostingJobLocation = Property "jobLocation"

propertyJobPostingJobLocationType :: Property JobPosting '[Text]
propertyJobPostingJobLocationType = Property "jobLocationType"

propertyJobPostingJobStartDate :: Property JobPosting '[Text, Date]
propertyJobPostingJobStartDate = Property "jobStartDate"

propertyPersonJobTitle :: Property Person '[DefinedTerm, Text]
propertyPersonJobTitle = Property "jobTitle"

propertyLegislationJurisdiction ::
  Property Legislation '[AdministrativeArea, Text]
propertyLegislationJurisdiction = Property "jurisdiction"

propertyGovernmentServiceJurisdiction ::
  Property GovernmentService '[AdministrativeArea, Text]
propertyGovernmentServiceJurisdiction = Property "jurisdiction"

propertyCreativeWorkKeywords ::
  Property CreativeWork '[URL, DefinedTerm, Text]
propertyCreativeWorkKeywords = Property "keywords"

propertyOrganizationKeywords ::
  Property Organization '[URL, DefinedTerm, Text]
propertyOrganizationKeywords = Property "keywords"

propertyProductKeywords ::
  Property Product '[URL, DefinedTerm, Text]
propertyProductKeywords = Property "keywords"

propertyEventKeywords :: Property Event '[URL, DefinedTerm, Text]
propertyEventKeywords = Property "keywords"

propertyPlaceKeywords :: Property Place '[URL, DefinedTerm, Text]
propertyPlaceKeywords = Property "keywords"

propertyVehicleKnownVehicleDamages :: Property Vehicle '[Text]
propertyVehicleKnownVehicleDamages = Property "knownVehicleDamages"

propertyPersonKnows :: Property Person '[Person]
propertyPersonKnows = Property "knows"

propertyOrganizationKnowsAbout ::
  Property Organization '[Thing, Text, URL]
propertyOrganizationKnowsAbout = Property "knowsAbout"

propertyPersonKnowsAbout :: Property Person '[Thing, Text, URL]
propertyPersonKnowsAbout = Property "knowsAbout"

propertyOrganizationKnowsLanguage ::
  Property Organization '[Text, Language]
propertyOrganizationKnowsLanguage = Property "knowsLanguage"

propertyPersonKnowsLanguage :: Property Person '[Text, Language]
propertyPersonKnowsLanguage = Property "knowsLanguage"

propertyDrugLabelDetails :: Property Drug '[URL]
propertyDrugLabelDetails = Property "labelDetails"

propertyRentActionLandlord ::
  Property RentAction '[Person, Organization]
propertyRentActionLandlord = Property "landlord"

propertyCommunicateActionLanguage ::
  Property CommunicateAction '[Language]
propertyCommunicateActionLanguage = Property "language"

propertyWriteActionLanguage :: Property WriteAction '[Language]
propertyWriteActionLanguage = Property "language"

propertyWebPageLastReviewed :: Property WebPage '[Date]
propertyWebPageLastReviewed = Property "lastReviewed"

propertyPlaceLatitude :: Property Place '[Number, Text]
propertyPlaceLatitude = Property "latitude"

propertyGeoCoordinatesLatitude ::
  Property GeoCoordinates '[Number, Text]
propertyGeoCoordinatesLatitude = Property "latitude"

propertyFloorPlanLayoutImage ::
  Property FloorPlan '[URL, ImageObject]
propertyFloorPlanLayoutImage = Property "layoutImage"

propertyLearningResourceLearningResourceType ::
  Property LearningResource '[DefinedTerm, Text]
propertyLearningResourceLearningResourceType =
  Property "learningResourceType"

propertyCreativeWorkLearningResourceType ::
  Property CreativeWork '[DefinedTerm, Text]
propertyCreativeWorkLearningResourceType =
  Property "learningResourceType"

propertyAccommodationLeaseLength ::
  Property Accommodation '[QuantitativeValue, Duration]
propertyAccommodationLeaseLength = Property "leaseLength"

propertyOfferLeaseLength ::
  Property Offer '[QuantitativeValue, Duration]
propertyOfferLeaseLength = Property "leaseLength"

propertyRealEstateListingLeaseLength ::
  Property RealEstateListing '[QuantitativeValue, Duration]
propertyRealEstateListingLeaseLength = Property "leaseLength"

propertyOrganizationLegalName :: Property Organization '[Text]
propertyOrganizationLegalName = Property "legalName"

propertyDietarySupplementLegalStatus ::
  Property
    DietarySupplement
    '[ Text,
       DrugLegalStatus,
       MedicalEnumeration
     ]
propertyDietarySupplementLegalStatus = Property "legalStatus"

propertyDrugLegalStatus ::
  Property Drug '[Text, DrugLegalStatus, MedicalEnumeration]
propertyDrugLegalStatus = Property "legalStatus"

propertyMedicalEntityLegalStatus ::
  Property MedicalEntity '[Text, DrugLegalStatus, MedicalEnumeration]
propertyMedicalEntityLegalStatus = Property "legalStatus"

propertyLegislationLegislationApplies ::
  Property Legislation '[Legislation]
propertyLegislationLegislationApplies =
  Property "legislationApplies"

propertyLegislationLegislationChanges ::
  Property Legislation '[Legislation]
propertyLegislationLegislationChanges =
  Property "legislationChanges"

propertyLegislationLegislationConsolidates ::
  Property Legislation '[Legislation]
propertyLegislationLegislationConsolidates =
  Property "legislationConsolidates"

propertyLegislationLegislationDate :: Property Legislation '[Date]
propertyLegislationLegislationDate = Property "legislationDate"

propertyLegislationLegislationDateVersion ::
  Property Legislation '[Date]
propertyLegislationLegislationDateVersion =
  Property "legislationDateVersion"

propertyLegislationLegislationIdentifier ::
  Property Legislation '[URL, Text]
propertyLegislationLegislationIdentifier =
  Property "legislationIdentifier"

propertyLegislationLegislationJurisdiction ::
  Property Legislation '[AdministrativeArea, Text]
propertyLegislationLegislationJurisdiction =
  Property "legislationJurisdiction"

propertyLegislationLegislationLegalForce ::
  Property Legislation '[LegalForceStatus]
propertyLegislationLegislationLegalForce =
  Property "legislationLegalForce"

propertyLegislationObjectLegislationLegalValue ::
  Property LegislationObject '[LegalValueLevel]
propertyLegislationObjectLegislationLegalValue =
  Property "legislationLegalValue"

propertyLegislationLegislationPassedBy ::
  Property Legislation '[Organization, Person]
propertyLegislationLegislationPassedBy =
  Property "legislationPassedBy"

propertyLegislationLegislationResponsible ::
  Property Legislation '[Organization, Person]
propertyLegislationLegislationResponsible =
  Property "legislationResponsible"

propertyLegislationLegislationTransposes ::
  Property Legislation '[Legislation]
propertyLegislationLegislationTransposes =
  Property "legislationTransposes"

propertyLegislationLegislationType ::
  Property Legislation '[CategoryCode, Text]
propertyLegislationLegislationType = Property "legislationType"

propertyOrganizationLeiCode :: Property Organization '[Text]
propertyOrganizationLeiCode = Property "leiCode"

propertyBorrowActionLender ::
  Property BorrowAction '[Organization, Person]
propertyBorrowActionLender = Property "lender"

propertyQualitativeValueLesser ::
  Property QualitativeValue '[QualitativeValue]
propertyQualitativeValueLesser = Property "lesser"

propertyQualitativeValueLesserOrEqual ::
  Property QualitativeValue '[QualitativeValue]
propertyQualitativeValueLesserOrEqual = Property "lesserOrEqual"

propertyVisualArtworkLetterer :: Property VisualArtwork '[Person]
propertyVisualArtworkLetterer = Property "letterer"

propertyComicStoryLetterer :: Property ComicStory '[Person]
propertyComicStoryLetterer = Property "letterer"

propertyComicIssueLetterer :: Property ComicIssue '[Person]
propertyComicIssueLetterer = Property "letterer"

propertyCreativeWorkLicense ::
  Property CreativeWork '[URL, CreativeWork]
propertyCreativeWorkLicense = Property "license"

propertyGeoShapeLine :: Property GeoShape '[Text]
propertyGeoShapeLine = Property "line"

propertyLinkRoleLinkRelationship :: Property LinkRole '[Text]
propertyLinkRoleLinkRelationship = Property "linkRelationship"

propertyLiveBlogPostingLiveBlogUpdate ::
  Property LiveBlogPosting '[BlogPosting]
propertyLiveBlogPostingLiveBlogUpdate = Property "liveBlogUpdate"

propertyMortgageLoanLoanMortgageMandateAmount ::
  Property MortgageLoan '[MonetaryAmount]
propertyMortgageLoanLoanMortgageMandateAmount =
  Property "loanMortgageMandateAmount"

propertyRepaymentSpecificationLoanPaymentAmount ::
  Property RepaymentSpecification '[MonetaryAmount]
propertyRepaymentSpecificationLoanPaymentAmount =
  Property "loanPaymentAmount"

propertyRepaymentSpecificationLoanPaymentFrequency ::
  Property RepaymentSpecification '[Number]
propertyRepaymentSpecificationLoanPaymentFrequency =
  Property "loanPaymentFrequency"

propertyLoanOrCreditLoanRepaymentForm ::
  Property LoanOrCredit '[RepaymentSpecification]
propertyLoanOrCreditLoanRepaymentForm =
  Property "loanRepaymentForm"

propertyLoanOrCreditLoanTerm ::
  Property LoanOrCredit '[QuantitativeValue]
propertyLoanOrCreditLoanTerm = Property "loanTerm"

propertyLoanOrCreditLoanType :: Property LoanOrCredit '[URL, Text]
propertyLoanOrCreditLoanType = Property "loanType"

propertyActionLocation ::
  Property Action '[Place, Text, VirtualLocation, PostalAddress]
propertyActionLocation = Property "location"

propertyEventLocation ::
  Property Event '[Place, Text, VirtualLocation, PostalAddress]
propertyEventLocation = Property "location"

propertyOrganizationLocation ::
  Property
    Organization
    '[ Place,
       Text,
       VirtualLocation,
       PostalAddress
     ]
propertyOrganizationLocation = Property "location"

propertyInteractionCounterLocation ::
  Property
    InteractionCounter
    '[ Place,
       Text,
       VirtualLocation,
       PostalAddress
     ]
propertyInteractionCounterLocation = Property "location"

propertyCreativeWorkLocationCreated ::
  Property CreativeWork '[Place]
propertyCreativeWorkLocationCreated = Property "locationCreated"

propertyLodgingReservationLodgingUnitDescription ::
  Property LodgingReservation '[Text]
propertyLodgingReservationLodgingUnitDescription =
  Property "lodgingUnitDescription"

propertyLodgingReservationLodgingUnitType ::
  Property LodgingReservation '[Text, QualitativeValue]
propertyLodgingReservationLodgingUnitType =
  Property "lodgingUnitType"

propertyBrandLogo :: Property Brand '[ImageObject, URL]
propertyBrandLogo = Property "logo"

propertyServiceLogo :: Property Service '[ImageObject, URL]
propertyServiceLogo = Property "logo"

propertyOrganizationLogo ::
  Property Organization '[ImageObject, URL]
propertyOrganizationLogo = Property "logo"

propertyProductLogo :: Property Product '[ImageObject, URL]
propertyProductLogo = Property "logo"

propertyPlaceLogo :: Property Place '[ImageObject, URL]
propertyPlaceLogo = Property "logo"

propertyPlaceLongitude :: Property Place '[Text, Number]
propertyPlaceLongitude = Property "longitude"

propertyGeoCoordinatesLongitude ::
  Property GeoCoordinates '[Text, Number]
propertyGeoCoordinatesLongitude = Property "longitude"

propertyWinActionLoser :: Property WinAction '[Person]
propertyWinActionLoser = Property "loser"

propertyAggregateOfferLowPrice ::
  Property AggregateOffer '[Number, Text]
propertyAggregateOfferLowPrice = Property "lowPrice"

propertyMusicCompositionLyricist ::
  Property MusicComposition '[Person]
propertyMusicCompositionLyricist = Property "lyricist"

propertyMusicCompositionLyrics ::
  Property MusicComposition '[CreativeWork]
propertyMusicCompositionLyrics = Property "lyrics"

propertyWebPageMainContentOfPage ::
  Property WebPage '[WebPageElement]
propertyWebPageMainContentOfPage = Property "mainContentOfPage"

propertyCreativeWorkMainEntity :: Property CreativeWork '[Thing]
propertyCreativeWorkMainEntity = Property "mainEntity"

propertyThingMainEntityOfPage ::
  Property Thing '[URL, CreativeWork]
propertyThingMainEntityOfPage = Property "mainEntityOfPage"

propertyCreativeWorkMaintainer ::
  Property CreativeWork '[Person, Organization]
propertyCreativeWorkMaintainer = Property "maintainer"

propertyOrganizationMakesOffer :: Property Organization '[Offer]
propertyOrganizationMakesOffer = Property "makesOffer"

propertyPersonMakesOffer :: Property Person '[Offer]
propertyPersonMakesOffer = Property "makesOffer"

propertyProductManufacturer :: Property Product '[Organization]
propertyProductManufacturer = Property "manufacturer"

propertyPlaceMap :: Property Place '[URL]
propertyPlaceMap = Property "map"

propertyMapMapType :: Property Map '[MapCategoryType]
propertyMapMapType = Property "mapType"

propertyPlaceMaps :: Property Place '[URL]
propertyPlaceMaps = Property "maps"

propertyObservationMarginOfError ::
  Property Observation '[QuantitativeValue]
propertyObservationMarginOfError = Property "marginOfError"

propertyNewsMediaOrganizationMasthead ::
  Property NewsMediaOrganization '[URL, CreativeWork]
propertyNewsMediaOrganizationMasthead = Property "masthead"

propertyProductMaterial :: Property Product '[Product, URL, Text]
propertyProductMaterial = Property "material"

propertyCreativeWorkMaterial ::
  Property CreativeWork '[Product, URL, Text]
propertyCreativeWorkMaterial = Property "material"

propertyCreativeWorkMaterialExtent ::
  Property CreativeWork '[QuantitativeValue, Text]
propertyCreativeWorkMaterialExtent = Property "materialExtent"

propertyMathSolverMathExpression ::
  Property MathSolver '[SolveMathAction, Text]
propertyMathSolverMathExpression = Property "mathExpression"

propertyPriceSpecificationMaxPrice ::
  Property PriceSpecification '[Number]
propertyPriceSpecificationMaxPrice = Property "maxPrice"

propertyQuantitativeValueMaxValue ::
  Property QuantitativeValue '[Number]
propertyQuantitativeValueMaxValue = Property "maxValue"

propertyPropertyValueSpecificationMaxValue ::
  Property PropertyValueSpecification '[Number]
propertyPropertyValueSpecificationMaxValue = Property "maxValue"

propertyPropertyValueMaxValue :: Property PropertyValue '[Number]
propertyPropertyValueMaxValue = Property "maxValue"

propertyMonetaryAmountMaxValue :: Property MonetaryAmount '[Number]
propertyMonetaryAmountMaxValue = Property "maxValue"

propertyPlaceMaximumAttendeeCapacity :: Property Place '[Integer]
propertyPlaceMaximumAttendeeCapacity =
  Property "maximumAttendeeCapacity"

propertyEventMaximumAttendeeCapacity :: Property Event '[Integer]
propertyEventMaximumAttendeeCapacity =
  Property "maximumAttendeeCapacity"

propertyEducationalOccupationalProgramMaximumEnrollment ::
  Property EducationalOccupationalProgram '[Integer]
propertyEducationalOccupationalProgramMaximumEnrollment =
  Property "maximumEnrollment"

propertyDietarySupplementMaximumIntake ::
  Property DietarySupplement '[MaximumDoseSchedule]
propertyDietarySupplementMaximumIntake = Property "maximumIntake"

propertySubstanceMaximumIntake ::
  Property Substance '[MaximumDoseSchedule]
propertySubstanceMaximumIntake = Property "maximumIntake"

propertyDrugStrengthMaximumIntake ::
  Property DrugStrength '[MaximumDoseSchedule]
propertyDrugStrengthMaximumIntake = Property "maximumIntake"

propertyDrugMaximumIntake :: Property Drug '[MaximumDoseSchedule]
propertyDrugMaximumIntake = Property "maximumIntake"

propertyEventMaximumPhysicalAttendeeCapacity ::
  Property Event '[Integer]
propertyEventMaximumPhysicalAttendeeCapacity =
  Property "maximumPhysicalAttendeeCapacity"

propertyEventMaximumVirtualAttendeeCapacity ::
  Property Event '[Integer]
propertyEventMaximumVirtualAttendeeCapacity =
  Property "maximumVirtualAttendeeCapacity"

propertyFlightMealService :: Property Flight '[Text]
propertyFlightMealService = Property "mealService"

propertyObservationMeasuredProperty ::
  Property Observation '[Property_]
propertyObservationMeasuredProperty = Property "measuredProperty"

propertyObservationMeasuredValue ::
  Property Observation '[DataType]
propertyObservationMeasuredValue = Property "measuredValue"

propertyDataCatalogMeasurementTechnique ::
  Property DataCatalog '[URL, Text]
propertyDataCatalogMeasurementTechnique =
  Property "measurementTechnique"

propertyDatasetMeasurementTechnique ::
  Property Dataset '[URL, Text]
propertyDatasetMeasurementTechnique =
  Property "measurementTechnique"

propertyPropertyValueMeasurementTechnique ::
  Property PropertyValue '[URL, Text]
propertyPropertyValueMeasurementTechnique =
  Property "measurementTechnique"

propertyDataDownloadMeasurementTechnique ::
  Property DataDownload '[URL, Text]
propertyDataDownloadMeasurementTechnique =
  Property "measurementTechnique"

propertyDietarySupplementMechanismOfAction ::
  Property DietarySupplement '[Text]
propertyDietarySupplementMechanismOfAction =
  Property "mechanismOfAction"

propertyDrugMechanismOfAction :: Property Drug '[Text]
propertyDrugMechanismOfAction = Property "mechanismOfAction"

propertyMediaReviewMediaAuthenticityCategory ::
  Property MediaReview '[MediaManipulationRatingEnumeration]
propertyMediaReviewMediaAuthenticityCategory =
  Property "mediaAuthenticityCategory"

propertyMediaReviewItemMediaItemAppearance ::
  Property MediaReviewItem '[MediaObject]
propertyMediaReviewItemMediaItemAppearance =
  Property "mediaItemAppearance"

propertyQuantitativeValueDistributionMedian ::
  Property QuantitativeValueDistribution '[Number]
propertyQuantitativeValueDistributionMedian = Property "median"

propertyMedicalWebPageMedicalAudience ::
  Property MedicalWebPage '[MedicalAudience, MedicalAudienceType]
propertyMedicalWebPageMedicalAudience = Property "medicalAudience"

propertyPhysicianMedicalSpecialty ::
  Property Physician '[MedicalSpecialty]
propertyPhysicianMedicalSpecialty = Property "medicalSpecialty"

propertyMedicalOrganizationMedicalSpecialty ::
  Property MedicalOrganization '[MedicalSpecialty]
propertyMedicalOrganizationMedicalSpecialty =
  Property "medicalSpecialty"

propertyMedicalClinicMedicalSpecialty ::
  Property MedicalClinic '[MedicalSpecialty]
propertyMedicalClinicMedicalSpecialty = Property "medicalSpecialty"

propertyHospitalMedicalSpecialty ::
  Property Hospital '[MedicalSpecialty]
propertyHospitalMedicalSpecialty = Property "medicalSpecialty"

propertyMedicalEntityMedicineSystem ::
  Property MedicalEntity '[MedicineSystem]
propertyMedicalEntityMedicineSystem = Property "medicineSystem"

propertyVehicleMeetsEmissionStandard ::
  Property Vehicle '[Text, URL, QualitativeValue]
propertyVehicleMeetsEmissionStandard =
  Property "meetsEmissionStandard"

propertyOrganizationMember ::
  Property Organization '[Organization, Person]
propertyOrganizationMember = Property "member"

propertyProgramMembershipMember ::
  Property ProgramMembership '[Organization, Person]
propertyProgramMembershipMember = Property "member"

propertyOrganizationMemberOf ::
  Property Organization '[Organization, ProgramMembership]
propertyOrganizationMemberOf = Property "memberOf"

propertyPersonMemberOf ::
  Property Person '[Organization, ProgramMembership]
propertyPersonMemberOf = Property "memberOf"

propertyOrganizationMembers ::
  Property Organization '[Organization, Person]
propertyOrganizationMembers = Property "members"

propertyProgramMembershipMembers ::
  Property ProgramMembership '[Organization, Person]
propertyProgramMembershipMembers = Property "members"

propertyProgramMembershipMembershipNumber ::
  Property ProgramMembership '[Text]
propertyProgramMembershipMembershipNumber =
  Property "membershipNumber"

propertyProgramMembershipMembershipPointsEarned ::
  Property ProgramMembership '[Number, QuantitativeValue]
propertyProgramMembershipMembershipPointsEarned =
  Property "membershipPointsEarned"

propertySoftwareApplicationMemoryRequirements ::
  Property SoftwareApplication '[Text, URL]
propertySoftwareApplicationMemoryRequirements =
  Property "memoryRequirements"

propertyCreativeWorkMentions :: Property CreativeWork '[Thing]
propertyCreativeWorkMentions = Property "mentions"

propertyFoodEstablishmentMenu ::
  Property FoodEstablishment '[Menu, Text, URL]
propertyFoodEstablishmentMenu = Property "menu"

propertyMenuItemMenuAddOn ::
  Property MenuItem '[MenuItem, MenuSection]
propertyMenuItemMenuAddOn = Property "menuAddOn"

propertyOrderMerchant :: Property Order '[Organization, Person]
propertyOrderMerchant = Property "merchant"

propertyMerchantReturnPolicySeasonalOverrideMerchantReturnDays ::
  Property
    MerchantReturnPolicySeasonalOverride
    '[ DateTime,
       Integer,
       Date
     ]
propertyMerchantReturnPolicySeasonalOverrideMerchantReturnDays =
  Property "merchantReturnDays"

propertyMerchantReturnPolicyMerchantReturnDays ::
  Property MerchantReturnPolicy '[DateTime, Integer, Date]
propertyMerchantReturnPolicyMerchantReturnDays =
  Property "merchantReturnDays"

propertyMerchantReturnPolicyMerchantReturnLink ::
  Property MerchantReturnPolicy '[URL]
propertyMerchantReturnPolicyMerchantReturnLink =
  Property "merchantReturnLink"

propertyMessageMessageAttachment ::
  Property Message '[CreativeWork]
propertyMessageMessageAttachment = Property "messageAttachment"

propertyVehicleMileageFromOdometer ::
  Property Vehicle '[QuantitativeValue]
propertyVehicleMileageFromOdometer = Property "mileageFromOdometer"

propertyPriceSpecificationMinPrice ::
  Property PriceSpecification '[Number]
propertyPriceSpecificationMinPrice = Property "minPrice"

propertyQuantitativeValueMinValue ::
  Property QuantitativeValue '[Number]
propertyQuantitativeValueMinValue = Property "minValue"

propertyPropertyValueSpecificationMinValue ::
  Property PropertyValueSpecification '[Number]
propertyPropertyValueSpecificationMinValue = Property "minValue"

propertyPropertyValueMinValue :: Property PropertyValue '[Number]
propertyPropertyValueMinValue = Property "minValue"

propertyMonetaryAmountMinValue :: Property MonetaryAmount '[Number]
propertyMonetaryAmountMinValue = Property "minValue"

propertyInvoiceMinimumPaymentDue ::
  Property Invoice '[MonetaryAmount, PriceSpecification]
propertyInvoiceMinimumPaymentDue = Property "minimumPaymentDue"

propertyNewsMediaOrganizationMissionCoveragePrioritiesPolicy ::
  Property NewsMediaOrganization '[CreativeWork, URL]
propertyNewsMediaOrganizationMissionCoveragePrioritiesPolicy =
  Property "missionCoveragePrioritiesPolicy"

propertyProductMobileUrl :: Property Product '[Text]
propertyProductMobileUrl = Property "mobileUrl"

propertyOfferMobileUrl :: Property Offer '[Text]
propertyOfferMobileUrl = Property "mobileUrl"

propertyProductModel :: Property Product '[ProductModel, Text]
propertyProductModel = Property "model"

propertyVehicleModelDate :: Property Vehicle '[Date]
propertyVehicleModelDate = Property "modelDate"

propertyReservationModifiedTime :: Property Reservation '[DateTime]
propertyReservationModifiedTime = Property "modifiedTime"

propertyMolecularEntityMolecularFormula ::
  Property MolecularEntity '[Text]
propertyMolecularEntityMolecularFormula =
  Property "molecularFormula"

propertyMolecularEntityMolecularWeight ::
  Property MolecularEntity '[QuantitativeValue, Text]
propertyMolecularEntityMolecularWeight = Property "molecularWeight"

propertyMolecularEntityMonoisotopicMolecularWeight ::
  Property MolecularEntity '[Text, QuantitativeValue]
propertyMolecularEntityMonoisotopicMolecularWeight =
  Property "monoisotopicMolecularWeight"

propertyPaymentCardMonthlyMinimumRepaymentAmount ::
  Property PaymentCard '[Number, MonetaryAmount]
propertyPaymentCardMonthlyMinimumRepaymentAmount =
  Property "monthlyMinimumRepaymentAmount"

propertyOccupationalExperienceRequirementsMonthsOfExperience ::
  Property OccupationalExperienceRequirements '[Number]
propertyOccupationalExperienceRequirementsMonthsOfExperience =
  Property "monthsOfExperience"

propertyDemandMpn :: Property Demand '[Text]
propertyDemandMpn = Property "mpn"

propertyProductMpn :: Property Product '[Text]
propertyProductMpn = Property "mpn"

propertyOfferMpn :: Property Offer '[Text]
propertyOfferMpn = Property "mpn"

propertyPropertyValueSpecificationMultipleValues ::
  Property PropertyValueSpecification '[Boolean]
propertyPropertyValueSpecificationMultipleValues =
  Property "multipleValues"

propertyMuscleMuscleAction :: Property Muscle '[Text]
propertyMuscleMuscleAction = Property "muscleAction"

propertyMusicCompositionMusicArrangement ::
  Property MusicComposition '[MusicComposition]
propertyMusicCompositionMusicArrangement =
  Property "musicArrangement"

propertyTVSeriesMusicBy :: Property TVSeries '[MusicGroup, Person]
propertyTVSeriesMusicBy = Property "musicBy"

propertyRadioSeriesMusicBy ::
  Property RadioSeries '[MusicGroup, Person]
propertyRadioSeriesMusicBy = Property "musicBy"

propertyMovieMusicBy :: Property Movie '[MusicGroup, Person]
propertyMovieMusicBy = Property "musicBy"

propertyMovieSeriesMusicBy ::
  Property MovieSeries '[MusicGroup, Person]
propertyMovieSeriesMusicBy = Property "musicBy"

propertyEpisodeMusicBy :: Property Episode '[MusicGroup, Person]
propertyEpisodeMusicBy = Property "musicBy"

propertyVideoGameMusicBy ::
  Property VideoGame '[MusicGroup, Person]
propertyVideoGameMusicBy = Property "musicBy"

propertyClipMusicBy :: Property Clip '[MusicGroup, Person]
propertyClipMusicBy = Property "musicBy"

propertyVideoGameSeriesMusicBy ::
  Property VideoGameSeries '[MusicGroup, Person]
propertyVideoGameSeriesMusicBy = Property "musicBy"

propertyVideoObjectMusicBy ::
  Property VideoObject '[MusicGroup, Person]
propertyVideoObjectMusicBy = Property "musicBy"

propertyMusicCompositionMusicCompositionForm ::
  Property MusicComposition '[Text]
propertyMusicCompositionMusicCompositionForm =
  Property "musicCompositionForm"

propertyMusicGroupMusicGroupMember :: Property MusicGroup '[Person]
propertyMusicGroupMusicGroupMember = Property "musicGroupMember"

propertyMusicReleaseMusicReleaseFormat ::
  Property MusicRelease '[MusicReleaseFormatType]
propertyMusicReleaseMusicReleaseFormat =
  Property "musicReleaseFormat"

propertyMusicCompositionMusicalKey ::
  Property MusicComposition '[Text]
propertyMusicCompositionMusicalKey = Property "musicalKey"

propertyOrganizationNaics :: Property Organization '[Text]
propertyOrganizationNaics = Property "naics"

propertyPersonNaics :: Property Person '[Text]
propertyPersonNaics = Property "naics"

propertyThingName :: Property Thing '[Text]
propertyThingName = Property "name"

propertyRoleNamedPosition :: Property Role '[Text, URL]
propertyRoleNamedPosition = Property "namedPosition"

propertyPersonNationality :: Property Person '[Country]
propertyPersonNationality = Property "nationality"

propertyMedicalConditionNaturalProgression ::
  Property MedicalCondition '[Text]
propertyMedicalConditionNaturalProgression =
  Property "naturalProgression"

propertyProductNegativeNotes ::
  Property Product '[ListItem, Text, ItemList, WebContent]
propertyProductNegativeNotes = Property "negativeNotes"

propertyReviewNegativeNotes ::
  Property Review '[ListItem, Text, ItemList, WebContent]
propertyReviewNegativeNotes = Property "negativeNotes"

propertyMuscleNerve :: Property Muscle '[Nerve]
propertyMuscleNerve = Property "nerve"

propertyNerveNerveMotor :: Property Nerve '[Muscle]
propertyNerveNerveMotor = Property "nerveMotor"

propertyPersonNetWorth ::
  Property Person '[MonetaryAmount, PriceSpecification]
propertyPersonNetWorth = Property "netWorth"

propertySpecialAnnouncementNewsUpdatesAndGuidelines ::
  Property SpecialAnnouncement '[WebContent, URL]
propertySpecialAnnouncementNewsUpdatesAndGuidelines =
  Property "newsUpdatesAndGuidelines"

propertyListItemNextItem :: Property ListItem '[ListItem]
propertyListItemNextItem = Property "nextItem"

propertyNewsMediaOrganizationNoBylinesPolicy ::
  Property NewsMediaOrganization '[CreativeWork, URL]
propertyNewsMediaOrganizationNoBylinesPolicy =
  Property "noBylinesPolicy"

propertyQualitativeValueNonEqual ::
  Property QualitativeValue '[QualitativeValue]
propertyQualitativeValueNonEqual = Property "nonEqual"

propertyDietarySupplementNonProprietaryName ::
  Property DietarySupplement '[Text]
propertyDietarySupplementNonProprietaryName =
  Property "nonProprietaryName"

propertyDrugNonProprietaryName :: Property Drug '[Text]
propertyDrugNonProprietaryName = Property "nonProprietaryName"

propertyOrganizationNonprofitStatus ::
  Property Organization '[NonprofitType]
propertyOrganizationNonprofitStatus = Property "nonprofitStatus"

propertyMedicalTestNormalRange ::
  Property MedicalTest '[Text, MedicalEnumeration]
propertyMedicalTestNormalRange = Property "normalRange"

propertyProductNsn :: Property Product '[Text]
propertyProductNsn = Property "nsn"

propertyLodgingReservationNumAdults ::
  Property LodgingReservation '[QuantitativeValue, Integer]
propertyLodgingReservationNumAdults = Property "numAdults"

propertyLodgingReservationNumChildren ::
  Property LodgingReservation '[Integer, QuantitativeValue]
propertyLodgingReservationNumChildren = Property "numChildren"

propertyStatisticalPopulationNumConstraints ::
  Property StatisticalPopulation '[Integer]
propertyStatisticalPopulationNumConstraints =
  Property "numConstraints"

propertyMusicPlaylistNumTracks :: Property MusicPlaylist '[Integer]
propertyMusicPlaylistNumTracks = Property "numTracks"

propertyApartmentComplexNumberOfAccommodationUnits ::
  Property ApartmentComplex '[QuantitativeValue]
propertyApartmentComplexNumberOfAccommodationUnits =
  Property "numberOfAccommodationUnits"

propertyFloorPlanNumberOfAccommodationUnits ::
  Property FloorPlan '[QuantitativeValue]
propertyFloorPlanNumberOfAccommodationUnits =
  Property "numberOfAccommodationUnits"

propertyVehicleNumberOfAirbags :: Property Vehicle '[Number, Text]
propertyVehicleNumberOfAirbags = Property "numberOfAirbags"

propertyApartmentComplexNumberOfAvailableAccommodationUnits ::
  Property ApartmentComplex '[QuantitativeValue]
propertyApartmentComplexNumberOfAvailableAccommodationUnits =
  Property "numberOfAvailableAccommodationUnits"

propertyFloorPlanNumberOfAvailableAccommodationUnits ::
  Property FloorPlan '[QuantitativeValue]
propertyFloorPlanNumberOfAvailableAccommodationUnits =
  Property "numberOfAvailableAccommodationUnits"

propertyVehicleNumberOfAxles ::
  Property Vehicle '[Number, QuantitativeValue]
propertyVehicleNumberOfAxles = Property "numberOfAxles"

propertyFloorPlanNumberOfBathroomsTotal ::
  Property FloorPlan '[Integer]
propertyFloorPlanNumberOfBathroomsTotal =
  Property "numberOfBathroomsTotal"

propertyAccommodationNumberOfBathroomsTotal ::
  Property Accommodation '[Integer]
propertyAccommodationNumberOfBathroomsTotal =
  Property "numberOfBathroomsTotal"

propertyFloorPlanNumberOfBedrooms ::
  Property FloorPlan '[Number, QuantitativeValue]
propertyFloorPlanNumberOfBedrooms = Property "numberOfBedrooms"

propertyAccommodationNumberOfBedrooms ::
  Property Accommodation '[Number, QuantitativeValue]
propertyAccommodationNumberOfBedrooms = Property "numberOfBedrooms"

propertyApartmentComplexNumberOfBedrooms ::
  Property ApartmentComplex '[Number, QuantitativeValue]
propertyApartmentComplexNumberOfBedrooms =
  Property "numberOfBedrooms"

propertyBedDetailsNumberOfBeds :: Property BedDetails '[Number]
propertyBedDetailsNumberOfBeds = Property "numberOfBeds"

propertyCourseNumberOfCredits ::
  Property Course '[StructuredValue, Integer]
propertyCourseNumberOfCredits = Property "numberOfCredits"

propertyEducationalOccupationalProgramNumberOfCredits ::
  Property EducationalOccupationalProgram '[StructuredValue, Integer]
propertyEducationalOccupationalProgramNumberOfCredits =
  Property "numberOfCredits"

propertyVehicleNumberOfDoors ::
  Property Vehicle '[Number, QuantitativeValue]
propertyVehicleNumberOfDoors = Property "numberOfDoors"

propertyBusinessAudienceNumberOfEmployees ::
  Property BusinessAudience '[QuantitativeValue]
propertyBusinessAudienceNumberOfEmployees =
  Property "numberOfEmployees"

propertyOrganizationNumberOfEmployees ::
  Property Organization '[QuantitativeValue]
propertyOrganizationNumberOfEmployees =
  Property "numberOfEmployees"

propertyRadioSeriesNumberOfEpisodes ::
  Property RadioSeries '[Integer]
propertyRadioSeriesNumberOfEpisodes = Property "numberOfEpisodes"

propertyVideoGameSeriesNumberOfEpisodes ::
  Property VideoGameSeries '[Integer]
propertyVideoGameSeriesNumberOfEpisodes =
  Property "numberOfEpisodes"

propertyTVSeriesNumberOfEpisodes :: Property TVSeries '[Integer]
propertyTVSeriesNumberOfEpisodes = Property "numberOfEpisodes"

propertyCreativeWorkSeasonNumberOfEpisodes ::
  Property CreativeWorkSeason '[Integer]
propertyCreativeWorkSeasonNumberOfEpisodes =
  Property "numberOfEpisodes"

propertyVehicleNumberOfForwardGears ::
  Property Vehicle '[Number, QuantitativeValue]
propertyVehicleNumberOfForwardGears =
  Property "numberOfForwardGears"

propertyFloorPlanNumberOfFullBathrooms ::
  Property FloorPlan '[Number]
propertyFloorPlanNumberOfFullBathrooms =
  Property "numberOfFullBathrooms"

propertyAccommodationNumberOfFullBathrooms ::
  Property Accommodation '[Number]
propertyAccommodationNumberOfFullBathrooms =
  Property "numberOfFullBathrooms"

propertyItemListNumberOfItems :: Property ItemList '[Integer]
propertyItemListNumberOfItems = Property "numberOfItems"

propertyRepaymentSpecificationNumberOfLoanPayments ::
  Property RepaymentSpecification '[Number]
propertyRepaymentSpecificationNumberOfLoanPayments =
  Property "numberOfLoanPayments"

propertyBookNumberOfPages :: Property Book '[Integer]
propertyBookNumberOfPages = Property "numberOfPages"

propertyFloorPlanNumberOfPartialBathrooms ::
  Property FloorPlan '[Number]
propertyFloorPlanNumberOfPartialBathrooms =
  Property "numberOfPartialBathrooms"

propertyAccommodationNumberOfPartialBathrooms ::
  Property Accommodation '[Number]
propertyAccommodationNumberOfPartialBathrooms =
  Property "numberOfPartialBathrooms"

propertyGameNumberOfPlayers :: Property Game '[QuantitativeValue]
propertyGameNumberOfPlayers = Property "numberOfPlayers"

propertyVideoGameSeriesNumberOfPlayers ::
  Property VideoGameSeries '[QuantitativeValue]
propertyVideoGameSeriesNumberOfPlayers = Property "numberOfPlayers"

propertyVehicleNumberOfPreviousOwners ::
  Property Vehicle '[QuantitativeValue, Number]
propertyVehicleNumberOfPreviousOwners =
  Property "numberOfPreviousOwners"

propertyApartmentNumberOfRooms ::
  Property Apartment '[Number, QuantitativeValue]
propertyApartmentNumberOfRooms = Property "numberOfRooms"

propertySingleFamilyResidenceNumberOfRooms ::
  Property SingleFamilyResidence '[Number, QuantitativeValue]
propertySingleFamilyResidenceNumberOfRooms =
  Property "numberOfRooms"

propertyFloorPlanNumberOfRooms ::
  Property FloorPlan '[Number, QuantitativeValue]
propertyFloorPlanNumberOfRooms = Property "numberOfRooms"

propertyLodgingBusinessNumberOfRooms ::
  Property LodgingBusiness '[Number, QuantitativeValue]
propertyLodgingBusinessNumberOfRooms = Property "numberOfRooms"

propertyAccommodationNumberOfRooms ::
  Property Accommodation '[Number, QuantitativeValue]
propertyAccommodationNumberOfRooms = Property "numberOfRooms"

propertyHouseNumberOfRooms ::
  Property House '[Number, QuantitativeValue]
propertyHouseNumberOfRooms = Property "numberOfRooms"

propertySuiteNumberOfRooms ::
  Property Suite '[Number, QuantitativeValue]
propertySuiteNumberOfRooms = Property "numberOfRooms"

propertyRadioSeriesNumberOfSeasons ::
  Property RadioSeries '[Integer]
propertyRadioSeriesNumberOfSeasons = Property "numberOfSeasons"

propertyVideoGameSeriesNumberOfSeasons ::
  Property VideoGameSeries '[Integer]
propertyVideoGameSeriesNumberOfSeasons = Property "numberOfSeasons"

propertyTVSeriesNumberOfSeasons :: Property TVSeries '[Integer]
propertyTVSeriesNumberOfSeasons = Property "numberOfSeasons"

propertyOrganizationRoleNumberedPosition ::
  Property OrganizationRole '[Number]
propertyOrganizationRoleNumberedPosition =
  Property "numberedPosition"

propertyMenuItemNutrition ::
  Property MenuItem '[NutritionInformation]
propertyMenuItemNutrition = Property "nutrition"

propertyRecipeNutrition :: Property Recipe '[NutritionInformation]
propertyRecipeNutrition = Property "nutrition"

propertyActionObject :: Property Action '[Thing]
propertyActionObject = Property "object"

propertyObservationObservationDate ::
  Property Observation '[DateTime]
propertyObservationObservationDate = Property "observationDate"

propertyObservationObservedNode ::
  Property Observation '[StatisticalPopulation]
propertyObservationObservedNode = Property "observedNode"

propertyHotelRoomOccupancy ::
  Property HotelRoom '[QuantitativeValue]
propertyHotelRoomOccupancy = Property "occupancy"

propertySuiteOccupancy :: Property Suite '[QuantitativeValue]
propertySuiteOccupancy = Property "occupancy"

propertyApartmentOccupancy ::
  Property Apartment '[QuantitativeValue]
propertyApartmentOccupancy = Property "occupancy"

propertySingleFamilyResidenceOccupancy ::
  Property SingleFamilyResidence '[QuantitativeValue]
propertySingleFamilyResidenceOccupancy = Property "occupancy"

propertyOccupationOccupationLocation ::
  Property Occupation '[AdministrativeArea]
propertyOccupationOccupationLocation =
  Property "occupationLocation"

propertyJobPostingOccupationalCategory ::
  Property JobPosting '[Text, CategoryCode]
propertyJobPostingOccupationalCategory =
  Property "occupationalCategory"

propertyWorkBasedProgramOccupationalCategory ::
  Property WorkBasedProgram '[Text, CategoryCode]
propertyWorkBasedProgramOccupationalCategory =
  Property "occupationalCategory"

propertyEducationalOccupationalProgramOccupationalCategory ::
  Property EducationalOccupationalProgram '[Text, CategoryCode]
propertyEducationalOccupationalProgramOccupationalCategory =
  Property "occupationalCategory"

propertyOccupationOccupationalCategory ::
  Property Occupation '[Text, CategoryCode]
propertyOccupationOccupationalCategory =
  Property "occupationalCategory"

propertyEducationalOccupationalProgramOccupationalCredentialAwarded ::
  Property
    EducationalOccupationalProgram
    '[ EducationalOccupationalCredential,
       Text,
       URL
     ]
propertyEducationalOccupationalProgramOccupationalCredentialAwarded =
  Property "occupationalCredentialAwarded"

propertyCourseOccupationalCredentialAwarded ::
  Property Course '[EducationalOccupationalCredential, Text, URL]
propertyCourseOccupationalCredentialAwarded =
  Property "occupationalCredentialAwarded"

propertyAggregateOfferOfferCount ::
  Property AggregateOffer '[Integer]
propertyAggregateOfferOfferCount = Property "offerCount"

propertyOfferOfferedBy :: Property Offer '[Organization, Person]
propertyOfferOfferedBy = Property "offeredBy"

propertyProductOffers :: Property Product '[Demand, Offer]
propertyProductOffers = Property "offers"

propertyEventOffers :: Property Event '[Demand, Offer]
propertyEventOffers = Property "offers"

propertyAggregateOfferOffers ::
  Property AggregateOffer '[Demand, Offer]
propertyAggregateOfferOffers = Property "offers"

propertyEducationalOccupationalProgramOffers ::
  Property EducationalOccupationalProgram '[Demand, Offer]
propertyEducationalOccupationalProgramOffers = Property "offers"

propertyTripOffers :: Property Trip '[Demand, Offer]
propertyTripOffers = Property "offers"

propertyServiceOffers :: Property Service '[Demand, Offer]
propertyServiceOffers = Property "offers"

propertyMenuItemOffers :: Property MenuItem '[Demand, Offer]
propertyMenuItemOffers = Property "offers"

propertyCreativeWorkOffers ::
  Property CreativeWork '[Demand, Offer]
propertyCreativeWorkOffers = Property "offers"

propertyHealthPlanFormularyOffersPrescriptionByMail ::
  Property HealthPlanFormulary '[Boolean]
propertyHealthPlanFormularyOffersPrescriptionByMail =
  Property "offersPrescriptionByMail"

propertyLocalBusinessOpeningHours :: Property LocalBusiness '[Text]
propertyLocalBusinessOpeningHours = Property "openingHours"

propertyCivicStructureOpeningHours ::
  Property CivicStructure '[Text]
propertyCivicStructureOpeningHours = Property "openingHours"

propertyPlaceOpeningHoursSpecification ::
  Property Place '[OpeningHoursSpecification]
propertyPlaceOpeningHoursSpecification =
  Property "openingHoursSpecification"

propertyOpeningHoursSpecificationOpens ::
  Property OpeningHoursSpecification '[Time]
propertyOpeningHoursSpecificationOpens = Property "opens"

propertySoftwareApplicationOperatingSystem ::
  Property SoftwareApplication '[Text]
propertySoftwareApplicationOperatingSystem =
  Property "operatingSystem"

propertyExerciseActionOpponent :: Property ExerciseAction '[Person]
propertyExerciseActionOpponent = Property "opponent"

propertyChooseActionOption :: Property ChooseAction '[Text, Thing]
propertyChooseActionOption = Property "option"

propertyOrderOrderDate :: Property Order '[DateTime, Date]
propertyOrderOrderDate = Property "orderDate"

propertyOrderOrderDelivery :: Property Order '[ParcelDelivery]
propertyOrderOrderDelivery = Property "orderDelivery"

propertyOrderItemOrderDelivery ::
  Property OrderItem '[ParcelDelivery]
propertyOrderItemOrderDelivery = Property "orderDelivery"

propertyOrderItemOrderItemNumber :: Property OrderItem '[Text]
propertyOrderItemOrderItemNumber = Property "orderItemNumber"

propertyOrderItemOrderItemStatus ::
  Property OrderItem '[OrderStatus]
propertyOrderItemOrderItemStatus = Property "orderItemStatus"

propertyOrderOrderNumber :: Property Order '[Text]
propertyOrderOrderNumber = Property "orderNumber"

propertyOrderItemOrderQuantity :: Property OrderItem '[Number]
propertyOrderItemOrderQuantity = Property "orderQuantity"

propertyOrderOrderStatus :: Property Order '[OrderStatus]
propertyOrderOrderStatus = Property "orderStatus"

propertyOrderItemOrderedItem ::
  Property OrderItem '[Product, Service, OrderItem]
propertyOrderItemOrderedItem = Property "orderedItem"

propertyOrderOrderedItem ::
  Property Order '[Product, Service, OrderItem]
propertyOrderOrderedItem = Property "orderedItem"

propertyEventOrganizer :: Property Event '[Organization, Person]
propertyEventOrganizer = Property "organizer"

propertyParcelDeliveryOriginAddress ::
  Property ParcelDelivery '[PostalAddress]
propertyParcelDeliveryOriginAddress = Property "originAddress"

propertyMediaReviewOriginalMediaContextDescription ::
  Property MediaReview '[Text]
propertyMediaReviewOriginalMediaContextDescription =
  Property "originalMediaContextDescription"

propertyMediaReviewOriginalMediaLink ::
  Property MediaReview '[WebPage, MediaObject, URL]
propertyMediaReviewOriginalMediaLink = Property "originalMediaLink"

propertyLymphaticVesselOriginatesFrom ::
  Property LymphaticVessel '[Vessel]
propertyLymphaticVesselOriginatesFrom = Property "originatesFrom"

propertyDrugOverdosage :: Property Drug '[Text]
propertyDrugOverdosage = Property "overdosage"

propertyOwnershipInfoOwnedFrom ::
  Property OwnershipInfo '[DateTime]
propertyOwnershipInfoOwnedFrom = Property "ownedFrom"

propertyOwnershipInfoOwnedThrough ::
  Property OwnershipInfo '[DateTime]
propertyOwnershipInfoOwnedThrough = Property "ownedThrough"

propertyNewsMediaOrganizationOwnershipFundingInfo ::
  Property
    NewsMediaOrganization
    '[ AboutPage,
       Text,
       CreativeWork,
       URL
     ]
propertyNewsMediaOrganizationOwnershipFundingInfo =
  Property "ownershipFundingInfo"

propertyOrganizationOwnershipFundingInfo ::
  Property Organization '[AboutPage, Text, CreativeWork, URL]
propertyOrganizationOwnershipFundingInfo =
  Property "ownershipFundingInfo"

propertyOrganizationOwns ::
  Property Organization '[Product, OwnershipInfo]
propertyOrganizationOwns = Property "owns"

propertyPersonOwns :: Property Person '[Product, OwnershipInfo]
propertyPersonOwns = Property "owns"

propertyPublicationVolumePageEnd ::
  Property PublicationVolume '[Integer, Text]
propertyPublicationVolumePageEnd = Property "pageEnd"

propertyPublicationIssuePageEnd ::
  Property PublicationIssue '[Integer, Text]
propertyPublicationIssuePageEnd = Property "pageEnd"

propertyChapterPageEnd :: Property Chapter '[Integer, Text]
propertyChapterPageEnd = Property "pageEnd"

propertyArticlePageEnd :: Property Article '[Integer, Text]
propertyArticlePageEnd = Property "pageEnd"

propertyChapterPageStart :: Property Chapter '[Integer, Text]
propertyChapterPageStart = Property "pageStart"

propertyArticlePageStart :: Property Article '[Integer, Text]
propertyArticlePageStart = Property "pageStart"

propertyPublicationVolumePageStart ::
  Property PublicationVolume '[Integer, Text]
propertyPublicationVolumePageStart = Property "pageStart"

propertyPublicationIssuePageStart ::
  Property PublicationIssue '[Integer, Text]
propertyPublicationIssuePageStart = Property "pageStart"

propertyPublicationVolumePagination ::
  Property PublicationVolume '[Text]
propertyPublicationVolumePagination = Property "pagination"

propertyPublicationIssuePagination ::
  Property PublicationIssue '[Text]
propertyPublicationIssuePagination = Property "pagination"

propertyChapterPagination :: Property Chapter '[Text]
propertyChapterPagination = Property "pagination"

propertyArticlePagination :: Property Article '[Text]
propertyArticlePagination = Property "pagination"

propertyPersonParent :: Property Person '[Person]
propertyPersonParent = Property "parent"

propertyCommentParentItem :: Property Comment '[Comment]
propertyCommentParentItem = Property "parentItem"

propertyOrganizationParentOrganization ::
  Property Organization '[Organization]
propertyOrganizationParentOrganization =
  Property "parentOrganization"

propertyBroadcastServiceParentService ::
  Property BroadcastService '[BroadcastService]
propertyBroadcastServiceParentService = Property "parentService"

propertyTaxonParentTaxon :: Property Taxon '[URL, Text, Taxon]
propertyTaxonParentTaxon = Property "parentTaxon"

propertyPersonParents :: Property Person '[Person]
propertyPersonParents = Property "parents"

propertyClipPartOfEpisode :: Property Clip '[Episode]
propertyClipPartOfEpisode = Property "partOfEpisode"

propertyOrderPartOfInvoice :: Property Order '[Invoice]
propertyOrderPartOfInvoice = Property "partOfInvoice"

propertyParcelDeliveryPartOfOrder ::
  Property ParcelDelivery '[Order]
propertyParcelDeliveryPartOfOrder = Property "partOfOrder"

propertyEpisodePartOfSeason ::
  Property Episode '[CreativeWorkSeason]
propertyEpisodePartOfSeason = Property "partOfSeason"

propertyClipPartOfSeason :: Property Clip '[CreativeWorkSeason]
propertyClipPartOfSeason = Property "partOfSeason"

propertyEpisodePartOfSeries ::
  Property Episode '[CreativeWorkSeries]
propertyEpisodePartOfSeries = Property "partOfSeries"

propertyClipPartOfSeries :: Property Clip '[CreativeWorkSeries]
propertyClipPartOfSeries = Property "partOfSeries"

propertyCreativeWorkSeasonPartOfSeries ::
  Property CreativeWorkSeason '[CreativeWorkSeries]
propertyCreativeWorkSeasonPartOfSeries = Property "partOfSeries"

propertyAnatomicalStructurePartOfSystem ::
  Property AnatomicalStructure '[AnatomicalSystem]
propertyAnatomicalStructurePartOfSystem = Property "partOfSystem"

propertyTVEpisodePartOfTVSeries :: Property TVEpisode '[TVSeries]
propertyTVEpisodePartOfTVSeries = Property "partOfTVSeries"

propertyTVClipPartOfTVSeries :: Property TVClip '[TVSeries]
propertyTVClipPartOfTVSeries = Property "partOfTVSeries"

propertyTVSeasonPartOfTVSeries :: Property TVSeason '[TVSeries]
propertyTVSeasonPartOfTVSeries = Property "partOfTVSeries"

propertyTripPartOfTrip :: Property Trip '[Trip]
propertyTripPartOfTrip = Property "partOfTrip"

propertyActionParticipant ::
  Property Action '[Organization, Person]
propertyActionParticipant = Property "participant"

propertyTaxiReservationPartySize ::
  Property TaxiReservation '[QuantitativeValue, Integer]
propertyTaxiReservationPartySize = Property "partySize"

propertyFoodEstablishmentReservationPartySize ::
  Property FoodEstablishmentReservation '[QuantitativeValue, Integer]
propertyFoodEstablishmentReservationPartySize =
  Property "partySize"

propertyFlightReservationPassengerPriorityStatus ::
  Property FlightReservation '[Text, QualitativeValue]
propertyFlightReservationPassengerPriorityStatus =
  Property "passengerPriorityStatus"

propertyFlightReservationPassengerSequenceNumber ::
  Property FlightReservation '[Text]
propertyFlightReservationPassengerSequenceNumber =
  Property "passengerSequenceNumber"

propertyPhysicalActivityPathophysiology ::
  Property PhysicalActivity '[Text]
propertyPhysicalActivityPathophysiology =
  Property "pathophysiology"

propertyMedicalConditionPathophysiology ::
  Property MedicalCondition '[Text]
propertyMedicalConditionPathophysiology =
  Property "pathophysiology"

propertyProductPattern :: Property Product '[DefinedTerm, Text]
propertyProductPattern = Property "pattern"

propertyCreativeWorkPattern ::
  Property CreativeWork '[DefinedTerm, Text]
propertyCreativeWorkPattern = Property "pattern"

propertyVehiclePayload :: Property Vehicle '[QuantitativeValue]
propertyVehiclePayload = Property "payload"

propertyLocalBusinessPaymentAccepted ::
  Property LocalBusiness '[Text]
propertyLocalBusinessPaymentAccepted = Property "paymentAccepted"

propertyInvoicePaymentDue :: Property Invoice '[DateTime]
propertyInvoicePaymentDue = Property "paymentDue"

propertyOrderPaymentDue :: Property Order '[DateTime]
propertyOrderPaymentDue = Property "paymentDue"

propertyInvoicePaymentDueDate :: Property Invoice '[Date, DateTime]
propertyInvoicePaymentDueDate = Property "paymentDueDate"

propertyOrderPaymentDueDate :: Property Order '[Date, DateTime]
propertyOrderPaymentDueDate = Property "paymentDueDate"

propertyInvoicePaymentMethod :: Property Invoice '[PaymentMethod]
propertyInvoicePaymentMethod = Property "paymentMethod"

propertyOrderPaymentMethod :: Property Order '[PaymentMethod]
propertyOrderPaymentMethod = Property "paymentMethod"

propertyInvoicePaymentMethodId :: Property Invoice '[Text]
propertyInvoicePaymentMethodId = Property "paymentMethodId"

propertyOrderPaymentMethodId :: Property Order '[Text]
propertyOrderPaymentMethodId = Property "paymentMethodId"

propertyInvoicePaymentStatus ::
  Property Invoice '[Text, PaymentStatusType]
propertyInvoicePaymentStatus = Property "paymentStatus"

propertyOrderPaymentUrl :: Property Order '[URL]
propertyOrderPaymentUrl = Property "paymentUrl"

propertyVisualArtworkPenciler :: Property VisualArtwork '[Person]
propertyVisualArtworkPenciler = Property "penciler"

propertyComicStoryPenciler :: Property ComicStory '[Person]
propertyComicStoryPenciler = Property "penciler"

propertyComicIssuePenciler :: Property ComicIssue '[Person]
propertyComicIssuePenciler = Property "penciler"

propertyQuantitativeValueDistributionPercentile10 ::
  Property QuantitativeValueDistribution '[Number]
propertyQuantitativeValueDistributionPercentile10 =
  Property "percentile10"

propertyQuantitativeValueDistributionPercentile25 ::
  Property QuantitativeValueDistribution '[Number]
propertyQuantitativeValueDistributionPercentile25 =
  Property "percentile25"

propertyQuantitativeValueDistributionPercentile75 ::
  Property QuantitativeValueDistribution '[Number]
propertyQuantitativeValueDistributionPercentile75 =
  Property "percentile75"

propertyQuantitativeValueDistributionPercentile90 ::
  Property QuantitativeValueDistribution '[Number]
propertyQuantitativeValueDistributionPercentile90 =
  Property "percentile90"

propertyHowToPerformTime :: Property HowTo '[Duration]
propertyHowToPerformTime = Property "performTime"

propertyHowToDirectionPerformTime ::
  Property HowToDirection '[Duration]
propertyHowToDirectionPerformTime = Property "performTime"

propertyEventPerformer :: Property Event '[Person, Organization]
propertyEventPerformer = Property "performer"

propertyPersonPerformerIn :: Property Person '[Event]
propertyPersonPerformerIn = Property "performerIn"

propertyEventPerformers :: Property Event '[Person, Organization]
propertyEventPerformers = Property "performers"

propertyDigitalDocumentPermissionPermissionType ::
  Property DigitalDocumentPermission '[DigitalDocumentPermissionType]
propertyDigitalDocumentPermissionPermissionType =
  Property "permissionType"

propertySoftwareApplicationPermissions ::
  Property SoftwareApplication '[Text]
propertySoftwareApplicationPermissions = Property "permissions"

propertyPermitPermitAudience :: Property Permit '[Audience]
propertyPermitPermitAudience = Property "permitAudience"

propertyAccommodationPermittedUsage ::
  Property Accommodation '[Text]
propertyAccommodationPermittedUsage = Property "permittedUsage"

propertyFloorPlanPetsAllowed :: Property FloorPlan '[Text, Boolean]
propertyFloorPlanPetsAllowed = Property "petsAllowed"

propertyLodgingBusinessPetsAllowed ::
  Property LodgingBusiness '[Text, Boolean]
propertyLodgingBusinessPetsAllowed = Property "petsAllowed"

propertyAccommodationPetsAllowed ::
  Property Accommodation '[Text, Boolean]
propertyAccommodationPetsAllowed = Property "petsAllowed"

propertyApartmentComplexPetsAllowed ::
  Property ApartmentComplex '[Text, Boolean]
propertyApartmentComplexPetsAllowed = Property "petsAllowed"

propertyPronounceableTextPhoneticText ::
  Property PronounceableText '[Text]
propertyPronounceableTextPhoneticText = Property "phoneticText"

propertyPlacePhoto :: Property Place '[Photograph, ImageObject]
propertyPlacePhoto = Property "photo"

propertyPlacePhotos :: Property Place '[ImageObject, Photograph]
propertyPlacePhotos = Property "photos"

propertyJobPostingPhysicalRequirement ::
  Property JobPosting '[DefinedTerm, Text, URL]
propertyJobPostingPhysicalRequirement =
  Property "physicalRequirement"

propertyDietPhysiologicalBenefits :: Property Diet '[Text]
propertyDietPhysiologicalBenefits =
  Property "physiologicalBenefits"

propertyTaxiReservationPickupLocation ::
  Property TaxiReservation '[Place]
propertyTaxiReservationPickupLocation = Property "pickupLocation"

propertyRentalCarReservationPickupLocation ::
  Property RentalCarReservation '[Place]
propertyRentalCarReservationPickupLocation =
  Property "pickupLocation"

propertyRentalCarReservationPickupTime ::
  Property RentalCarReservation '[DateTime]
propertyRentalCarReservationPickupTime = Property "pickupTime"

propertyTaxiReservationPickupTime ::
  Property TaxiReservation '[DateTime]
propertyTaxiReservationPickupTime = Property "pickupTime"

propertyVideoGameSeriesPlayMode ::
  Property VideoGameSeries '[GamePlayMode]
propertyVideoGameSeriesPlayMode = Property "playMode"

propertyVideoGamePlayMode :: Property VideoGame '[GamePlayMode]
propertyVideoGamePlayMode = Property "playMode"

propertyMediaObjectPlayerType :: Property MediaObject '[Text]
propertyMediaObjectPlayerType = Property "playerType"

propertyGameServerPlayersOnline :: Property GameServer '[Integer]
propertyGameServerPlayersOnline = Property "playersOnline"

propertyGeoShapePolygon :: Property GeoShape '[Text]
propertyGeoShapePolygon = Property "polygon"

propertyStatisticalPopulationPopulationType ::
  Property StatisticalPopulation '[Class_]
propertyStatisticalPopulationPopulationType =
  Property "populationType"

propertyListItemPosition :: Property ListItem '[Text, Integer]
propertyListItemPosition = Property "position"

propertyCreativeWorkPosition ::
  Property CreativeWork '[Text, Integer]
propertyCreativeWorkPosition = Property "position"

propertyProductPositiveNotes ::
  Property Product '[Text, WebContent, ListItem, ItemList]
propertyProductPositiveNotes = Property "positiveNotes"

propertyReviewPositiveNotes ::
  Property Review '[Text, WebContent, ListItem, ItemList]
propertyReviewPositiveNotes = Property "positiveNotes"

propertyMedicalConditionPossibleComplication ::
  Property MedicalCondition '[Text]
propertyMedicalConditionPossibleComplication =
  Property "possibleComplication"

propertyMedicalSignOrSymptomPossibleTreatment ::
  Property MedicalSignOrSymptom '[MedicalTherapy]
propertyMedicalSignOrSymptomPossibleTreatment =
  Property "possibleTreatment"

propertyMedicalConditionPossibleTreatment ::
  Property MedicalCondition '[MedicalTherapy]
propertyMedicalConditionPossibleTreatment =
  Property "possibleTreatment"

propertyPostalAddressPostOfficeBoxNumber ::
  Property PostalAddress '[Text]
propertyPostalAddressPostOfficeBoxNumber =
  Property "postOfficeBoxNumber"

propertyMedicalDevicePostOp :: Property MedicalDevice '[Text]
propertyMedicalDevicePostOp = Property "postOp"

propertyPostalAddressPostalCode :: Property PostalAddress '[Text]
propertyPostalAddressPostalCode = Property "postalCode"

propertyGeoCoordinatesPostalCode :: Property GeoCoordinates '[Text]
propertyGeoCoordinatesPostalCode = Property "postalCode"

propertyDefinedRegionPostalCode :: Property DefinedRegion '[Text]
propertyDefinedRegionPostalCode = Property "postalCode"

propertyGeoShapePostalCode :: Property GeoShape '[Text]
propertyGeoShapePostalCode = Property "postalCode"

propertyPostalCodeRangeSpecificationPostalCodeBegin ::
  Property PostalCodeRangeSpecification '[Text]
propertyPostalCodeRangeSpecificationPostalCodeBegin =
  Property "postalCodeBegin"

propertyPostalCodeRangeSpecificationPostalCodeEnd ::
  Property PostalCodeRangeSpecification '[Text]
propertyPostalCodeRangeSpecificationPostalCodeEnd =
  Property "postalCodeEnd"

propertyDefinedRegionPostalCodePrefix ::
  Property DefinedRegion '[Text]
propertyDefinedRegionPostalCodePrefix = Property "postalCodePrefix"

propertyDefinedRegionPostalCodeRange ::
  Property DefinedRegion '[PostalCodeRangeSpecification]
propertyDefinedRegionPostalCodeRange = Property "postalCodeRange"

propertyThingPotentialAction :: Property Thing '[Action]
propertyThingPotentialAction = Property "potentialAction"

propertyChemicalSubstancePotentialUse ::
  Property ChemicalSubstance '[DefinedTerm]
propertyChemicalSubstancePotentialUse = Property "potentialUse"

propertyMolecularEntityPotentialUse ::
  Property MolecularEntity '[DefinedTerm]
propertyMolecularEntityPotentialUse = Property "potentialUse"

propertyMedicalDevicePreOp :: Property MedicalDevice '[Text]
propertyMedicalDevicePreOp = Property "preOp"

propertyProductModelPredecessorOf ::
  Property ProductModel '[ProductModel]
propertyProductModelPredecessorOf = Property "predecessorOf"

propertyDrugPregnancyCategory ::
  Property Drug '[DrugPregnancyCategory]
propertyDrugPregnancyCategory = Property "pregnancyCategory"

propertyDrugPregnancyWarning :: Property Drug '[Text]
propertyDrugPregnancyWarning = Property "pregnancyWarning"

propertyHowToDirectionPrepTime ::
  Property HowToDirection '[Duration]
propertyHowToDirectionPrepTime = Property "prepTime"

propertyHowToPrepTime :: Property HowTo '[Duration]
propertyHowToPrepTime = Property "prepTime"

propertyMedicalProcedurePreparation ::
  Property MedicalProcedure '[Text, MedicalEntity]
propertyMedicalProcedurePreparation = Property "preparation"

propertyDrugPrescribingInfo :: Property Drug '[URL]
propertyDrugPrescribingInfo = Property "prescribingInfo"

propertyDrugPrescriptionStatus ::
  Property Drug '[Text, DrugPrescriptionStatus]
propertyDrugPrescriptionStatus = Property "prescriptionStatus"

propertyListItemPreviousItem :: Property ListItem '[ListItem]
propertyListItemPreviousItem = Property "previousItem"

propertyEventPreviousStartDate :: Property Event '[Date]
propertyEventPreviousStartDate = Property "previousStartDate"

propertyPriceSpecificationPrice ::
  Property PriceSpecification '[Text, Number]
propertyPriceSpecificationPrice = Property "price"

propertyTradeActionPrice :: Property TradeAction '[Text, Number]
propertyTradeActionPrice = Property "price"

propertyOfferPrice :: Property Offer '[Text, Number]
propertyOfferPrice = Property "price"

propertyCompoundPriceSpecificationPriceComponent ::
  Property CompoundPriceSpecification '[UnitPriceSpecification]
propertyCompoundPriceSpecificationPriceComponent =
  Property "priceComponent"

propertyUnitPriceSpecificationPriceComponentType ::
  Property UnitPriceSpecification '[PriceComponentTypeEnumeration]
propertyUnitPriceSpecificationPriceComponentType =
  Property "priceComponentType"

propertyTradeActionPriceCurrency :: Property TradeAction '[Text]
propertyTradeActionPriceCurrency = Property "priceCurrency"

propertyOfferPriceCurrency :: Property Offer '[Text]
propertyOfferPriceCurrency = Property "priceCurrency"

propertyTicketPriceCurrency :: Property Ticket '[Text]
propertyTicketPriceCurrency = Property "priceCurrency"

propertyReservationPriceCurrency :: Property Reservation '[Text]
propertyReservationPriceCurrency = Property "priceCurrency"

propertyPriceSpecificationPriceCurrency ::
  Property PriceSpecification '[Text]
propertyPriceSpecificationPriceCurrency = Property "priceCurrency"

propertyLocalBusinessPriceRange :: Property LocalBusiness '[Text]
propertyLocalBusinessPriceRange = Property "priceRange"

propertyDemandPriceSpecification ::
  Property Demand '[PriceSpecification]
propertyDemandPriceSpecification = Property "priceSpecification"

propertyOfferPriceSpecification ::
  Property Offer '[PriceSpecification]
propertyOfferPriceSpecification = Property "priceSpecification"

propertyTradeActionPriceSpecification ::
  Property TradeAction '[PriceSpecification]
propertyTradeActionPriceSpecification =
  Property "priceSpecification"

propertyCompoundPriceSpecificationPriceType ::
  Property CompoundPriceSpecification '[Text, PriceTypeEnumeration]
propertyCompoundPriceSpecificationPriceType = Property "priceType"

propertyUnitPriceSpecificationPriceType ::
  Property UnitPriceSpecification '[Text, PriceTypeEnumeration]
propertyUnitPriceSpecificationPriceType = Property "priceType"

propertyOfferPriceValidUntil :: Property Offer '[Date]
propertyOfferPriceValidUntil = Property "priceValidUntil"

propertyWebPagePrimaryImageOfPage ::
  Property WebPage '[ImageObject]
propertyWebPagePrimaryImageOfPage = Property "primaryImageOfPage"

propertyMedicalConditionPrimaryPrevention ::
  Property MedicalCondition '[MedicalTherapy]
propertyMedicalConditionPrimaryPrevention =
  Property "primaryPrevention"

propertyNewsArticlePrintColumn :: Property NewsArticle '[Text]
propertyNewsArticlePrintColumn = Property "printColumn"

propertyNewsArticlePrintEdition :: Property NewsArticle '[Text]
propertyNewsArticlePrintEdition = Property "printEdition"

propertyNewsArticlePrintPage :: Property NewsArticle '[Text]
propertyNewsArticlePrintPage = Property "printPage"

propertyNewsArticlePrintSection :: Property NewsArticle '[Text]
propertyNewsArticlePrintSection = Property "printSection"

propertyMedicalDeviceProcedure :: Property MedicalDevice '[Text]
propertyMedicalDeviceProcedure = Property "procedure"

propertyMedicalProcedureProcedureType ::
  Property MedicalProcedure '[MedicalProcedureType]
propertyMedicalProcedureProcedureType = Property "procedureType"

propertyServiceChannelProcessingTime ::
  Property ServiceChannel '[Duration]
propertyServiceChannelProcessingTime = Property "processingTime"

propertySoftwareApplicationProcessorRequirements ::
  Property SoftwareApplication '[Text]
propertySoftwareApplicationProcessorRequirements =
  Property "processorRequirements"

propertyCreativeWorkProducer ::
  Property CreativeWork '[Organization, Person]
propertyCreativeWorkProducer = Property "producer"

propertyServiceProduces :: Property Service '[Thing]
propertyServiceProduces = Property "produces"

propertyProductGroupProductGroupID :: Property ProductGroup '[Text]
propertyProductGroupProductGroupID = Property "productGroupID"

propertyProductProductID :: Property Product '[Text]
propertyProductProductID = Property "productID"

propertyContactPointProductSupported ::
  Property ContactPoint '[Text, Product]
propertyContactPointProductSupported = Property "productSupported"

propertyMediaObjectProductionCompany ::
  Property MediaObject '[Organization]
propertyMediaObjectProductionCompany = Property "productionCompany"

propertyEpisodeProductionCompany ::
  Property Episode '[Organization]
propertyEpisodeProductionCompany = Property "productionCompany"

propertyVideoGameSeriesProductionCompany ::
  Property VideoGameSeries '[Organization]
propertyVideoGameSeriesProductionCompany =
  Property "productionCompany"

propertyTVSeriesProductionCompany ::
  Property TVSeries '[Organization]
propertyTVSeriesProductionCompany = Property "productionCompany"

propertyCreativeWorkSeasonProductionCompany ::
  Property CreativeWorkSeason '[Organization]
propertyCreativeWorkSeasonProductionCompany =
  Property "productionCompany"

propertyMovieProductionCompany :: Property Movie '[Organization]
propertyMovieProductionCompany = Property "productionCompany"

propertyMovieSeriesProductionCompany ::
  Property MovieSeries '[Organization]
propertyMovieSeriesProductionCompany = Property "productionCompany"

propertyRadioSeriesProductionCompany ::
  Property RadioSeries '[Organization]
propertyRadioSeriesProductionCompany = Property "productionCompany"

propertyProductProductionDate :: Property Product '[Date]
propertyProductProductionDate = Property "productionDate"

propertyVehicleProductionDate :: Property Vehicle '[Date]
propertyVehicleProductionDate = Property "productionDate"

propertyTechArticleProficiencyLevel :: Property TechArticle '[Text]
propertyTechArticleProficiencyLevel = Property "proficiencyLevel"

propertyReservationProgramMembershipUsed ::
  Property Reservation '[ProgramMembership]
propertyReservationProgramMembershipUsed =
  Property "programMembershipUsed"

propertyProgramMembershipProgramName ::
  Property ProgramMembership '[Text]
propertyProgramMembershipProgramName = Property "programName"

propertyEducationalOccupationalProgramProgramPrerequisites ::
  Property
    EducationalOccupationalProgram
    '[ Text,
       EducationalOccupationalCredential,
       Course,
       AlignmentObject
     ]
propertyEducationalOccupationalProgramProgramPrerequisites =
  Property "programPrerequisites"

propertyEducationalOccupationalProgramProgramType ::
  Property EducationalOccupationalProgram '[DefinedTerm, Text]
propertyEducationalOccupationalProgramProgramType =
  Property "programType"

propertySoftwareSourceCodeProgrammingLanguage ::
  Property SoftwareSourceCode '[Text, ComputerLanguage]
propertySoftwareSourceCodeProgrammingLanguage =
  Property "programmingLanguage"

propertyAPIReferenceProgrammingModel ::
  Property APIReference '[Text]
propertyAPIReferenceProgrammingModel = Property "programmingModel"

propertyPropertyValuePropertyID ::
  Property PropertyValue '[Text, URL]
propertyPropertyValuePropertyID = Property "propertyID"

propertyDietarySupplementProprietaryName ::
  Property DietarySupplement '[Text]
propertyDietarySupplementProprietaryName =
  Property "proprietaryName"

propertyDrugProprietaryName :: Property Drug '[Text]
propertyDrugProprietaryName = Property "proprietaryName"

propertyNutritionInformationProteinContent ::
  Property NutritionInformation '[Mass]
propertyNutritionInformationProteinContent =
  Property "proteinContent"

propertyServiceProvider :: Property Service '[Organization, Person]
propertyServiceProvider = Property "provider"

propertyEducationalOccupationalProgramProvider ::
  Property EducationalOccupationalProgram '[Organization, Person]
propertyEducationalOccupationalProgramProvider =
  Property "provider"

propertyReservationProvider ::
  Property Reservation '[Organization, Person]
propertyReservationProvider = Property "provider"

propertyCreativeWorkProvider ::
  Property CreativeWork '[Organization, Person]
propertyCreativeWorkProvider = Property "provider"

propertyParcelDeliveryProvider ::
  Property ParcelDelivery '[Organization, Person]
propertyParcelDeliveryProvider = Property "provider"

propertyTripProvider :: Property Trip '[Organization, Person]
propertyTripProvider = Property "provider"

propertyInvoiceProvider :: Property Invoice '[Organization, Person]
propertyInvoiceProvider = Property "provider"

propertyActionProvider :: Property Action '[Organization, Person]
propertyActionProvider = Property "provider"

propertyServiceProviderMobility :: Property Service '[Text]
propertyServiceProviderMobility = Property "providerMobility"

propertyBroadcastChannelProvidesBroadcastService ::
  Property BroadcastChannel '[BroadcastService]
propertyBroadcastChannelProvidesBroadcastService =
  Property "providesBroadcastService"

propertyServiceChannelProvidesService ::
  Property ServiceChannel '[Service]
propertyServiceChannelProvidesService = Property "providesService"

propertyPlacePublicAccess :: Property Place '[Boolean]
propertyPlacePublicAccess = Property "publicAccess"

propertySpecialAnnouncementPublicTransportClosuresInfo ::
  Property SpecialAnnouncement '[WebContent, URL]
propertySpecialAnnouncementPublicTransportClosuresInfo =
  Property "publicTransportClosuresInfo"

propertyCreativeWorkPublication ::
  Property CreativeWork '[PublicationEvent]
propertyCreativeWorkPublication = Property "publication"

propertyMedicalScholarlyArticlePublicationType ::
  Property MedicalScholarlyArticle '[Text]
propertyMedicalScholarlyArticlePublicationType =
  Property "publicationType"

propertyPublicationEventPublishedBy ::
  Property PublicationEvent '[Organization, Person]
propertyPublicationEventPublishedBy = Property "publishedBy"

propertyPublicationEventPublishedOn ::
  Property PublicationEvent '[BroadcastService]
propertyPublicationEventPublishedOn = Property "publishedOn"

propertyCreativeWorkPublisher ::
  Property CreativeWork '[Organization, Person]
propertyCreativeWorkPublisher = Property "publisher"

propertyCreativeWorkPublisherImprint ::
  Property CreativeWork '[Organization]
propertyCreativeWorkPublisherImprint = Property "publisherImprint"

propertyOrganizationPublishingPrinciples ::
  Property Organization '[CreativeWork, URL]
propertyOrganizationPublishingPrinciples =
  Property "publishingPrinciples"

propertyPersonPublishingPrinciples ::
  Property Person '[CreativeWork, URL]
propertyPersonPublishingPrinciples =
  Property "publishingPrinciples"

propertyCreativeWorkPublishingPrinciples ::
  Property CreativeWork '[CreativeWork, URL]
propertyCreativeWorkPublishingPrinciples =
  Property "publishingPrinciples"

propertyProductPurchaseDate :: Property Product '[Date]
propertyProductPurchaseDate = Property "purchaseDate"

propertyVehiclePurchaseDate :: Property Vehicle '[Date]
propertyVehiclePurchaseDate = Property "purchaseDate"

propertyOccupationQualifications ::
  Property Occupation '[EducationalOccupationalCredential, Text]
propertyOccupationQualifications = Property "qualifications"

propertyJobPostingQualifications ::
  Property JobPosting '[EducationalOccupationalCredential, Text]
propertyJobPostingQualifications = Property "qualifications"

propertySpecialAnnouncementQuarantineGuidelines ::
  Property SpecialAnnouncement '[URL, WebContent]
propertySpecialAnnouncementQuarantineGuidelines =
  Property "quarantineGuidelines"

propertySearchActionQuery :: Property SearchAction '[Text]
propertySearchActionQuery = Property "query"

propertyVideoGameSeriesQuest :: Property VideoGameSeries '[Thing]
propertyVideoGameSeriesQuest = Property "quest"

propertyGameQuest :: Property Game '[Thing]
propertyGameQuest = Property "quest"

propertyAskActionQuestion :: Property AskAction '[Question]
propertyAskActionQuestion = Property "question"

propertyAggregateRatingRatingCount ::
  Property AggregateRating '[Integer]
propertyAggregateRatingRatingCount = Property "ratingCount"

propertyRatingRatingExplanation :: Property Rating '[Text]
propertyRatingRatingExplanation = Property "ratingExplanation"

propertyRatingRatingValue :: Property Rating '[Number, Text]
propertyRatingRatingValue = Property "ratingValue"

propertyAudiobookReadBy :: Property Audiobook '[Person]
propertyAudiobookReadBy = Property "readBy"

propertyPropertyValueSpecificationReadonlyValue ::
  Property PropertyValueSpecification '[Boolean]
propertyPropertyValueSpecificationReadonlyValue =
  Property "readonlyValue"

propertyRentActionRealEstateAgent ::
  Property RentAction '[RealEstateAgent]
propertyRentActionRealEstateAgent = Property "realEstateAgent"

propertyCookActionRecipe :: Property CookAction '[Recipe]
propertyCookActionRecipe = Property "recipe"

propertyRecipeRecipeCategory :: Property Recipe '[Text]
propertyRecipeRecipeCategory = Property "recipeCategory"

propertyRecipeRecipeCuisine :: Property Recipe '[Text]
propertyRecipeRecipeCuisine = Property "recipeCuisine"

propertyRecipeRecipeIngredient :: Property Recipe '[Text]
propertyRecipeRecipeIngredient = Property "recipeIngredient"

propertyRecipeRecipeInstructions ::
  Property Recipe '[Text, ItemList, CreativeWork]
propertyRecipeRecipeInstructions = Property "recipeInstructions"

propertyRecipeRecipeYield ::
  Property Recipe '[QuantitativeValue, Text]
propertyRecipeRecipeYield = Property "recipeYield"

propertyTipActionRecipient ::
  Property TipAction '[Organization, ContactPoint, Person, Audience]
propertyTipActionRecipient = Property "recipient"

propertyCommunicateActionRecipient ::
  Property
    CommunicateAction
    '[ Organization,
       ContactPoint,
       Person,
       Audience
     ]
propertyCommunicateActionRecipient = Property "recipient"

propertyPayActionRecipient ::
  Property PayAction '[Organization, ContactPoint, Person, Audience]
propertyPayActionRecipient = Property "recipient"

propertyDonateActionRecipient ::
  Property
    DonateAction
    '[ Organization,
       ContactPoint,
       Person,
       Audience
     ]
propertyDonateActionRecipient = Property "recipient"

propertyReturnActionRecipient ::
  Property
    ReturnAction
    '[ Organization,
       ContactPoint,
       Person,
       Audience
     ]
propertyReturnActionRecipient = Property "recipient"

propertyGiveActionRecipient ::
  Property GiveAction '[Organization, ContactPoint, Person, Audience]
propertyGiveActionRecipient = Property "recipient"

propertyMessageRecipient ::
  Property Message '[Organization, ContactPoint, Person, Audience]
propertyMessageRecipient = Property "recipient"

propertySendActionRecipient ::
  Property SendAction '[Organization, ContactPoint, Person, Audience]
propertySendActionRecipient = Property "recipient"

propertyAuthorizeActionRecipient ::
  Property
    AuthorizeAction
    '[ Organization,
       ContactPoint,
       Person,
       Audience
     ]
propertyAuthorizeActionRecipient = Property "recipient"

propertyEducationalOccupationalCredentialRecognizedBy ::
  Property EducationalOccupationalCredential '[Organization]
propertyEducationalOccupationalCredentialRecognizedBy =
  Property "recognizedBy"

propertyMedicalEntityRecognizingAuthority ::
  Property MedicalEntity '[Organization]
propertyMedicalEntityRecognizingAuthority =
  Property "recognizingAuthority"

propertyMedicalGuidelineRecommendationRecommendationStrength ::
  Property MedicalGuidelineRecommendation '[Text]
propertyMedicalGuidelineRecommendationRecommendationStrength =
  Property "recommendationStrength"

propertyDietarySupplementRecommendedIntake ::
  Property DietarySupplement '[RecommendedDoseSchedule]
propertyDietarySupplementRecommendedIntake =
  Property "recommendedIntake"

propertyMusicReleaseRecordLabel ::
  Property MusicRelease '[Organization]
propertyMusicReleaseRecordLabel = Property "recordLabel"

propertyMusicCompositionRecordedAs ::
  Property MusicComposition '[MusicRecording]
propertyMusicCompositionRecordedAs = Property "recordedAs"

propertyCreativeWorkRecordedAt :: Property CreativeWork '[Event]
propertyCreativeWorkRecordedAt = Property "recordedAt"

propertyEventRecordedIn :: Property Event '[CreativeWork]
propertyEventRecordedIn = Property "recordedIn"

propertyMusicRecordingRecordingOf ::
  Property MusicRecording '[MusicComposition]
propertyMusicRecordingRecordingOf = Property "recordingOf"

propertyLoanOrCreditRecourseLoan ::
  Property LoanOrCredit '[Boolean]
propertyLoanOrCreditRecourseLoan = Property "recourseLoan"

propertyUnitPriceSpecificationReferenceQuantity ::
  Property UnitPriceSpecification '[QuantitativeValue]
propertyUnitPriceSpecificationReferenceQuantity =
  Property "referenceQuantity"

propertyInvoiceReferencesOrder :: Property Invoice '[Order]
propertyInvoiceReferencesOrder = Property "referencesOrder"

propertyMerchantReturnPolicyRefundType ::
  Property MerchantReturnPolicy '[RefundTypeEnumeration]
propertyMerchantReturnPolicyRefundType = Property "refundType"

propertyVeinRegionDrained ::
  Property Vein '[AnatomicalSystem, AnatomicalStructure]
propertyVeinRegionDrained = Property "regionDrained"

propertyLymphaticVesselRegionDrained ::
  Property LymphaticVessel '[AnatomicalSystem, AnatomicalStructure]
propertyLymphaticVesselRegionDrained = Property "regionDrained"

propertyMediaObjectRegionsAllowed :: Property MediaObject '[Place]
propertyMediaObjectRegionsAllowed = Property "regionsAllowed"

propertySuperficialAnatomyRelatedAnatomy ::
  Property
    SuperficialAnatomy
    '[ AnatomicalSystem,
       AnatomicalStructure
     ]
propertySuperficialAnatomyRelatedAnatomy =
  Property "relatedAnatomy"

propertyAnatomicalSystemRelatedCondition ::
  Property AnatomicalSystem '[MedicalCondition]
propertyAnatomicalSystemRelatedCondition =
  Property "relatedCondition"

propertySuperficialAnatomyRelatedCondition ::
  Property SuperficialAnatomy '[MedicalCondition]
propertySuperficialAnatomyRelatedCondition =
  Property "relatedCondition"

propertyAnatomicalStructureRelatedCondition ::
  Property AnatomicalStructure '[MedicalCondition]
propertyAnatomicalStructureRelatedCondition =
  Property "relatedCondition"

propertyDrugRelatedDrug :: Property Drug '[Drug]
propertyDrugRelatedDrug = Property "relatedDrug"

propertyWebPageRelatedLink :: Property WebPage '[URL]
propertyWebPageRelatedLink = Property "relatedLink"

propertyAnatomicalSystemRelatedStructure ::
  Property AnatomicalSystem '[AnatomicalStructure]
propertyAnatomicalSystemRelatedStructure =
  Property "relatedStructure"

propertyAnatomicalStructureRelatedTherapy ::
  Property AnatomicalStructure '[MedicalTherapy]
propertyAnatomicalStructureRelatedTherapy =
  Property "relatedTherapy"

propertySuperficialAnatomyRelatedTherapy ::
  Property SuperficialAnatomy '[MedicalTherapy]
propertySuperficialAnatomyRelatedTherapy =
  Property "relatedTherapy"

propertyAnatomicalSystemRelatedTherapy ::
  Property AnatomicalSystem '[MedicalTherapy]
propertyAnatomicalSystemRelatedTherapy = Property "relatedTherapy"

propertyPersonRelatedTo :: Property Person '[Person]
propertyPersonRelatedTo = Property "relatedTo"

propertyProductReleaseDate :: Property Product '[Date]
propertyProductReleaseDate = Property "releaseDate"

propertySoftwareApplicationReleaseNotes ::
  Property SoftwareApplication '[Text, URL]
propertySoftwareApplicationReleaseNotes = Property "releaseNotes"

propertyMusicReleaseReleaseOf ::
  Property MusicRelease '[MusicAlbum]
propertyMusicReleaseReleaseOf = Property "releaseOf"

propertyCreativeWorkReleasedEvent ::
  Property CreativeWork '[PublicationEvent]
propertyCreativeWorkReleasedEvent = Property "releasedEvent"

propertyJobPostingRelevantOccupation ::
  Property JobPosting '[Occupation]
propertyJobPostingRelevantOccupation =
  Property "relevantOccupation"

propertyMedicalEntityRelevantSpecialty ::
  Property MedicalEntity '[MedicalSpecialty]
propertyMedicalEntityRelevantSpecialty =
  Property "relevantSpecialty"

propertyEventRemainingAttendeeCapacity :: Property Event '[Integer]
propertyEventRemainingAttendeeCapacity =
  Property "remainingAttendeeCapacity"

propertyLoanOrCreditRenegotiableLoan ::
  Property LoanOrCredit '[Boolean]
propertyLoanOrCreditRenegotiableLoan = Property "renegotiableLoan"

propertyScheduleRepeatCount :: Property Schedule '[Integer]
propertyScheduleRepeatCount = Property "repeatCount"

propertyScheduleRepeatFrequency ::
  Property Schedule '[Text, Duration]
propertyScheduleRepeatFrequency = Property "repeatFrequency"

propertyExercisePlanRepetitions ::
  Property ExercisePlan '[Number, QuantitativeValue]
propertyExercisePlanRepetitions = Property "repetitions"

propertyReplaceActionReplacee :: Property ReplaceAction '[Thing]
propertyReplaceActionReplacee = Property "replacee"

propertyReplaceActionReplacer :: Property ReplaceAction '[Thing]
propertyReplaceActionReplacer = Property "replacer"

propertyUserCommentsReplyToUrl :: Property UserComments '[URL]
propertyUserCommentsReplyToUrl = Property "replyToUrl"

propertyReportReportNumber :: Property Report '[Text]
propertyReportReportNumber = Property "reportNumber"

propertyImageObjectRepresentativeOfPage ::
  Property ImageObject '[Boolean]
propertyImageObjectRepresentativeOfPage =
  Property "representativeOfPage"

propertyLoanOrCreditRequiredCollateral ::
  Property LoanOrCredit '[Thing, Text]
propertyLoanOrCreditRequiredCollateral =
  Property "requiredCollateral"

propertyPeopleAudienceRequiredGender ::
  Property PeopleAudience '[Text]
propertyPeopleAudienceRequiredGender = Property "requiredGender"

propertyPeopleAudienceRequiredMaxAge ::
  Property PeopleAudience '[Integer]
propertyPeopleAudienceRequiredMaxAge = Property "requiredMaxAge"

propertyPeopleAudienceRequiredMinAge ::
  Property PeopleAudience '[Integer]
propertyPeopleAudienceRequiredMinAge = Property "requiredMinAge"

propertyHowToItemRequiredQuantity ::
  Property HowToItem '[Text, Number, QuantitativeValue]
propertyHowToItemRequiredQuantity = Property "requiredQuantity"

propertySoftwareApplicationRequirements ::
  Property SoftwareApplication '[Text, URL]
propertySoftwareApplicationRequirements = Property "requirements"

propertyActionAccessSpecificationRequiresSubscription ::
  Property ActionAccessSpecification '[MediaSubscription, Boolean]
propertyActionAccessSpecificationRequiresSubscription =
  Property "requiresSubscription"

propertyMediaObjectRequiresSubscription ::
  Property MediaObject '[MediaSubscription, Boolean]
propertyMediaObjectRequiresSubscription =
  Property "requiresSubscription"

propertyReservationReservationFor :: Property Reservation '[Thing]
propertyReservationReservationFor = Property "reservationFor"

propertyReservationReservationId :: Property Reservation '[Text]
propertyReservationReservationId = Property "reservationId"

propertyReservationReservationStatus ::
  Property Reservation '[ReservationStatusType]
propertyReservationReservationStatus = Property "reservationStatus"

propertyReservationReservedTicket :: Property Reservation '[Ticket]
propertyReservationReservedTicket = Property "reservedTicket"

propertyOccupationResponsibilities :: Property Occupation '[Text]
propertyOccupationResponsibilities = Property "responsibilities"

propertyJobPostingResponsibilities :: Property JobPosting '[Text]
propertyJobPostingResponsibilities = Property "responsibilities"

propertyExercisePlanRestPeriods ::
  Property ExercisePlan '[QuantitativeValue, Text]
propertyExercisePlanRestPeriods = Property "restPeriods"

propertyMerchantReturnPolicyRestockingFee ::
  Property MerchantReturnPolicy '[MonetaryAmount, Number]
propertyMerchantReturnPolicyRestockingFee =
  Property "restockingFee"

propertyActionResult :: Property Action '[Thing]
propertyActionResult = Property "result"

propertyReplyActionResultComment :: Property ReplyAction '[Comment]
propertyReplyActionResultComment = Property "resultComment"

propertyCommentActionResultComment ::
  Property CommentAction '[Comment]
propertyCommentActionResultComment = Property "resultComment"

propertyReviewActionResultReview :: Property ReviewAction '[Review]
propertyReviewActionResultReview = Property "resultReview"

propertyMerchantReturnPolicyReturnFees ::
  Property MerchantReturnPolicy '[ReturnFeesEnumeration]
propertyMerchantReturnPolicyReturnFees = Property "returnFees"

propertyMerchantReturnPolicyReturnLabelSource ::
  Property MerchantReturnPolicy '[ReturnLabelSourceEnumeration]
propertyMerchantReturnPolicyReturnLabelSource =
  Property "returnLabelSource"

propertyMerchantReturnPolicyReturnMethod ::
  Property MerchantReturnPolicy '[ReturnMethodEnumeration]
propertyMerchantReturnPolicyReturnMethod = Property "returnMethod"

propertyMerchantReturnPolicySeasonalOverrideReturnPolicyCategory ::
  Property MerchantReturnPolicySeasonalOverride '[MerchantReturnEnumeration]
propertyMerchantReturnPolicySeasonalOverrideReturnPolicyCategory =
  Property "returnPolicyCategory"

propertyMerchantReturnPolicyReturnPolicyCategory ::
  Property MerchantReturnPolicy '[MerchantReturnEnumeration]
propertyMerchantReturnPolicyReturnPolicyCategory =
  Property "returnPolicyCategory"

propertyMerchantReturnPolicyReturnPolicyCountry ::
  Property MerchantReturnPolicy '[Country, Text]
propertyMerchantReturnPolicyReturnPolicyCountry =
  Property "returnPolicyCountry"

propertyMerchantReturnPolicyReturnPolicySeasonalOverride ::
  Property MerchantReturnPolicy '[MerchantReturnPolicySeasonalOverride]
propertyMerchantReturnPolicyReturnPolicySeasonalOverride =
  Property "returnPolicySeasonalOverride"

propertyMerchantReturnPolicyReturnShippingFeesAmount ::
  Property MerchantReturnPolicy '[MonetaryAmount]
propertyMerchantReturnPolicyReturnShippingFeesAmount =
  Property "returnShippingFeesAmount"

propertyOrganizationReview :: Property Organization '[Review]
propertyOrganizationReview = Property "review"

propertyOfferReview :: Property Offer '[Review]
propertyOfferReview = Property "review"

propertyBrandReview :: Property Brand '[Review]
propertyBrandReview = Property "review"

propertyEventReview :: Property Event '[Review]
propertyEventReview = Property "review"

propertyPlaceReview :: Property Place '[Review]
propertyPlaceReview = Property "review"

propertyProductReview :: Property Product '[Review]
propertyProductReview = Property "review"

propertyServiceReview :: Property Service '[Review]
propertyServiceReview = Property "review"

propertyCreativeWorkReview :: Property CreativeWork '[Review]
propertyCreativeWorkReview = Property "review"

propertyGuideReviewAspect :: Property Guide '[Text]
propertyGuideReviewAspect = Property "reviewAspect"

propertyReviewReviewAspect :: Property Review '[Text]
propertyReviewReviewAspect = Property "reviewAspect"

propertyRatingReviewAspect :: Property Rating '[Text]
propertyRatingReviewAspect = Property "reviewAspect"

propertyReviewReviewBody :: Property Review '[Text]
propertyReviewReviewBody = Property "reviewBody"

propertyAggregateRatingReviewCount ::
  Property AggregateRating '[Integer]
propertyAggregateRatingReviewCount = Property "reviewCount"

propertyReviewReviewRating :: Property Review '[Rating]
propertyReviewReviewRating = Property "reviewRating"

propertyWebPageReviewedBy ::
  Property WebPage '[Organization, Person]
propertyWebPageReviewedBy = Property "reviewedBy"

propertyProductReviews :: Property Product '[Review]
propertyProductReviews = Property "reviews"

propertyOfferReviews :: Property Offer '[Review]
propertyOfferReviews = Property "reviews"

propertyCreativeWorkReviews :: Property CreativeWork '[Review]
propertyCreativeWorkReviews = Property "reviews"

propertyPlaceReviews :: Property Place '[Review]
propertyPlaceReviews = Property "reviews"

propertyOrganizationReviews :: Property Organization '[Review]
propertyOrganizationReviews = Property "reviews"

propertyMedicalConditionRiskFactor ::
  Property MedicalCondition '[MedicalRiskFactor]
propertyMedicalConditionRiskFactor = Property "riskFactor"

propertyDietRisks :: Property Diet '[Text]
propertyDietRisks = Property "risks"

propertyRoleRoleName :: Property Role '[Text, URL]
propertyRoleRoleName = Property "roleName"

propertyCarRoofLoad :: Property Car '[QuantitativeValue]
propertyCarRoofLoad = Property "roofLoad"

propertyBusOrCoachRoofLoad ::
  Property BusOrCoach '[QuantitativeValue]
propertyBusOrCoachRoofLoad = Property "roofLoad"

propertyRsvpActionRsvpResponse ::
  Property RsvpAction '[RsvpResponseType]
propertyRsvpActionRsvpResponse = Property "rsvpResponse"

propertyLymphaticVesselRunsTo :: Property LymphaticVessel '[Vessel]
propertyLymphaticVesselRunsTo = Property "runsTo"

propertySoftwareSourceCodeRuntime ::
  Property SoftwareSourceCode '[Text]
propertySoftwareSourceCodeRuntime = Property "runtime"

propertySoftwareSourceCodeRuntimePlatform ::
  Property SoftwareSourceCode '[Text]
propertySoftwareSourceCodeRuntimePlatform =
  Property "runtimePlatform"

propertyDrugRxcui :: Property Drug '[Text]
propertyDrugRxcui = Property "rxcui"

propertyDietarySupplementSafetyConsideration ::
  Property DietarySupplement '[Text]
propertyDietarySupplementSafetyConsideration =
  Property "safetyConsideration"

propertyJobPostingSalaryCurrency :: Property JobPosting '[Text]
propertyJobPostingSalaryCurrency = Property "salaryCurrency"

propertyEmployeeRoleSalaryCurrency :: Property EmployeeRole '[Text]
propertyEmployeeRoleSalaryCurrency = Property "salaryCurrency"

propertyEducationalOccupationalProgramSalaryUponCompletion ::
  Property EducationalOccupationalProgram '[MonetaryAmountDistribution]
propertyEducationalOccupationalProgramSalaryUponCompletion =
  Property "salaryUponCompletion"

propertyThingSameAs :: Property Thing '[URL]
propertyThingSameAs = Property "sameAs"

propertySoftwareSourceCodeSampleType ::
  Property SoftwareSourceCode '[Text]
propertySoftwareSourceCodeSampleType = Property "sampleType"

propertyNutritionInformationSaturatedFatContent ::
  Property NutritionInformation '[Mass]
propertyNutritionInformationSaturatedFatContent =
  Property "saturatedFatContent"

propertyScheduleScheduleTimezone :: Property Schedule '[Text]
propertyScheduleScheduleTimezone = Property "scheduleTimezone"

propertyInvoiceScheduledPaymentDate :: Property Invoice '[Date]
propertyInvoiceScheduledPaymentDate =
  Property "scheduledPaymentDate"

propertyPlanActionScheduledTime :: Property PlanAction '[DateTime]
propertyPlanActionScheduledTime = Property "scheduledTime"

propertyCreativeWorkSchemaVersion ::
  Property CreativeWork '[URL, Text]
propertyCreativeWorkSchemaVersion = Property "schemaVersion"

propertySpecialAnnouncementSchoolClosuresInfo ::
  Property SpecialAnnouncement '[WebContent, URL]
propertySpecialAnnouncementSchoolClosuresInfo =
  Property "schoolClosuresInfo"

propertyMovieTheaterScreenCount :: Property MovieTheater '[Number]
propertyMovieTheaterScreenCount = Property "screenCount"

propertySoftwareApplicationScreenshot ::
  Property SoftwareApplication '[URL, ImageObject]
propertySoftwareApplicationScreenshot = Property "screenshot"

propertyCreativeWorkSdDatePublished ::
  Property CreativeWork '[Date]
propertyCreativeWorkSdDatePublished = Property "sdDatePublished"

propertyCreativeWorkSdLicense ::
  Property CreativeWork '[CreativeWork, URL]
propertyCreativeWorkSdLicense = Property "sdLicense"

propertyCreativeWorkSdPublisher ::
  Property CreativeWork '[Organization, Person]
propertyCreativeWorkSdPublisher = Property "sdPublisher"

propertyVideoGameSeriesSeason ::
  Property VideoGameSeries '[URL, CreativeWorkSeason]
propertyVideoGameSeriesSeason = Property "season"

propertyTVSeriesSeason ::
  Property TVSeries '[URL, CreativeWorkSeason]
propertyTVSeriesSeason = Property "season"

propertyRadioSeriesSeason ::
  Property RadioSeries '[URL, CreativeWorkSeason]
propertyRadioSeriesSeason = Property "season"

propertyCreativeWorkSeasonSeasonNumber ::
  Property CreativeWorkSeason '[Text, Integer]
propertyCreativeWorkSeasonSeasonNumber = Property "seasonNumber"

propertyTVSeriesSeasons :: Property TVSeries '[CreativeWorkSeason]
propertyTVSeriesSeasons = Property "seasons"

propertyRadioSeriesSeasons ::
  Property RadioSeries '[CreativeWorkSeason]
propertyRadioSeriesSeasons = Property "seasons"

propertyVideoGameSeriesSeasons ::
  Property VideoGameSeries '[CreativeWorkSeason]
propertyVideoGameSeriesSeasons = Property "seasons"

propertySeatSeatNumber :: Property Seat '[Text]
propertySeatSeatNumber = Property "seatNumber"

propertySeatSeatRow :: Property Seat '[Text]
propertySeatSeatRow = Property "seatRow"

propertySeatSeatSection :: Property Seat '[Text]
propertySeatSeatSection = Property "seatSection"

propertyVehicleSeatingCapacity ::
  Property Vehicle '[Number, QuantitativeValue]
propertyVehicleSeatingCapacity = Property "seatingCapacity"

propertySeatSeatingType :: Property Seat '[Text, QualitativeValue]
propertySeatSeatingType = Property "seatingType"

propertyMedicalConditionSecondaryPrevention ::
  Property MedicalCondition '[MedicalTherapy]
propertyMedicalConditionSecondaryPrevention =
  Property "secondaryPrevention"

propertyJobPostingSecurityClearanceRequirement ::
  Property JobPosting '[Text, URL]
propertyJobPostingSecurityClearanceRequirement =
  Property "securityClearanceRequirement"

propertyFlightReservationSecurityScreening ::
  Property FlightReservation '[Text]
propertyFlightReservationSecurityScreening =
  Property "securityScreening"

propertyOrganizationSeeks :: Property Organization '[Demand]
propertyOrganizationSeeks = Property "seeks"

propertyPersonSeeks :: Property Person '[Demand]
propertyPersonSeeks = Property "seeks"

propertyDemandSeller :: Property Demand '[Organization, Person]
propertyDemandSeller = Property "seller"

propertyOfferSeller :: Property Offer '[Organization, Person]
propertyOfferSeller = Property "seller"

propertyBuyActionSeller ::
  Property BuyAction '[Organization, Person]
propertyBuyActionSeller = Property "seller"

propertyOrderSeller :: Property Order '[Organization, Person]
propertyOrderSeller = Property "seller"

propertyFlightSeller :: Property Flight '[Organization, Person]
propertyFlightSeller = Property "seller"

propertyMessageSender ::
  Property Message '[Audience, Organization, Person]
propertyMessageSender = Property "sender"

propertyReceiveActionSender ::
  Property ReceiveAction '[Audience, Organization, Person]
propertyReceiveActionSender = Property "sender"

propertyJobPostingSensoryRequirement ::
  Property JobPosting '[URL, DefinedTerm, Text]
propertyJobPostingSensoryRequirement =
  Property "sensoryRequirement"

propertyNerveSensoryUnit ::
  Property Nerve '[AnatomicalStructure, SuperficialAnatomy]
propertyNerveSensoryUnit = Property "sensoryUnit"

propertyIndividualProductSerialNumber ::
  Property IndividualProduct '[Text]
propertyIndividualProductSerialNumber = Property "serialNumber"

propertyDemandSerialNumber :: Property Demand '[Text]
propertyDemandSerialNumber = Property "serialNumber"

propertyOfferSerialNumber :: Property Offer '[Text]
propertyOfferSerialNumber = Property "serialNumber"

propertyMedicalTherapySeriousAdverseOutcome ::
  Property MedicalTherapy '[MedicalEntity]
propertyMedicalTherapySeriousAdverseOutcome =
  Property "seriousAdverseOutcome"

propertyMedicalDeviceSeriousAdverseOutcome ::
  Property MedicalDevice '[MedicalEntity]
propertyMedicalDeviceSeriousAdverseOutcome =
  Property "seriousAdverseOutcome"

propertyGameServerServerStatus ::
  Property GameServer '[GameServerStatus]
propertyGameServerServerStatus = Property "serverStatus"

propertyFoodEstablishmentServesCuisine ::
  Property FoodEstablishment '[Text]
propertyFoodEstablishmentServesCuisine = Property "servesCuisine"

propertyServiceServiceArea ::
  Property Service '[AdministrativeArea, Place, GeoShape]
propertyServiceServiceArea = Property "serviceArea"

propertyOrganizationServiceArea ::
  Property Organization '[AdministrativeArea, Place, GeoShape]
propertyOrganizationServiceArea = Property "serviceArea"

propertyContactPointServiceArea ::
  Property ContactPoint '[AdministrativeArea, Place, GeoShape]
propertyContactPointServiceArea = Property "serviceArea"

propertyServiceServiceAudience :: Property Service '[Audience]
propertyServiceServiceAudience = Property "serviceAudience"

propertyServiceChannelServiceLocation ::
  Property ServiceChannel '[Place]
propertyServiceChannelServiceLocation = Property "serviceLocation"

propertyGovernmentServiceServiceOperator ::
  Property GovernmentService '[Organization]
propertyGovernmentServiceServiceOperator =
  Property "serviceOperator"

propertyServiceServiceOutput :: Property Service '[Thing]
propertyServiceServiceOutput = Property "serviceOutput"

propertyServiceChannelServicePhone ::
  Property ServiceChannel '[ContactPoint]
propertyServiceChannelServicePhone = Property "servicePhone"

propertyServiceChannelServicePostalAddress ::
  Property ServiceChannel '[PostalAddress]
propertyServiceChannelServicePostalAddress =
  Property "servicePostalAddress"

propertyServiceChannelServiceSmsNumber ::
  Property ServiceChannel '[ContactPoint]
propertyServiceChannelServiceSmsNumber =
  Property "serviceSmsNumber"

propertyServiceServiceType ::
  Property Service '[Text, GovernmentBenefitsType]
propertyServiceServiceType = Property "serviceType"

propertyServiceChannelServiceUrl :: Property ServiceChannel '[URL]
propertyServiceChannelServiceUrl = Property "serviceUrl"

propertyNutritionInformationServingSize ::
  Property NutritionInformation '[Text]
propertyNutritionInformationServingSize = Property "servingSize"

propertyMediaObjectSha256 :: Property MediaObject '[Text]
propertyMediaObjectSha256 = Property "sha256"

propertySocialMediaPostingSharedContent ::
  Property SocialMediaPosting '[CreativeWork]
propertySocialMediaPostingSharedContent = Property "sharedContent"

propertyShippingRateSettingsShippingDestination ::
  Property ShippingRateSettings '[DefinedRegion]
propertyShippingRateSettingsShippingDestination =
  Property "shippingDestination"

propertyOfferShippingDetailsShippingDestination ::
  Property OfferShippingDetails '[DefinedRegion]
propertyOfferShippingDetailsShippingDestination =
  Property "shippingDestination"

propertyDeliveryTimeSettingsShippingDestination ::
  Property DeliveryTimeSettings '[DefinedRegion]
propertyDeliveryTimeSettingsShippingDestination =
  Property "shippingDestination"

propertyOfferShippingDetails ::
  Property Offer '[OfferShippingDetails]
propertyOfferShippingDetails = Property "shippingDetails"

propertyOfferShippingDetailsShippingLabel ::
  Property OfferShippingDetails '[Text]
propertyOfferShippingDetailsShippingLabel =
  Property "shippingLabel"

propertyShippingRateSettingsShippingLabel ::
  Property ShippingRateSettings '[Text]
propertyShippingRateSettingsShippingLabel =
  Property "shippingLabel"

propertyOfferShippingDetailsShippingOrigin ::
  Property OfferShippingDetails '[DefinedRegion]
propertyOfferShippingDetailsShippingOrigin =
  Property "shippingOrigin"

propertyShippingRateSettingsShippingRate ::
  Property ShippingRateSettings '[MonetaryAmount]
propertyShippingRateSettingsShippingRate = Property "shippingRate"

propertyOfferShippingDetailsShippingRate ::
  Property OfferShippingDetails '[MonetaryAmount]
propertyOfferShippingDetailsShippingRate = Property "shippingRate"

propertyOfferShippingDetailsShippingSettingsLink ::
  Property OfferShippingDetails '[URL]
propertyOfferShippingDetailsShippingSettingsLink =
  Property "shippingSettingsLink"

propertyPersonSibling :: Property Person '[Person]
propertyPersonSibling = Property "sibling"

propertyPersonSiblings :: Property Person '[Person]
propertyPersonSiblings = Property "siblings"

propertyMedicalTestSignDetected ::
  Property MedicalTest '[MedicalSign]
propertyMedicalTestSignDetected = Property "signDetected"

propertyMedicalConditionSignOrSymptom ::
  Property MedicalCondition '[MedicalSignOrSymptom]
propertyMedicalConditionSignOrSymptom = Property "signOrSymptom"

propertySuperficialAnatomySignificance ::
  Property SuperficialAnatomy '[Text]
propertySuperficialAnatomySignificance = Property "significance"

propertyWebPageSignificantLink :: Property WebPage '[URL]
propertyWebPageSignificantLink = Property "significantLink"

propertyWebPageSignificantLinks :: Property WebPage '[URL]
propertyWebPageSignificantLinks = Property "significantLinks"

propertyProductSize ::
  Property
    Product
    '[ DefinedTerm,
       QuantitativeValue,
       Text,
       SizeSpecification
     ]
propertyProductSize = Property "size"

propertyCreativeWorkSize ::
  Property
    CreativeWork
    '[ DefinedTerm,
       QuantitativeValue,
       Text,
       SizeSpecification
     ]
propertyCreativeWorkSize = Property "size"

propertySizeSpecificationSizeGroup ::
  Property SizeSpecification '[SizeGroupEnumeration, Text]
propertySizeSpecificationSizeGroup = Property "sizeGroup"

propertySizeSpecificationSizeSystem ::
  Property SizeSpecification '[Text, SizeSystemEnumeration]
propertySizeSpecificationSizeSystem = Property "sizeSystem"

propertyOccupationSkills ::
  Property Occupation '[Text, DefinedTerm]
propertyOccupationSkills = Property "skills"

propertyJobPostingSkills ::
  Property JobPosting '[Text, DefinedTerm]
propertyJobPostingSkills = Property "skills"

propertyDemandSku :: Property Demand '[Text]
propertyDemandSku = Property "sku"

propertyProductSku :: Property Product '[Text]
propertyProductSku = Property "sku"

propertyOfferSku :: Property Offer '[Text]
propertyOfferSku = Property "sku"

propertyServiceSlogan :: Property Service '[Text]
propertyServiceSlogan = Property "slogan"

propertyOrganizationSlogan :: Property Organization '[Text]
propertyOrganizationSlogan = Property "slogan"

propertyProductSlogan :: Property Product '[Text]
propertyProductSlogan = Property "slogan"

propertyBrandSlogan :: Property Brand '[Text]
propertyBrandSlogan = Property "slogan"

propertyPlaceSlogan :: Property Place '[Text]
propertyPlaceSlogan = Property "slogan"

propertyMolecularEntitySmiles :: Property MolecularEntity '[Text]
propertyMolecularEntitySmiles = Property "smiles"

propertyPlaceSmokingAllowed :: Property Place '[Boolean]
propertyPlaceSmokingAllowed = Property "smokingAllowed"

propertyNutritionInformationSodiumContent ::
  Property NutritionInformation '[Mass]
propertyNutritionInformationSodiumContent =
  Property "sodiumContent"

propertySoftwareApplicationSoftwareAddOn ::
  Property SoftwareApplication '[SoftwareApplication]
propertySoftwareApplicationSoftwareAddOn = Property "softwareAddOn"

propertySoftwareApplicationSoftwareHelp ::
  Property SoftwareApplication '[CreativeWork]
propertySoftwareApplicationSoftwareHelp = Property "softwareHelp"

propertySoftwareApplicationSoftwareRequirements ::
  Property SoftwareApplication '[Text, URL]
propertySoftwareApplicationSoftwareRequirements =
  Property "softwareRequirements"

propertySoftwareApplicationSoftwareVersion ::
  Property SoftwareApplication '[Text]
propertySoftwareApplicationSoftwareVersion =
  Property "softwareVersion"

propertyCreativeWorkSourceOrganization ::
  Property CreativeWork '[Organization]
propertyCreativeWorkSourceOrganization =
  Property "sourceOrganization"

propertyNerveSourcedFrom :: Property Nerve '[BrainStructure]
propertyNerveSourcedFrom = Property "sourcedFrom"

propertyCreativeWorkSpatial :: Property CreativeWork '[Place]
propertyCreativeWorkSpatial = Property "spatial"

propertyCreativeWorkSpatialCoverage ::
  Property CreativeWork '[Place]
propertyCreativeWorkSpatialCoverage = Property "spatialCoverage"

propertyArticleSpeakable ::
  Property Article '[SpeakableSpecification, URL]
propertyArticleSpeakable = Property "speakable"

propertyWebPageSpeakable ::
  Property WebPage '[SpeakableSpecification, URL]
propertyWebPageSpeakable = Property "speakable"

propertyJobPostingSpecialCommitments :: Property JobPosting '[Text]
propertyJobPostingSpecialCommitments =
  Property "specialCommitments"

propertyPlaceSpecialOpeningHoursSpecification ::
  Property Place '[OpeningHoursSpecification]
propertyPlaceSpecialOpeningHoursSpecification =
  Property "specialOpeningHoursSpecification"

propertyWebPageSpecialty :: Property WebPage '[Specialty]
propertyWebPageSpecialty = Property "specialty"

propertyPronounceableTextSpeechToTextMarkup ::
  Property PronounceableText '[Text]
propertyPronounceableTextSpeechToTextMarkup =
  Property "speechToTextMarkup"

propertyVehicleSpeed :: Property Vehicle '[QuantitativeValue]
propertyVehicleSpeed = Property "speed"

propertyQuotationSpokenByCharacter ::
  Property Quotation '[Organization, Person]
propertyQuotationSpokenByCharacter = Property "spokenByCharacter"

propertyEventSponsor :: Property Event '[Organization, Person]
propertyEventSponsor = Property "sponsor"

propertyMedicalStudySponsor ::
  Property MedicalStudy '[Organization, Person]
propertyMedicalStudySponsor = Property "sponsor"

propertyGrantSponsor :: Property Grant '[Organization, Person]
propertyGrantSponsor = Property "sponsor"

propertyCreativeWorkSponsor ::
  Property CreativeWork '[Organization, Person]
propertyCreativeWorkSponsor = Property "sponsor"

propertyOrganizationSponsor ::
  Property Organization '[Organization, Person]
propertyOrganizationSponsor = Property "sponsor"

propertyPersonSponsor :: Property Person '[Organization, Person]
propertyPersonSponsor = Property "sponsor"

propertySportsOrganizationSport ::
  Property SportsOrganization '[Text, URL]
propertySportsOrganizationSport = Property "sport"

propertySportsEventSport :: Property SportsEvent '[Text, URL]
propertySportsEventSport = Property "sport"

propertyExerciseActionSportsActivityLocation ::
  Property ExerciseAction '[SportsActivityLocation]
propertyExerciseActionSportsActivityLocation =
  Property "sportsActivityLocation"

propertyExerciseActionSportsEvent ::
  Property ExerciseAction '[SportsEvent]
propertyExerciseActionSportsEvent = Property "sportsEvent"

propertyExerciseActionSportsTeam ::
  Property ExerciseAction '[SportsTeam]
propertyExerciseActionSportsTeam = Property "sportsTeam"

propertyPersonSpouse :: Property Person '[Person]
propertyPersonSpouse = Property "spouse"

propertyMedicalConditionStage ::
  Property MedicalCondition '[MedicalConditionStage]
propertyMedicalConditionStage = Property "stage"

propertyMedicalConditionStageStageAsNumber ::
  Property MedicalConditionStage '[Number]
propertyMedicalConditionStageStageAsNumber =
  Property "stageAsNumber"

propertyFoodEstablishmentStarRating ::
  Property FoodEstablishment '[Rating]
propertyFoodEstablishmentStarRating = Property "starRating"

propertyLodgingBusinessStarRating ::
  Property LodgingBusiness '[Rating]
propertyLodgingBusinessStarRating = Property "starRating"

propertyMerchantReturnPolicySeasonalOverrideStartDate ::
  Property MerchantReturnPolicySeasonalOverride '[DateTime, Date]
propertyMerchantReturnPolicySeasonalOverrideStartDate =
  Property "startDate"

propertyCreativeWorkSeriesStartDate ::
  Property CreativeWorkSeries '[DateTime, Date]
propertyCreativeWorkSeriesStartDate = Property "startDate"

propertyDatedMoneySpecificationStartDate ::
  Property DatedMoneySpecification '[DateTime, Date]
propertyDatedMoneySpecificationStartDate = Property "startDate"

propertyRoleStartDate :: Property Role '[DateTime, Date]
propertyRoleStartDate = Property "startDate"

propertyScheduleStartDate :: Property Schedule '[DateTime, Date]
propertyScheduleStartDate = Property "startDate"

propertyEventStartDate :: Property Event '[DateTime, Date]
propertyEventStartDate = Property "startDate"

propertyEducationalOccupationalProgramStartDate ::
  Property EducationalOccupationalProgram '[DateTime, Date]
propertyEducationalOccupationalProgramStartDate =
  Property "startDate"

propertyCreativeWorkSeasonStartDate ::
  Property CreativeWorkSeason '[DateTime, Date]
propertyCreativeWorkSeasonStartDate = Property "startDate"

propertyClipStartOffset :: Property Clip '[Number, HyperTocEntry]
propertyClipStartOffset = Property "startOffset"

propertySeekToActionStartOffset ::
  Property SeekToAction '[Number, HyperTocEntry]
propertySeekToActionStartOffset = Property "startOffset"

propertyScheduleStartTime :: Property Schedule '[Time, DateTime]
propertyScheduleStartTime = Property "startTime"

propertyActionStartTime :: Property Action '[Time, DateTime]
propertyActionStartTime = Property "startTime"

propertyMediaObjectStartTime ::
  Property MediaObject '[Time, DateTime]
propertyMediaObjectStartTime = Property "startTime"

propertyInteractionCounterStartTime ::
  Property InteractionCounter '[Time, DateTime]
propertyInteractionCounterStartTime = Property "startTime"

propertyFoodEstablishmentReservationStartTime ::
  Property FoodEstablishmentReservation '[Time, DateTime]
propertyFoodEstablishmentReservationStartTime =
  Property "startTime"

propertyMedicalProcedureStatus ::
  Property
    MedicalProcedure
    '[ MedicalStudyStatus,
       Text,
       EventStatusType
     ]
propertyMedicalProcedureStatus = Property "status"

propertyMedicalConditionStatus ::
  Property
    MedicalCondition
    '[ MedicalStudyStatus,
       Text,
       EventStatusType
     ]
propertyMedicalConditionStatus = Property "status"

propertyMedicalStudyStatus ::
  Property MedicalStudy '[MedicalStudyStatus, Text, EventStatusType]
propertyMedicalStudyStatus = Property "status"

propertyVehicleSteeringPosition ::
  Property Vehicle '[SteeringPositionValue]
propertyVehicleSteeringPosition = Property "steeringPosition"

propertyHowToStep ::
  Property HowTo '[HowToSection, HowToStep, Text, CreativeWork]
propertyHowToStep = Property "step"

propertyPropertyValueSpecificationStepValue ::
  Property PropertyValueSpecification '[Number]
propertyPropertyValueSpecificationStepValue = Property "stepValue"

propertyHowToSectionSteps ::
  Property HowToSection '[Text, ItemList, CreativeWork]
propertyHowToSectionSteps = Property "steps"

propertyHowToSteps ::
  Property HowTo '[Text, ItemList, CreativeWork]
propertyHowToSteps = Property "steps"

propertySoftwareApplicationStorageRequirements ::
  Property SoftwareApplication '[URL, Text]
propertySoftwareApplicationStorageRequirements =
  Property "storageRequirements"

propertyPostalAddressStreetAddress ::
  Property PostalAddress '[Text]
propertyPostalAddressStreetAddress = Property "streetAddress"

propertyDrugStrengthStrengthUnit :: Property DrugStrength '[Text]
propertyDrugStrengthStrengthUnit = Property "strengthUnit"

propertyDrugStrengthStrengthValue ::
  Property DrugStrength '[Number]
propertyDrugStrengthStrengthValue = Property "strengthValue"

propertyJointStructuralClass :: Property Joint '[Text]
propertyJointStructuralClass = Property "structuralClass"

propertyMedicalEntityStudy ::
  Property MedicalEntity '[MedicalStudy]
propertyMedicalEntityStudy = Property "study"

propertyMedicalObservationalStudyStudyDesign ::
  Property MedicalObservationalStudy '[MedicalObservationalStudyDesign]
propertyMedicalObservationalStudyStudyDesign =
  Property "studyDesign"

propertyMedicalStudyStudyLocation ::
  Property MedicalStudy '[AdministrativeArea]
propertyMedicalStudyStudyLocation = Property "studyLocation"

propertyMedicalStudyStudySubject ::
  Property MedicalStudy '[MedicalEntity]
propertyMedicalStudyStudySubject = Property "studySubject"

propertyEventSubEvent :: Property Event '[Event]
propertyEventSubEvent = Property "subEvent"

propertyEventSubEvents :: Property Event '[Event]
propertyEventSubEvents = Property "subEvents"

propertyOrganizationSubOrganization ::
  Property Organization '[Organization]
propertyOrganizationSubOrganization = Property "subOrganization"

propertyReservationPackageSubReservation ::
  Property ReservationPackage '[Reservation]
propertyReservationPackageSubReservation =
  Property "subReservation"

propertyMedicalConditionStageSubStageSuffix ::
  Property MedicalConditionStage '[Text]
propertyMedicalConditionStageSubStageSuffix =
  Property "subStageSuffix"

propertyAnatomicalStructureSubStructure ::
  Property AnatomicalStructure '[AnatomicalStructure]
propertyAnatomicalStructureSubStructure = Property "subStructure"

propertyMedicalTestPanelSubTest ::
  Property MedicalTestPanel '[MedicalTest]
propertyMedicalTestPanelSubTest = Property "subTest"

propertyTripSubTrip :: Property Trip '[Trip]
propertyTripSubTrip = Property "subTrip"

propertyThingSubjectOf :: Property Thing '[Event, CreativeWork]
propertyThingSubjectOf = Property "subjectOf"

propertyScreeningEventSubtitleLanguage ::
  Property ScreeningEvent '[Language, Text]
propertyScreeningEventSubtitleLanguage =
  Property "subtitleLanguage"

propertyMovieSubtitleLanguage :: Property Movie '[Language, Text]
propertyMovieSubtitleLanguage = Property "subtitleLanguage"

propertyBroadcastEventSubtitleLanguage ::
  Property BroadcastEvent '[Language, Text]
propertyBroadcastEventSubtitleLanguage =
  Property "subtitleLanguage"

propertyTVEpisodeSubtitleLanguage ::
  Property TVEpisode '[Language, Text]
propertyTVEpisodeSubtitleLanguage = Property "subtitleLanguage"

propertyProductModelSuccessorOf ::
  Property ProductModel '[ProductModel]
propertyProductModelSuccessorOf = Property "successorOf"

propertyNutritionInformationSugarContent ::
  Property NutritionInformation '[Mass]
propertyNutritionInformationSugarContent = Property "sugarContent"

propertySizeSpecificationSuggestedAge ::
  Property SizeSpecification '[QuantitativeValue]
propertySizeSpecificationSuggestedAge = Property "suggestedAge"

propertyPeopleAudienceSuggestedAge ::
  Property PeopleAudience '[QuantitativeValue]
propertyPeopleAudienceSuggestedAge = Property "suggestedAge"

propertyQuestionSuggestedAnswer ::
  Property Question '[Answer, ItemList]
propertyQuestionSuggestedAnswer = Property "suggestedAnswer"

propertySizeSpecificationSuggestedGender ::
  Property SizeSpecification '[GenderType, Text]
propertySizeSpecificationSuggestedGender =
  Property "suggestedGender"

propertyPeopleAudienceSuggestedGender ::
  Property PeopleAudience '[GenderType, Text]
propertyPeopleAudienceSuggestedGender = Property "suggestedGender"

propertyPeopleAudienceSuggestedMaxAge ::
  Property PeopleAudience '[Number]
propertyPeopleAudienceSuggestedMaxAge = Property "suggestedMaxAge"

propertySizeSpecificationSuggestedMeasurement ::
  Property SizeSpecification '[QuantitativeValue]
propertySizeSpecificationSuggestedMeasurement =
  Property "suggestedMeasurement"

propertyPeopleAudienceSuggestedMeasurement ::
  Property PeopleAudience '[QuantitativeValue]
propertyPeopleAudienceSuggestedMeasurement =
  Property "suggestedMeasurement"

propertyPeopleAudienceSuggestedMinAge ::
  Property PeopleAudience '[Number]
propertyPeopleAudienceSuggestedMinAge = Property "suggestedMinAge"

propertyRecipeSuitableForDiet :: Property Recipe '[RestrictedDiet]
propertyRecipeSuitableForDiet = Property "suitableForDiet"

propertyMenuItemSuitableForDiet ::
  Property MenuItem '[RestrictedDiet]
propertyMenuItemSuitableForDiet = Property "suitableForDiet"

propertyEventSuperEvent :: Property Event '[Event]
propertyEventSuperEvent = Property "superEvent"

propertyHowToSupply :: Property HowTo '[Text, HowToSupply]
propertyHowToSupply = Property "supply"

propertyHowToDirectionSupply ::
  Property HowToDirection '[Text, HowToSupply]
propertyHowToDirectionSupply = Property "supply"

propertyArterySupplyTo :: Property Artery '[AnatomicalStructure]
propertyArterySupplyTo = Property "supplyTo"

propertySoftwareApplicationSupportingData ::
  Property SoftwareApplication '[DataFeed]
propertySoftwareApplicationSupportingData =
  Property "supportingData"

propertyVisualArtworkSurface :: Property VisualArtwork '[Text, URL]
propertyVisualArtworkSurface = Property "surface"

propertyActionTarget :: Property Action '[URL, EntryPoint]
propertyActionTarget = Property "target"

propertyUpdateActionTargetCollection ::
  Property UpdateAction '[Thing]
propertyUpdateActionTargetCollection = Property "targetCollection"

propertyAlignmentObjectTargetDescription ::
  Property AlignmentObject '[Text]
propertyAlignmentObjectTargetDescription =
  Property "targetDescription"

propertyAlignmentObjectTargetName ::
  Property AlignmentObject '[Text]
propertyAlignmentObjectTargetName = Property "targetName"

propertyAPIReferenceTargetPlatform :: Property APIReference '[Text]
propertyAPIReferenceTargetPlatform = Property "targetPlatform"

propertyDoseScheduleTargetPopulation ::
  Property DoseSchedule '[Text]
propertyDoseScheduleTargetPopulation = Property "targetPopulation"

propertyDietarySupplementTargetPopulation ::
  Property DietarySupplement '[Text]
propertyDietarySupplementTargetPopulation =
  Property "targetPopulation"

propertySoftwareSourceCodeTargetProduct ::
  Property SoftwareSourceCode '[SoftwareApplication]
propertySoftwareSourceCodeTargetProduct = Property "targetProduct"

propertyAlignmentObjectTargetUrl :: Property AlignmentObject '[URL]
propertyAlignmentObjectTargetUrl = Property "targetUrl"

propertyOrganizationTaxID :: Property Organization '[Text]
propertyOrganizationTaxID = Property "taxID"

propertyPersonTaxID :: Property Person '[Text]
propertyPersonTaxID = Property "taxID"

propertyTaxonTaxonRank ::
  Property Taxon '[URL, PropertyValue, Text]
propertyTaxonTaxonRank = Property "taxonRank"

propertyBioChemEntityTaxonomicRange ::
  Property BioChemEntity '[URL, DefinedTerm, Text, Taxon]
propertyBioChemEntityTaxonomicRange = Property "taxonomicRange"

propertyCreativeWorkTeaches ::
  Property CreativeWork '[DefinedTerm, Text]
propertyCreativeWorkTeaches = Property "teaches"

propertyEducationEventTeaches ::
  Property EducationEvent '[DefinedTerm, Text]
propertyEducationEventTeaches = Property "teaches"

propertyLearningResourceTeaches ::
  Property LearningResource '[DefinedTerm, Text]
propertyLearningResourceTeaches = Property "teaches"

propertyPlaceTelephone :: Property Place '[Text]
propertyPlaceTelephone = Property "telephone"

propertyOrganizationTelephone :: Property Organization '[Text]
propertyOrganizationTelephone = Property "telephone"

propertyContactPointTelephone :: Property ContactPoint '[Text]
propertyContactPointTelephone = Property "telephone"

propertyPersonTelephone :: Property Person '[Text]
propertyPersonTelephone = Property "telephone"

propertyCreativeWorkTemporal ::
  Property CreativeWork '[DateTime, Text]
propertyCreativeWorkTemporal = Property "temporal"

propertyCreativeWorkTemporalCoverage ::
  Property CreativeWork '[URL, Text, DateTime]
propertyCreativeWorkTemporalCoverage = Property "temporalCoverage"

propertyDefinedTermTermCode :: Property DefinedTerm '[Text]
propertyDefinedTermTermCode = Property "termCode"

propertyEducationalOccupationalProgramTermDuration ::
  Property EducationalOccupationalProgram '[Duration]
propertyEducationalOccupationalProgramTermDuration =
  Property "termDuration"

propertyServiceTermsOfService :: Property Service '[URL, Text]
propertyServiceTermsOfService = Property "termsOfService"

propertyEducationalOccupationalProgramTermsPerYear ::
  Property EducationalOccupationalProgram '[Number]
propertyEducationalOccupationalProgramTermsPerYear =
  Property "termsPerYear"

propertyCreativeWorkText :: Property CreativeWork '[Text]
propertyCreativeWorkText = Property "text"

propertyPronounceableTextTextValue ::
  Property PronounceableText '[Text]
propertyPronounceableTextTextValue = Property "textValue"

propertyImageObjectThumbnail :: Property ImageObject '[ImageObject]
propertyImageObjectThumbnail = Property "thumbnail"

propertyVideoObjectThumbnail :: Property VideoObject '[ImageObject]
propertyVideoObjectThumbnail = Property "thumbnail"

propertyCreativeWorkThumbnailUrl :: Property CreativeWork '[URL]
propertyCreativeWorkThumbnailUrl = Property "thumbnailUrl"

propertyCorporationTickerSymbol :: Property Corporation '[Text]
propertyCorporationTickerSymbol = Property "tickerSymbol"

propertyTicketTicketNumber :: Property Ticket '[Text]
propertyTicketTicketNumber = Property "ticketNumber"

propertyTicketTicketToken :: Property Ticket '[URL, Text]
propertyTicketTicketToken = Property "ticketToken"

propertyTicketTicketedSeat :: Property Ticket '[Seat]
propertyTicketTicketedSeat = Property "ticketedSeat"

propertyEducationalOccupationalProgramTimeOfDay ::
  Property EducationalOccupationalProgram '[Text]
propertyEducationalOccupationalProgramTimeOfDay =
  Property "timeOfDay"

propertyCreativeWorkTimeRequired ::
  Property CreativeWork '[Duration]
propertyCreativeWorkTimeRequired = Property "timeRequired"

propertyEducationalOccupationalProgramTimeToComplete ::
  Property EducationalOccupationalProgram '[Duration]
propertyEducationalOccupationalProgramTimeToComplete =
  Property "timeToComplete"

propertyPathologyTestTissueSample :: Property PathologyTest '[Text]
propertyPathologyTestTissueSample = Property "tissueSample"

propertyJobPostingTitle :: Property JobPosting '[Text]
propertyJobPostingTitle = Property "title"

propertyTVEpisodeTitleEIDR :: Property TVEpisode '[URL, Text]
propertyTVEpisodeTitleEIDR = Property "titleEIDR"

propertyMovieTitleEIDR :: Property Movie '[URL, Text]
propertyMovieTitleEIDR = Property "titleEIDR"

propertyTransferActionToLocation ::
  Property TransferAction '[Place]
propertyTransferActionToLocation = Property "toLocation"

propertyMoveActionToLocation :: Property MoveAction '[Place]
propertyMoveActionToLocation = Property "toLocation"

propertyInsertActionToLocation :: Property InsertAction '[Place]
propertyInsertActionToLocation = Property "toLocation"

propertyExerciseActionToLocation ::
  Property ExerciseAction '[Place]
propertyExerciseActionToLocation = Property "toLocation"

propertyMessageToRecipient ::
  Property Message '[Person, Organization, ContactPoint, Audience]
propertyMessageToRecipient = Property "toRecipient"

propertyHyperTocEntryTocContinuation ::
  Property HyperTocEntry '[HyperTocEntry]
propertyHyperTocEntryTocContinuation = Property "tocContinuation"

propertyHyperTocTocEntry :: Property HyperToc '[HyperTocEntry]
propertyHyperTocTocEntry = Property "tocEntry"

propertyVehicleTongueWeight ::
  Property Vehicle '[QuantitativeValue]
propertyVehicleTongueWeight = Property "tongueWeight"

propertyHowToDirectionTool ::
  Property HowToDirection '[HowToTool, Text]
propertyHowToDirectionTool = Property "tool"

propertyHowToTool :: Property HowTo '[HowToTool, Text]
propertyHowToTool = Property "tool"

propertyEngineSpecificationTorque ::
  Property EngineSpecification '[QuantitativeValue]
propertyEngineSpecificationTorque = Property "torque"

propertyJobPostingTotalJobOpenings ::
  Property JobPosting '[Integer]
propertyJobPostingTotalJobOpenings = Property "totalJobOpenings"

propertyInvoiceTotalPaymentDue ::
  Property Invoice '[MonetaryAmount, PriceSpecification]
propertyInvoiceTotalPaymentDue = Property "totalPaymentDue"

propertyReservationTotalPrice ::
  Property Reservation '[Number, PriceSpecification, Text]
propertyReservationTotalPrice = Property "totalPrice"

propertyTicketTotalPrice ::
  Property Ticket '[Number, PriceSpecification, Text]
propertyTicketTotalPrice = Property "totalPrice"

propertyHowToDirectionTotalTime ::
  Property HowToDirection '[Duration]
propertyHowToDirectionTotalTime = Property "totalTime"

propertyHowToTotalTime :: Property HowTo '[Duration]
propertyHowToTotalTime = Property "totalTime"

propertyPlaceTourBookingPage :: Property Place '[URL]
propertyPlaceTourBookingPage = Property "tourBookingPage"

propertyAccommodationTourBookingPage ::
  Property Accommodation '[URL]
propertyAccommodationTourBookingPage = Property "tourBookingPage"

propertyApartmentComplexTourBookingPage ::
  Property ApartmentComplex '[URL]
propertyApartmentComplexTourBookingPage =
  Property "tourBookingPage"

propertyTouristDestinationTouristType ::
  Property TouristDestination '[Text, Audience]
propertyTouristDestinationTouristType = Property "touristType"

propertyTouristAttractionTouristType ::
  Property TouristAttraction '[Text, Audience]
propertyTouristAttractionTouristType = Property "touristType"

propertyTouristTripTouristType ::
  Property TouristTrip '[Text, Audience]
propertyTouristTripTouristType = Property "touristType"

propertyMusicPlaylistTrack ::
  Property MusicPlaylist '[ItemList, MusicRecording]
propertyMusicPlaylistTrack = Property "track"

propertyMusicGroupTrack ::
  Property MusicGroup '[ItemList, MusicRecording]
propertyMusicGroupTrack = Property "track"

propertyParcelDeliveryTrackingNumber ::
  Property ParcelDelivery '[Text]
propertyParcelDeliveryTrackingNumber = Property "trackingNumber"

propertyParcelDeliveryTrackingUrl :: Property ParcelDelivery '[URL]
propertyParcelDeliveryTrackingUrl = Property "trackingUrl"

propertyMusicPlaylistTracks ::
  Property MusicPlaylist '[MusicRecording]
propertyMusicPlaylistTracks = Property "tracks"

propertyMusicGroupTracks :: Property MusicGroup '[MusicRecording]
propertyMusicGroupTracks = Property "tracks"

propertyVideoGameSeriesTrailer ::
  Property VideoGameSeries '[VideoObject]
propertyVideoGameSeriesTrailer = Property "trailer"

propertyTVSeriesTrailer :: Property TVSeries '[VideoObject]
propertyTVSeriesTrailer = Property "trailer"

propertyCreativeWorkSeasonTrailer ::
  Property CreativeWorkSeason '[VideoObject]
propertyCreativeWorkSeasonTrailer = Property "trailer"

propertyMovieTrailer :: Property Movie '[VideoObject]
propertyMovieTrailer = Property "trailer"

propertyMovieSeriesTrailer :: Property MovieSeries '[VideoObject]
propertyMovieSeriesTrailer = Property "trailer"

propertyRadioSeriesTrailer :: Property RadioSeries '[VideoObject]
propertyRadioSeriesTrailer = Property "trailer"

propertyEpisodeTrailer :: Property Episode '[VideoObject]
propertyEpisodeTrailer = Property "trailer"

propertyVideoGameTrailer :: Property VideoGame '[VideoObject]
propertyVideoGameTrailer = Property "trailer"

propertyVehicleTrailerWeight ::
  Property Vehicle '[QuantitativeValue]
propertyVehicleTrailerWeight = Property "trailerWeight"

propertyTrainTripTrainName :: Property TrainTrip '[Text]
propertyTrainTripTrainName = Property "trainName"

propertyTrainTripTrainNumber :: Property TrainTrip '[Text]
propertyTrainTripTrainNumber = Property "trainNumber"

propertyWorkBasedProgramTrainingSalary ::
  Property WorkBasedProgram '[MonetaryAmountDistribution]
propertyWorkBasedProgramTrainingSalary = Property "trainingSalary"

propertyEducationalOccupationalProgramTrainingSalary ::
  Property EducationalOccupationalProgram '[MonetaryAmountDistribution]
propertyEducationalOccupationalProgramTrainingSalary =
  Property "trainingSalary"

propertyNutritionInformationTransFatContent ::
  Property NutritionInformation '[Mass]
propertyNutritionInformationTransFatContent =
  Property "transFatContent"

propertyVideoObjectTranscript :: Property VideoObject '[Text]
propertyVideoObjectTranscript = Property "transcript"

propertyAudioObjectTranscript :: Property AudioObject '[Text]
propertyAudioObjectTranscript = Property "transcript"

propertyShippingDeliveryTimeTransitTime ::
  Property ShippingDeliveryTime '[QuantitativeValue]
propertyShippingDeliveryTimeTransitTime = Property "transitTime"

propertyOfferShippingDetailsTransitTimeLabel ::
  Property OfferShippingDetails '[Text]
propertyOfferShippingDetailsTransitTimeLabel =
  Property "transitTimeLabel"

propertyDeliveryTimeSettingsTransitTimeLabel ::
  Property DeliveryTimeSettings '[Text]
propertyDeliveryTimeSettingsTransitTimeLabel =
  Property "transitTimeLabel"

propertyCreativeWorkTranslationOfWork ::
  Property CreativeWork '[CreativeWork]
propertyCreativeWorkTranslationOfWork =
  Property "translationOfWork"

propertyCreativeWorkTranslator ::
  Property CreativeWork '[Organization, Person]
propertyCreativeWorkTranslator = Property "translator"

propertyEventTranslator :: Property Event '[Organization, Person]
propertyEventTranslator = Property "translator"

propertyInfectiousDiseaseTransmissionMethod ::
  Property InfectiousDisease '[Text]
propertyInfectiousDiseaseTransmissionMethod =
  Property "transmissionMethod"

propertySpecialAnnouncementTravelBans ::
  Property SpecialAnnouncement '[URL, WebContent]
propertySpecialAnnouncementTravelBans = Property "travelBans"

propertyMedicalTrialTrialDesign ::
  Property MedicalTrial '[MedicalTrialDesign]
propertyMedicalTrialTrialDesign = Property "trialDesign"

propertyVeinTributary :: Property Vein '[AnatomicalStructure]
propertyVeinTributary = Property "tributary"

propertyBedDetailsTypeOfBed :: Property BedDetails '[BedType, Text]
propertyBedDetailsTypeOfBed = Property "typeOfBed"

propertyTypeAndQuantityNodeTypeOfGood ::
  Property TypeAndQuantityNode '[Product, Service]
propertyTypeAndQuantityNodeTypeOfGood = Property "typeOfGood"

propertyOwnershipInfoTypeOfGood ::
  Property OwnershipInfo '[Product, Service]
propertyOwnershipInfoTypeOfGood = Property "typeOfGood"

propertyCreativeWorkTypicalAgeRange ::
  Property CreativeWork '[Text]
propertyCreativeWorkTypicalAgeRange = Property "typicalAgeRange"

propertyEventTypicalAgeRange :: Property Event '[Text]
propertyEventTypicalAgeRange = Property "typicalAgeRange"

propertyEducationalOccupationalProgramTypicalCreditsPerTerm ::
  Property EducationalOccupationalProgram '[Integer, StructuredValue]
propertyEducationalOccupationalProgramTypicalCreditsPerTerm =
  Property "typicalCreditsPerTerm"

propertyMedicalConditionTypicalTest ::
  Property MedicalCondition '[MedicalTest]
propertyMedicalConditionTypicalTest = Property "typicalTest"

propertyTicketUnderName :: Property Ticket '[Organization, Person]
propertyTicketUnderName = Property "underName"

propertyReservationUnderName ::
  Property Reservation '[Organization, Person]
propertyReservationUnderName = Property "underName"

propertyPropertyValueUnitCode ::
  Property PropertyValue '[Text, URL]
propertyPropertyValueUnitCode = Property "unitCode"

propertyQuantitativeValueUnitCode ::
  Property QuantitativeValue '[Text, URL]
propertyQuantitativeValueUnitCode = Property "unitCode"

propertyTypeAndQuantityNodeUnitCode ::
  Property TypeAndQuantityNode '[Text, URL]
propertyTypeAndQuantityNodeUnitCode = Property "unitCode"

propertyUnitPriceSpecificationUnitCode ::
  Property UnitPriceSpecification '[Text, URL]
propertyUnitPriceSpecificationUnitCode = Property "unitCode"

propertyQuantitativeValueUnitText ::
  Property QuantitativeValue '[Text]
propertyQuantitativeValueUnitText = Property "unitText"

propertyUnitPriceSpecificationUnitText ::
  Property UnitPriceSpecification '[Text]
propertyUnitPriceSpecificationUnitText = Property "unitText"

propertyPropertyValueUnitText :: Property PropertyValue '[Text]
propertyPropertyValueUnitText = Property "unitText"

propertyTypeAndQuantityNodeUnitText ::
  Property TypeAndQuantityNode '[Text]
propertyTypeAndQuantityNodeUnitText = Property "unitText"

propertyNewsMediaOrganizationUnnamedSourcesPolicy ::
  Property NewsMediaOrganization '[CreativeWork, URL]
propertyNewsMediaOrganizationUnnamedSourcesPolicy =
  Property "unnamedSourcesPolicy"

propertyOrganizationUnnamedSourcesPolicy ::
  Property Organization '[CreativeWork, URL]
propertyOrganizationUnnamedSourcesPolicy =
  Property "unnamedSourcesPolicy"

propertyNutritionInformationUnsaturatedFatContent ::
  Property NutritionInformation '[Mass]
propertyNutritionInformationUnsaturatedFatContent =
  Property "unsaturatedFatContent"

propertyMediaObjectUploadDate :: Property MediaObject '[Date]
propertyMediaObjectUploadDate = Property "uploadDate"

propertyCommentUpvoteCount :: Property Comment '[Integer]
propertyCommentUpvoteCount = Property "upvoteCount"

propertyThingUrl :: Property Thing '[URL]
propertyThingUrl = Property "url"

propertyEntryPointUrlTemplate :: Property EntryPoint '[Text]
propertyEntryPointUrlTemplate = Property "urlTemplate"

propertyCreativeWorkUsageInfo ::
  Property CreativeWork '[URL, CreativeWork]
propertyCreativeWorkUsageInfo = Property "usageInfo"

propertyMedicalTestUsedToDiagnose ::
  Property MedicalTest '[MedicalCondition]
propertyMedicalTestUsedToDiagnose = Property "usedToDiagnose"

propertyInteractionCounterUserInteractionCount ::
  Property InteractionCounter '[Integer]
propertyInteractionCounterUserInteractionCount =
  Property "userInteractionCount"

propertyMedicalTestUsesDevice ::
  Property MedicalTest '[MedicalDevice]
propertyMedicalTestUsesDevice = Property "usesDevice"

propertyHealthInsurancePlanUsesHealthPlanIdStandard ::
  Property HealthInsurancePlan '[URL, Text]
propertyHealthInsurancePlanUsesHealthPlanIdStandard =
  Property "usesHealthPlanIdStandard"

propertyHyperTocEntryUtterances :: Property HyperTocEntry '[Text]
propertyHyperTocEntryUtterances = Property "utterances"

propertyEducationalOccupationalCredentialValidFor ::
  Property EducationalOccupationalCredential '[Duration]
propertyEducationalOccupationalCredentialValidFor =
  Property "validFor"

propertyPermitValidFor :: Property Permit '[Duration]
propertyPermitValidFor = Property "validFor"

propertyDemandValidFrom :: Property Demand '[Date, DateTime]
propertyDemandValidFrom = Property "validFrom"

propertyMonetaryAmountValidFrom ::
  Property MonetaryAmount '[Date, DateTime]
propertyMonetaryAmountValidFrom = Property "validFrom"

propertyLocationFeatureSpecificationValidFrom ::
  Property LocationFeatureSpecification '[Date, DateTime]
propertyLocationFeatureSpecificationValidFrom =
  Property "validFrom"

propertyPriceSpecificationValidFrom ::
  Property PriceSpecification '[Date, DateTime]
propertyPriceSpecificationValidFrom = Property "validFrom"

propertyOpeningHoursSpecificationValidFrom ::
  Property OpeningHoursSpecification '[Date, DateTime]
propertyOpeningHoursSpecificationValidFrom = Property "validFrom"

propertyPermitValidFrom :: Property Permit '[Date, DateTime]
propertyPermitValidFrom = Property "validFrom"

propertyOfferValidFrom :: Property Offer '[Date, DateTime]
propertyOfferValidFrom = Property "validFrom"

propertyEducationalOccupationalCredentialValidIn ::
  Property EducationalOccupationalCredential '[AdministrativeArea]
propertyEducationalOccupationalCredentialValidIn =
  Property "validIn"

propertyPermitValidIn :: Property Permit '[AdministrativeArea]
propertyPermitValidIn = Property "validIn"

propertyDemandValidThrough :: Property Demand '[Date, DateTime]
propertyDemandValidThrough = Property "validThrough"

propertyMonetaryAmountValidThrough ::
  Property MonetaryAmount '[Date, DateTime]
propertyMonetaryAmountValidThrough = Property "validThrough"

propertyOfferValidThrough :: Property Offer '[Date, DateTime]
propertyOfferValidThrough = Property "validThrough"

propertyPriceSpecificationValidThrough ::
  Property PriceSpecification '[Date, DateTime]
propertyPriceSpecificationValidThrough = Property "validThrough"

propertyJobPostingValidThrough ::
  Property JobPosting '[Date, DateTime]
propertyJobPostingValidThrough = Property "validThrough"

propertyLocationFeatureSpecificationValidThrough ::
  Property LocationFeatureSpecification '[Date, DateTime]
propertyLocationFeatureSpecificationValidThrough =
  Property "validThrough"

propertyOpeningHoursSpecificationValidThrough ::
  Property OpeningHoursSpecification '[Date, DateTime]
propertyOpeningHoursSpecificationValidThrough =
  Property "validThrough"

propertyPermitValidUntil :: Property Permit '[Date]
propertyPermitValidUntil = Property "validUntil"

propertyPropertyValueValue ::
  Property PropertyValue '[StructuredValue, Text, Boolean, Number]
propertyPropertyValueValue = Property "value"

propertyQuantitativeValueValue ::
  Property
    QuantitativeValue
    '[ StructuredValue,
       Text,
       Boolean,
       Number
     ]
propertyQuantitativeValueValue = Property "value"

propertyMonetaryAmountValue ::
  Property MonetaryAmount '[StructuredValue, Text, Boolean, Number]
propertyMonetaryAmountValue = Property "value"

propertyPriceSpecificationValueAddedTaxIncluded ::
  Property PriceSpecification '[Boolean]
propertyPriceSpecificationValueAddedTaxIncluded =
  Property "valueAddedTaxIncluded"

propertyPropertyValueSpecificationValueMaxLength ::
  Property PropertyValueSpecification '[Number]
propertyPropertyValueSpecificationValueMaxLength =
  Property "valueMaxLength"

propertyPropertyValueSpecificationValueMinLength ::
  Property PropertyValueSpecification '[Number]
propertyPropertyValueSpecificationValueMinLength =
  Property "valueMinLength"

propertyPropertyValueSpecificationValueName ::
  Property PropertyValueSpecification '[Text]
propertyPropertyValueSpecificationValueName = Property "valueName"

propertyPropertyValueSpecificationValuePattern ::
  Property PropertyValueSpecification '[Text]
propertyPropertyValueSpecificationValuePattern =
  Property "valuePattern"

propertyQualitativeValueValueReference ::
  Property
    QualitativeValue
    '[ Enumeration,
       QualitativeValue,
       DefinedTerm,
       StructuredValue,
       PropertyValue,
       Text,
       MeasurementTypeEnumeration,
       QuantitativeValue
     ]
propertyQualitativeValueValueReference = Property "valueReference"

propertyPropertyValueValueReference ::
  Property
    PropertyValue
    '[ Enumeration,
       QualitativeValue,
       DefinedTerm,
       StructuredValue,
       PropertyValue,
       Text,
       MeasurementTypeEnumeration,
       QuantitativeValue
     ]
propertyPropertyValueValueReference = Property "valueReference"

propertyQuantitativeValueValueReference ::
  Property
    QuantitativeValue
    '[ Enumeration,
       QualitativeValue,
       DefinedTerm,
       StructuredValue,
       PropertyValue,
       Text,
       MeasurementTypeEnumeration,
       QuantitativeValue
     ]
propertyQuantitativeValueValueReference = Property "valueReference"

propertyPropertyValueSpecificationValueRequired ::
  Property PropertyValueSpecification '[Boolean]
propertyPropertyValueSpecificationValueRequired =
  Property "valueRequired"

propertyDatasetVariableMeasured ::
  Property Dataset '[Text, PropertyValue]
propertyDatasetVariableMeasured = Property "variableMeasured"

propertyComicIssueVariantCover :: Property ComicIssue '[Text]
propertyComicIssueVariantCover = Property "variantCover"

propertyProductGroupVariesBy ::
  Property ProductGroup '[Text, DefinedTerm]
propertyProductGroupVariesBy = Property "variesBy"

propertyOrganizationVatID :: Property Organization '[Text]
propertyOrganizationVatID = Property "vatID"

propertyPersonVatID :: Property Person '[Text]
propertyPersonVatID = Property "vatID"

propertyVehicleVehicleConfiguration :: Property Vehicle '[Text]
propertyVehicleVehicleConfiguration =
  Property "vehicleConfiguration"

propertyVehicleVehicleEngine ::
  Property Vehicle '[EngineSpecification]
propertyVehicleVehicleEngine = Property "vehicleEngine"

propertyVehicleVehicleIdentificationNumber ::
  Property Vehicle '[Text]
propertyVehicleVehicleIdentificationNumber =
  Property "vehicleIdentificationNumber"

propertyVehicleVehicleInteriorColor :: Property Vehicle '[Text]
propertyVehicleVehicleInteriorColor =
  Property "vehicleInteriorColor"

propertyVehicleVehicleInteriorType :: Property Vehicle '[Text]
propertyVehicleVehicleInteriorType = Property "vehicleInteriorType"

propertyVehicleVehicleModelDate :: Property Vehicle '[Date]
propertyVehicleVehicleModelDate = Property "vehicleModelDate"

propertyVehicleVehicleSeatingCapacity ::
  Property Vehicle '[Number, QuantitativeValue]
propertyVehicleVehicleSeatingCapacity =
  Property "vehicleSeatingCapacity"

propertyVehicleVehicleSpecialUsage ::
  Property Vehicle '[CarUsageType, Text]
propertyVehicleVehicleSpecialUsage = Property "vehicleSpecialUsage"

propertyVehicleVehicleTransmission ::
  Property Vehicle '[Text, URL, QualitativeValue]
propertyVehicleVehicleTransmission = Property "vehicleTransmission"

propertyBuyActionVendor ::
  Property BuyAction '[Organization, Person]
propertyBuyActionVendor = Property "vendor"

propertyNewsMediaOrganizationVerificationFactCheckingPolicy ::
  Property NewsMediaOrganization '[CreativeWork, URL]
propertyNewsMediaOrganizationVerificationFactCheckingPolicy =
  Property "verificationFactCheckingPolicy"

propertyCreativeWorkVersion ::
  Property CreativeWork '[Number, Text]
propertyCreativeWorkVersion = Property "version"

propertyCreativeWorkVideo ::
  Property CreativeWork '[VideoObject, Clip]
propertyCreativeWorkVideo = Property "video"

propertyBroadcastEventVideoFormat ::
  Property BroadcastEvent '[Text]
propertyBroadcastEventVideoFormat = Property "videoFormat"

propertyBroadcastServiceVideoFormat ::
  Property BroadcastService '[Text]
propertyBroadcastServiceVideoFormat = Property "videoFormat"

propertyScreeningEventVideoFormat ::
  Property ScreeningEvent '[Text]
propertyScreeningEventVideoFormat = Property "videoFormat"

propertyVideoObjectVideoFrameSize :: Property VideoObject '[Text]
propertyVideoObjectVideoFrameSize = Property "videoFrameSize"

propertyVideoObjectVideoQuality :: Property VideoObject '[Text]
propertyVideoObjectVideoQuality = Property "videoQuality"

propertyPublicationVolumeVolumeNumber ::
  Property PublicationVolume '[Text, Integer]
propertyPublicationVolumeVolumeNumber = Property "volumeNumber"

propertyDrugWarning :: Property Drug '[Text, URL]
propertyDrugWarning = Property "warning"

propertyDemandWarranty :: Property Demand '[WarrantyPromise]
propertyDemandWarranty = Property "warranty"

propertyOfferWarranty :: Property Offer '[WarrantyPromise]
propertyOfferWarranty = Property "warranty"

propertySellActionWarrantyPromise ::
  Property SellAction '[WarrantyPromise]
propertySellActionWarrantyPromise = Property "warrantyPromise"

propertyBuyActionWarrantyPromise ::
  Property BuyAction '[WarrantyPromise]
propertyBuyActionWarrantyPromise = Property "warrantyPromise"

propertyWarrantyPromiseWarrantyScope ::
  Property WarrantyPromise '[WarrantyScope]
propertyWarrantyPromiseWarrantyScope = Property "warrantyScope"

propertyFlightWebCheckinTime :: Property Flight '[DateTime]
propertyFlightWebCheckinTime = Property "webCheckinTime"

propertySpecialAnnouncementWebFeed ::
  Property SpecialAnnouncement '[DataFeed, URL]
propertySpecialAnnouncementWebFeed = Property "webFeed"

propertyPodcastSeriesWebFeed ::
  Property PodcastSeries '[DataFeed, URL]
propertyPodcastSeriesWebFeed = Property "webFeed"

propertyPersonWeight :: Property Person '[QuantitativeValue]
propertyPersonWeight = Property "weight"

propertyProductWeight :: Property Product '[QuantitativeValue]
propertyProductWeight = Property "weight"

propertyOfferShippingDetailsWeight ::
  Property OfferShippingDetails '[QuantitativeValue]
propertyOfferShippingDetailsWeight = Property "weight"

propertyVehicleWeightTotal :: Property Vehicle '[QuantitativeValue]
propertyVehicleWeightTotal = Property "weightTotal"

propertyVehicleWheelbase :: Property Vehicle '[QuantitativeValue]
propertyVehicleWheelbase = Property "wheelbase"

propertyVisualArtworkWidth ::
  Property VisualArtwork '[Distance, QuantitativeValue]
propertyVisualArtworkWidth = Property "width"

propertyProductWidth ::
  Property Product '[Distance, QuantitativeValue]
propertyProductWidth = Property "width"

propertyMediaObjectWidth ::
  Property MediaObject '[Distance, QuantitativeValue]
propertyMediaObjectWidth = Property "width"

propertyOfferShippingDetailsWidth ::
  Property OfferShippingDetails '[Distance, QuantitativeValue]
propertyOfferShippingDetailsWidth = Property "width"

propertyLoseActionWinner :: Property LoseAction '[Person]
propertyLoseActionWinner = Property "winner"

propertyArticleWordCount :: Property Article '[Integer]
propertyArticleWordCount = Property "wordCount"

propertyCreativeWorkWorkExample ::
  Property CreativeWork '[CreativeWork]
propertyCreativeWorkWorkExample = Property "workExample"

propertyEventWorkFeatured :: Property Event '[CreativeWork]
propertyEventWorkFeatured = Property "workFeatured"

propertyJobPostingWorkHours :: Property JobPosting '[Text]
propertyJobPostingWorkHours = Property "workHours"

propertyPersonWorkLocation ::
  Property Person '[ContactPoint, Place]
propertyPersonWorkLocation = Property "workLocation"

propertyEventWorkPerformed :: Property Event '[CreativeWork]
propertyEventWorkPerformed = Property "workPerformed"

propertyScreeningEventWorkPresented ::
  Property ScreeningEvent '[Movie]
propertyScreeningEventWorkPresented = Property "workPresented"

propertyCreativeWorkWorkTranslation ::
  Property CreativeWork '[CreativeWork]
propertyCreativeWorkWorkTranslation = Property "workTranslation"

propertyExercisePlanWorkload ::
  Property ExercisePlan '[Energy, QuantitativeValue]
propertyExercisePlanWorkload = Property "workload"

propertyPersonWorksFor :: Property Person '[Organization]
propertyPersonWorksFor = Property "worksFor"

propertyRatingWorstRating :: Property Rating '[Text, Number]
propertyRatingWorstRating = Property "worstRating"

propertySpeakableSpecificationXpath ::
  Property SpeakableSpecification '[XPathType]
propertySpeakableSpecificationXpath = Property "xpath"

propertyWebPageElementXpath :: Property WebPageElement '[XPathType]
propertyWebPageElementXpath = Property "xpath"

propertyAccommodationYearBuilt :: Property Accommodation '[Number]
propertyAccommodationYearBuilt = Property "yearBuilt"

propertyBusinessAudienceYearlyRevenue ::
  Property BusinessAudience '[QuantitativeValue]
propertyBusinessAudienceYearlyRevenue = Property "yearlyRevenue"

propertyBusinessAudienceYearsInOperation ::
  Property BusinessAudience '[QuantitativeValue]
propertyBusinessAudienceYearsInOperation =
  Property "yearsInOperation"

propertyHowToYield :: Property HowTo '[QuantitativeValue, Text]
propertyHowToYield = Property "yield"
