{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TypeFamilies          #-}

module Ballast.Types
  ( Username(..)
  , Password(..)
  , GetRate(..)
  , RateResponse(..)
  , RateOptions(..)
  , Currency(..)
  , GroupBy(..)
  , WarehouseArea(..)
  , RateOrder(..)
  , Items
  , ItemInfo(..)
  , ShipTo(..)
  , SKU(..)
  , Quantity(..)
  , AddressLine(..)
  , City(..)
  , PostalCode(..)
  , Region(..)
  , Country(..)
  , CanSplit(..)
  , IsCommercial(..)
  , IsPoBox(..)
  , Rates(..)
  , ServiceOptions(..)
  , RateResource(..)
  , ShipwireRequest(..)
  , RateRequest
  , StockRequest
  , mkShipwireRequest
  , ShipwireReturn
  , defaultGetRate
  , Reply
  , Method
  , Host
  , ShipwireConfig(..)
  , Params(..)
  , TupleBS8
  , filterQuery
  , filterBody
  , (-&-)
  , ShipwireHost(..)
  , hostUri
  , credentialsEnv
  , prodEnvConfig
  , sandboxEnvConfig
  , ParentId(..)
  , ProductIdParam(..)
  , ProductExternalIdParam(..)
  , WarehouseIdParam(..)
  , WarehouseExternalIdParam(..)
  , WarehouseRegionParam(..)
  , WarehouseAreaParam(..)
  , ChannelName(..)
  , IncludeEmpty(..)
  , VendorIdParam(..)
  , VendorExternalIdParam(..)
  , DisableAutoBreakLots(..)
  , Mode(..)
  , IncludeEmptyShipwireAnywhere(..)
  , Offset(..)
  , Total(..)
  , Previous(..)
  , Next(..)
  , Limit(..)
  , GetReceivingsRequest
  , CreateReceivingRequest
  , CreateReceiving(..)
  , ExpectedDateText(..)
  , ReceivingOptions(..)
  , WarehouseRegion(..)
  , ReceivingArrangement(..)
  , ArrangementType(..)
  , ReceivingShipments(..)
  , ReceivingShipment(..)
  , Type(..)
  , ReceivingItems(..)
  , ReceivingItem(..)
  , ReceivingShipFrom(..)
  , Name(..)
  , State(..)
  , Phone(..)
  , ExpandReceivings(..)
  , ExpandParamReceivings(..)
  , CommerceNameParam(..)
  , TransactionIdParam(..)
  , OrderIdParam(..)
  , OrderNoParam(..)
  , StatusParams(..)
  , StatusParam(..)
  , UpdatedAfter(..)
  ) where

import           Control.Applicative
import           Data.Aeson
import           Data.Aeson.Types
import           Data.ByteString            (ByteString)
import qualified Data.ByteString.Char8      as BS8
import qualified Data.ByteString.Lazy.Char8 as BSL
import qualified Data.Char                  as DC
import           Data.Fixed
import           Data.Monoid                ((<>))
import           Data.Text                  (Text)
import qualified Data.Text                  as T
import qualified Data.Text.Encoding         as TE
import           Data.Time.Clock            (UTCTime)
import           GHC.Generics
import           Network.HTTP.Client
import qualified Network.HTTP.Types.Method  as NHTM
import           System.Environment         (getEnv)

-- | Username type used for HTTP Basic authentication.
newtype Username = Username
  { unUsername :: BS8.ByteString
  } deriving (Read, Show, Eq)

-- | Password type used for HTTP Basic authentication.
newtype Password = Password
  { unPassword :: BS8.ByteString
  } deriving (Read, Show, Eq)

---------------------------------------------------------------------
-- Rate Endpoint -- https://www.shipwire.com/w/developers/rate/
---------------------------------------------------------------------

newtype SKU = SKU
  { unSku :: Text
  } deriving (Eq, Generic, Show)

mkSku :: Text -> Maybe SKU
mkSku sku
  | T.length sku > 16 = Nothing
  | T.length sku < 1 = Nothing
  | otherwise = Just (SKU sku)

instance ToJSON SKU where
  toJSON sku = genericToJSON options sku
    where
      options =
        defaultOptions
        { unwrapUnaryRecords = True
        }

instance FromJSON SKU where
  parseJSON s = genericParseJSON options s
    where
      options =
        defaultOptions
        { unwrapUnaryRecords = True
        }

-- max 16 characters
-- haskellbookskuty
data GetRate = GetRate
  { rateOptions :: RateOptions
  , rateOrder   :: RateOrder
  } deriving (Eq, Generic, Show)

instance ToJSON GetRate where
  toJSON rate = genericToJSON options rate
    where
      options =
        defaultOptions
        { fieldLabelModifier = downcaseHead . drop 4
        , omitNothingFields = True
        }

data RateOptions = RateOptions
  { rateOptionCurrency      :: Currency
  , rateOptionGroupBy       :: GroupBy
  , rateOptionCanSplit      :: CanSplit
  , rateOptionWarehouseArea :: WarehouseArea
  , rateOptionChannelName   :: Maybe ChannelName
  } deriving (Eq, Generic, Show)

newtype CanSplit = CanSplit
  { unCanSplit :: Integer
  } deriving (Eq, Generic, Show)

instance ToJSON CanSplit where
  toJSON cs = genericToJSON options cs
    where
      options =
        defaultOptions
        { unwrapUnaryRecords = True
        }

data RateOrder = RateOrder
  { rateOrderShipTo :: ShipTo
  , rateOrderItems  :: Items
  } deriving (Eq, Generic, Show)

instance ToJSON RateOrder where
  toJSON order = genericToJSON options order
    where
      options =
        defaultOptions
        { fieldLabelModifier = downcaseHead . drop 9
        , omitNothingFields = True
        }

data ShipTo = ShipTo
  { shipToAddress1     :: AddressLine
  , shipToAddress2     :: AddressLine
  , shipToAddress3     :: AddressLine
  , shipToCity         :: City
  , shipToPostalCode   :: PostalCode
  , shipToRegion       :: Region
  , shipToCountry      :: Country
  , shipToIsCommercial :: IsCommercial
  , shipToIsPoBox      :: IsPoBox
  } deriving (Eq, Generic, Show)

data IsCommercial
  = Commercial
  | NotCommercial
  deriving (Eq, Generic, Show)

instance ToJSON IsCommercial where
  toJSON Commercial = Number 1
  toJSON NotCommercial = Number 0

data IsPoBox
  = PoBox
  | NotPoBox
  deriving (Eq, Generic, Show)

instance ToJSON IsPoBox where
  toJSON PoBox = Number 1
  toJSON NotPoBox = Number 0

type Items = [ItemInfo]

data ItemInfo =
  ItemInfo (SKU, Quantity)
  deriving (Eq, Show)

instance ToJSON ItemInfo where
  toJSON (ItemInfo (sku, q)) = object ["sku" .= sku, "quantity" .= q]

newtype Quantity = Quantity
  { unQuantity :: Integer
  } deriving (Eq, Generic, Show)

instance ToJSON Quantity where
  toJSON q = genericToJSON options q
    where
      options =
        defaultOptions
        { unwrapUnaryRecords = True
        }

instance FromJSON Quantity where
  parseJSON q = genericParseJSON options q
    where
      options =
        defaultOptions
        { unwrapUnaryRecords = True
        }

instance ToJSON ShipTo where
  toJSON shipto = genericToJSON options shipto
    where
      options =
        defaultOptions
        { fieldLabelModifier = downcaseHead . drop 6
        , omitNothingFields = True
        }

defaultGetRate :: GetRate
defaultGetRate = GetRate defaultRateOptions defaultRateOrder

defaultRateOrder :: RateOrder
defaultRateOrder = RateOrder defaultShipTo defaultItems

defaultItems :: Items
defaultItems = [ItemInfo ((SKU "Ballasttest"), Quantity 1)]

defaultShipTo :: ShipTo
defaultShipTo =
  ShipTo
    (AddressLine "6501 Railroad Avenue SE")
    (AddressLine "Room 315")
    (AddressLine "")
    (City "Snoqualmie")
    (PostalCode "85283")
    (Region "WA")
    (Country "US")
    Commercial
    NotPoBox

newtype AddressLine = AddressLine
  { unAddressLine :: Text
  } deriving (Eq, Generic, Show)

instance FromJSON AddressLine where
  parseJSON al = genericParseJSON options al
    where
      options =
        defaultOptions
        { fieldLabelModifier = downcaseHead . drop 2
        , unwrapUnaryRecords = True
        }

instance ToJSON AddressLine where
  toJSON address = genericToJSON options address
    where
      options =
        defaultOptions
        { unwrapUnaryRecords = True
        }

newtype City = City
  { unCity :: Text
  } deriving (Eq, Generic, Show)

instance FromJSON City where
  parseJSON c = genericParseJSON options c
    where
      options =
        defaultOptions
        { fieldLabelModifier = downcaseHead . drop 2
        , unwrapUnaryRecords = True
        }

instance ToJSON City where
  toJSON city = genericToJSON options city
    where
      options =
        defaultOptions
        { unwrapUnaryRecords = True
        }

newtype PostalCode = PostalCode
  { unPostalCode :: Text
  } deriving (Eq, Generic, Show)

instance FromJSON PostalCode where
  parseJSON pc = genericParseJSON options pc
    where
      options =
        defaultOptions
        { fieldLabelModifier = downcaseHead . drop 2
        , unwrapUnaryRecords = True
        }

instance ToJSON PostalCode where
  toJSON code = genericToJSON options code
    where
      options =
        defaultOptions
        { unwrapUnaryRecords = True
        }

newtype Region = Region
  { unRegion :: Text
  } deriving (Eq, Generic, Show)

instance ToJSON Region where
  toJSON region = genericToJSON options region
    where
      options =
        defaultOptions
        { unwrapUnaryRecords = True
        }

newtype Country = Country
  { unCountry :: Text
  } deriving (Eq, Generic, Show)

instance FromJSON Country where
  parseJSON c = genericParseJSON options c
    where
      options =
        defaultOptions
        { fieldLabelModifier = downcaseHead . drop 2
        , unwrapUnaryRecords = True
        }

instance ToJSON Country where
  toJSON country = genericToJSON options country
    where
      options =
        defaultOptions
        { unwrapUnaryRecords = True
        }

downcaseHead :: [Char] -> [Char]
downcaseHead [] = []
downcaseHead (x:xs) = (DC.toLower x) : xs

instance ToJSON RateOptions where
  toJSON ro = genericToJSON options ro
    where
      options =
        defaultOptions
        { fieldLabelModifier = downcaseHead . drop 10
        , omitNothingFields = True
        }

defaultRateOptions :: RateOptions
defaultRateOptions = RateOptions USD GroupByAll (CanSplit 1) WarehouseAreaUS Nothing

data Currency =
  USD
  deriving (Eq, Show)

instance ToJSON Currency where
  toJSON USD = String "USD"

tshow
  :: Show a
  => a -> Text
tshow = T.pack . show

omitNulls :: [(Text, Value)] -> Value
omitNulls = object . filter notNull
  where
    notNull (_, Null) = False
    -- notNull (_, Array a) = (not . V.null) a
    notNull _ = True

data GroupBy
  = GroupByAll
  | GroupByWarehouse
  deriving (Eq, Generic, Show)

instance ToJSON GroupBy where
  toJSON GroupByAll = String "all"
  toJSON GroupByWarehouse = String "warehouse"

instance FromJSON GroupBy where
  parseJSON = withText "groupBy" parse
    where
      parse "all" = pure GroupByAll
      parse "warehouse" = pure GroupByWarehouse
      parse o = fail ("Unexpected groupBy value: " <> show o)

data WarehouseArea =
  WarehouseAreaUS
  deriving (Eq, Show)

instance ToJSON WarehouseArea where
  toJSON WarehouseAreaUS = String "US"

defaultRateResponse :: IO RateResponse
defaultRateResponse = do
  file <- BSL.readFile "test.json"
  let decoded = eitherDecode file
  case decoded of
    Right info -> return info
    Left err -> error err

data RateResponse = RateResponse
  { rateResponseStatus           :: ResponseStatus
  , rateResponseMessage          :: ResponseMessage
  , rateResponseWarnings         :: Maybe ResponseWarnings
  , rateResponseErrors           :: Maybe ResponseErrors
  , rateResponseResourceLocation :: Maybe ResponseResourceLocation
  , rateResponseResource         :: Maybe RateResource
  } deriving (Eq, Generic, Show)

newtype ResponseStatus = ResponseStatus
  { unResponseStatus :: Integer
  } deriving (Eq, Generic, Show)

instance FromJSON ResponseStatus where
  parseJSON rs = genericParseJSON options rs
    where
      options =
        defaultOptions
        { unwrapUnaryRecords = True
        , fieldLabelModifier = downcaseHead . drop 10
        }

newtype ResponseMessage = ResponseMessage
  { unResponseMessage :: Text
  } deriving (Eq, Generic, Show)

instance FromJSON ResponseMessage where
  parseJSON rm = genericParseJSON options rm
    where
      options =
        defaultOptions
        { unwrapUnaryRecords = True
        }

newtype ResponseWarnings = ResponseWarnings
  { unResponseWarnings :: [Warning]
  } deriving (Eq, Generic, Show)

instance FromJSON ResponseWarnings where
  parseJSON rw = genericParseJSON options rw
    where
      options =
        defaultOptions
        { unwrapUnaryRecords = True
        }

newtype ResponseErrors = ResponseErrors
  { unResponseErrors :: [Error]
  } deriving (Eq, Generic, Show)

instance FromJSON ResponseErrors where
  parseJSON re = genericParseJSON options re
    where
      options =
        defaultOptions
        { unwrapUnaryRecords = True
        }

newtype ResponseResourceLocation = ResponseResourceLocation
  { unResponseResourceLocation :: Text
  } deriving (Eq, Generic, Show)

instance FromJSON ResponseResourceLocation where
  parseJSON rrl = genericParseJSON options rrl
    where
      options =
        defaultOptions
        { unwrapUnaryRecords = True
        , fieldLabelModifier = downcaseHead . drop 10
        }

instance FromJSON RateResponse where
  parseJSON rr = genericParseJSON options rr
    where
      options =
        defaultOptions
        { fieldLabelModifier = downcaseHead . drop 12
        , omitNothingFields  = True
        }

data Warning = Warning
  { warningCode    :: WarningCode
  , warningMessage :: WarningMessage
  , warningType    :: WarningType
  } deriving (Eq, Generic, Show)

newtype WarningCode = WarningCode
  { unWarningCode :: Text
  } deriving (Eq, Generic, Show)

instance FromJSON WarningCode where
  parseJSON wc = genericParseJSON options wc
    where
      options =
        defaultOptions
        { unwrapUnaryRecords = True
        }

newtype WarningMessage = WarningMessage
  { unWarningMessage :: Text
  } deriving (Eq, Generic, Show)

instance FromJSON WarningMessage where
  parseJSON wm = genericParseJSON options wm
    where
      options =
        defaultOptions
        { unwrapUnaryRecords = True
        }

data WarningType
  = WarningWarning
  | WarningError
  deriving (Eq, Generic, Show)

instance FromJSON WarningType where
  parseJSON = withText "warningType" parse
    where
      parse "warning" = pure WarningWarning
      parse "error" = pure WarningError
      parse o = fail ("Unexpected warningType value: " <> show o)

instance FromJSON Warning where
  parseJSON w = genericParseJSON options w
    where
      options =
        defaultOptions
        { fieldLabelModifier = downcaseHead . drop 7
        }

data Error = Error
  { errorCode    :: ErrorCode
  , errorMessage :: ErrorMessage
  , errorType    :: ErrorType
  } deriving (Eq, Generic, Show)

newtype ErrorCode = ErrorCode
  { unErrorCode :: Text
  } deriving (Eq, Generic, Show)

instance FromJSON ErrorCode where
  parseJSON ec = genericParseJSON options ec
    where
      options =
        defaultOptions
        { unwrapUnaryRecords = True
        }

newtype ErrorMessage = ErrorMessage
  { unErrorMessage :: Text
  } deriving (Eq, Generic, Show)

instance FromJSON ErrorMessage where
  parseJSON em = genericParseJSON options em
    where
      options =
        defaultOptions
        { unwrapUnaryRecords = True
        }

data ErrorType
  = ErrorWarning
  | ErrorError
  deriving (Eq, Generic, Show)

instance FromJSON ErrorType where
  parseJSON = withText "errorType" parse
    where
      parse "warning" = pure ErrorWarning
      parse "error" = pure ErrorError
      parse o = fail ("Unexpected errorType value: " <> show o)

instance FromJSON Error where
  parseJSON w = genericParseJSON options w
    where
      options =
        defaultOptions
        { fieldLabelModifier = downcaseHead . drop 5
        }
data RateResource = RateResource
  { resourceGroupBy :: GroupBy
  , resourceRates   :: Rates
  } deriving (Eq, Generic, Show)

instance FromJSON RateResource where
  parseJSON res = genericParseJSON options res
    where
      options =
        defaultOptions
        { fieldLabelModifier = downcaseHead . drop 8
        }

newtype Rates = Rates
  { rateServiceOptions :: [ServiceOptions]
  } deriving (Eq, Generic, Show)

instance FromJSON Rates where
  parseJSON rates = genericParseJSON options rates
    where
      options =
        defaultOptions
        { fieldLabelModifier = downcaseHead . drop 4
        , unwrapUnaryRecords = True
        }

data ServiceOptions = ServiceOptions
  { sOptsServiceOptions  :: [ServiceOption]
  , sOptsGroupId         :: Maybe GroupId
  , sOptsGroupExternalId :: Maybe GroupExternalId
  } deriving (Eq, Generic, Show)

type GroupId = Id

type GroupExternalId = ExternalId

newtype Id = Id
  { unId :: Integer
  } deriving (Eq, Generic, Show)

instance ToJSON Id where
  toJSON i = genericToJSON options i
    where
      options =
        defaultOptions
        { unwrapUnaryRecords = True
        }

instance FromJSON Id where
  parseJSON id = genericParseJSON options id
    where
      options =
        defaultOptions
        { unwrapUnaryRecords = True
        }

newtype ExternalId = ExternalId
  { unExternalId :: Text
  } deriving (Eq, Generic, Show)

instance ToJSON ExternalId where
  toJSON e = genericToJSON options e
    where
      options =
        defaultOptions
        { unwrapUnaryRecords = True
        }

instance FromJSON ExternalId where
  parseJSON eid = genericParseJSON options eid
    where
      options =
        defaultOptions
        { unwrapUnaryRecords = True
        }

instance FromJSON ServiceOptions where
  parseJSON sop = genericParseJSON options sop
    where
      options =
        defaultOptions
        { fieldLabelModifier = downcaseHead . drop 5
        , omitNothingFields  = True
        }

data ServiceOption = ServiceOption
  { sOptServiceLevelCode :: ServiceLevelCode
  , sOptServiceLevelName :: ServiceOptionServiceLevelName
  , sOptShipments        :: [Shipment]
  } deriving (Eq, Generic, Show)

newtype ServiceOptionServiceLevelName = ServiceOptionServiceLevelName
  { unServiceOptionServiceLevelName :: Text
  } deriving (Eq, Generic, Show)

instance FromJSON ServiceOptionServiceLevelName where
  parseJSON sln = genericParseJSON options sln
    where
      options =
        defaultOptions
        { unwrapUnaryRecords = True
        }

data ServiceLevelCode
  = DomesticGround
  | DomesticTwoDay
  | DomesticOneDay
  | InternationalEconomy
  | InternationalStandard
  | InternationalPlus
  | InternationalPremium
  deriving (Eq, Generic, Show)

instance FromJSON ServiceLevelCode where
  parseJSON = withText "serviceLevelCode" parse
    where
      parse "GD"      = pure DomesticGround
      parse "2D"      = pure DomesticTwoDay
      parse "1D"      = pure DomesticOneDay
      parse "E-INTL"  = pure InternationalEconomy
      parse "INTL"    = pure InternationalStandard
      parse "PL-INTL" = pure InternationalPlus
      parse "PM-INTL" = pure InternationalPremium
      parse o = fail ("Unexpected serviceLevelCode value: " <> show o)

instance FromJSON ServiceOption where
  parseJSON sopt = genericParseJSON options sopt
    where
      options =
        defaultOptions
        { fieldLabelModifier = downcaseHead . drop 4
        }

data Shipment = Shipment
  { shipmentWarehouseName           :: WarehouseName
  , shipmentCarrier                 :: Carrier
  , shipmentCost                    :: Cost
  , shipmentSubtotals               :: [Subtotal]
  , shipmentPieces                  :: [Piece]
  , shipmentExpectedShipDate        :: Maybe ExpectedShipDate
  , shipmentExpectedDeliveryDateMin :: Maybe ExpectedDeliveryDateMin
  , shipmentExpectedDeliveryDateMax :: Maybe ExpectedDeliveryDateMax
  } deriving (Eq, Generic, Show)

newtype WarehouseName = WarehouseName
  { unWarehouseName :: Text
  } deriving (Eq, Generic, Show)

instance FromJSON WarehouseName where
  parseJSON wn = genericParseJSON options wn
    where
      options =
        defaultOptions
        { unwrapUnaryRecords = True
        }

newtype ExpectedShipDate = ExpectedShipDate
  { unExpectedShipDate :: UTCTime
  } deriving (Eq, Generic, Show)

instance FromJSON ExpectedShipDate where
  parseJSON esd = genericParseJSON options esd
    where
      options =
        defaultOptions
        { unwrapUnaryRecords = True
        }

type ExpectedDeliveryDateMin = ExpectedShipDate

type ExpectedDeliveryDateMax = ExpectedShipDate

instance FromJSON Shipment where
  parseJSON sh = genericParseJSON options sh
    where
      options =
        defaultOptions
        { fieldLabelModifier = downcaseHead . drop 8
        , omitNothingFields  = True
        }

data Carrier = Carrier
  { carrierCode       :: CarrierCode
  , carrierName       :: CarrierName
  , carrierProperties :: CarrierProperties
  } deriving (Eq, Generic, Show)

newtype CarrierCode = CarrierCode
  { unCarrierCode :: Text
  } deriving (Eq, Generic, Show)

instance FromJSON CarrierCode where
  parseJSON cc = genericParseJSON options cc
    where
      options =
        defaultOptions
        { unwrapUnaryRecords = True
        }        

newtype CarrierName = CarrierName
  { unCarrierName :: Text
  } deriving (Eq, Generic, Show)

instance FromJSON CarrierName where
  parseJSON cn = genericParseJSON options cn
    where
      options =
        defaultOptions
        { unwrapUnaryRecords = True
        }

instance ToJSON CarrierName where
  toJSON c = genericToJSON options c
    where
      options =
        defaultOptions
        { unwrapUnaryRecords = True
        }
        
newtype CarrierProperties = CarrierProperties
  { unCarrierProperties :: [Text]
  } deriving (Eq, Generic, Show)

instance FromJSON CarrierProperties where
  parseJSON cp = genericParseJSON options cp
    where
      options =
        defaultOptions
        { unwrapUnaryRecords = True
        }

instance FromJSON Carrier where
  parseJSON car = genericParseJSON options car
    where
      options =
        defaultOptions
        { fieldLabelModifier = downcaseHead . drop 7
        }

data Cost = Cost
  { costCurrency         :: CostCurrency
  , costType             :: CostType
  , costName             :: CostName
  , costAmount           :: CostAmount
  , costConverted        :: CostConverted
  , costOriginalCost     :: CostOriginalCost
  , costOriginalCurrency :: CostOriginalCurrency
  } deriving (Eq, Generic, Show)

newtype CostCurrency = CostCurrency
  { unCostCurrency :: Text
  } deriving (Eq, Generic, Show)

instance FromJSON CostCurrency where
  parseJSON cc = genericParseJSON options cc
    where
      options =
        defaultOptions
        { unwrapUnaryRecords = True
        }

newtype CostType = CostType
  { unCostType :: Text
  } deriving (Eq, Generic, Show)

instance FromJSON CostType where
  parseJSON ct = genericParseJSON options ct
    where
      options =
        defaultOptions
        { unwrapUnaryRecords = True
        }

newtype CostName = CostName
  { unCostName :: Text
  } deriving (Eq, Generic, Show)

instance FromJSON CostName where
  parseJSON cn = genericParseJSON options cn
    where
      options =
        defaultOptions
        { unwrapUnaryRecords = True
        }

newtype CostAmount = CostAmount
  { unCostAmount :: Centi
  } deriving (Eq, Generic, Show)

instance FromJSON CostAmount where
  parseJSON ca = genericParseJSON options ca
    where
      options =
        defaultOptions
        { unwrapUnaryRecords = True
        }

newtype CostConverted = CostConverted
  { unCostConverted :: Bool
  } deriving (Eq, Generic, Show)

instance FromJSON CostConverted where
  parseJSON cc = genericParseJSON options cc
    where
      options =
        defaultOptions
        { unwrapUnaryRecords = True
        }

newtype CostOriginalCost = CostOriginalCost
  { unCostOriginalCost :: Centi
  } deriving (Eq, Generic, Show)

instance FromJSON CostOriginalCost where
  parseJSON cc = genericParseJSON options cc
    where
      options =
        defaultOptions
        { unwrapUnaryRecords = True
        }

newtype CostOriginalCurrency = CostOriginalCurrency
  { unCostOriginalCurrency :: Text
  } deriving (Eq, Generic, Show)

instance FromJSON CostOriginalCurrency where
  parseJSON cc = genericParseJSON options cc
    where
      options =
        defaultOptions
        { unwrapUnaryRecords = True
        }

instance FromJSON Cost where
  parseJSON cost = genericParseJSON options cost
    where
      options =
        defaultOptions
        { fieldLabelModifier = downcaseHead . drop 4
        }

data Subtotal = Subtotal
  { subtotalCurrency         :: SubtotalCurrency
  , sbutotalType             :: SubtotalType
  , subtotalName             :: SubtotalName
  , subtotalAmount           :: SubtotalAmount
  , subtotalConverted        :: SubtotalConverted
  , subtotalOriginalCost     :: SubtotalOriginalCost
  , subtotalOriginalCurrency :: SubtotalOriginalCurrency
  } deriving (Eq, Generic, Show)

newtype SubtotalCurrency = SubtotalCurrency
  { unSubtotalCurrency :: Text
  } deriving (Eq, Generic, Show)

instance FromJSON SubtotalCurrency where
  parseJSON sc = genericParseJSON options sc
    where
      options =
        defaultOptions
        { unwrapUnaryRecords = True
        }

newtype SubtotalType = SubtotalType
  { unSubtotalType :: Text
  } deriving (Eq, Generic, Show)

instance FromJSON SubtotalType where
  parseJSON st = genericParseJSON options st
    where
      options =
        defaultOptions
        { unwrapUnaryRecords = True
        }

newtype SubtotalName = SubtotalName
  { unSubtotalName :: Text
  } deriving (Eq, Generic, Show)

instance FromJSON SubtotalName where
  parseJSON sn = genericParseJSON options sn
    where
      options =
        defaultOptions
        { unwrapUnaryRecords = True
        }

newtype SubtotalAmount = SubtotalAmount
  { unSubtotalAmount :: Centi
  } deriving (Eq, Generic, Show)

instance FromJSON SubtotalAmount where
  parseJSON sa = genericParseJSON options sa
    where
      options =
        defaultOptions
        { unwrapUnaryRecords = True
        }

newtype SubtotalConverted = SubtotalConverted
  { unSubtotalConverted :: Bool
  } deriving (Eq, Generic, Show)

instance FromJSON SubtotalConverted where
  parseJSON sc = genericParseJSON options sc
    where
      options =
        defaultOptions
        { unwrapUnaryRecords = True
        }

newtype SubtotalOriginalCost = SubtotalOriginalCost
  { unSubtotalOriginalCost :: Centi
  } deriving (Eq, Generic, Show)

instance FromJSON SubtotalOriginalCost where
  parseJSON soc = genericParseJSON options soc
    where
      options =
        defaultOptions
        { unwrapUnaryRecords = True
        }

newtype SubtotalOriginalCurrency = SubtotalOriginalCurrency
  { unSubtotalOriginalCurrency :: Text
  } deriving (Eq, Generic, Show)

instance FromJSON SubtotalOriginalCurrency where
  parseJSON soc = genericParseJSON options soc
    where
      options =
        defaultOptions
        { unwrapUnaryRecords = True
        }

instance FromJSON Subtotal where
  parseJSON sub = genericParseJSON options sub
    where
      options =
        defaultOptions
        { fieldLabelModifier = downcaseHead . drop 8
        }

data Piece = Piece
  { pieceLength     :: PieceLength
  , pieceWidth      :: PieceWidth
  , pieceHeight     :: PieceHeight
  , pieceWeight     :: PieceWeight
  , pieceSubweights :: [PieceSubWeight]
  , pieceContents   :: [PieceContent]
  } deriving (Eq, Generic, Show)

instance FromJSON Piece where
  parseJSON piece = genericParseJSON options piece
    where
      options =
        defaultOptions
        { fieldLabelModifier = downcaseHead . drop 5
        }

data PieceLength = PieceLength
  { pieceLengthAmount :: Length
  , pieceLengthUnits  :: PieceLengthUnits
  } deriving (Eq, Generic, Show)

newtype Length = Length
  { unLength :: Double
  } deriving (Eq, Generic, Show)

instance ToJSON Length where
  toJSON l = genericToJSON options l
    where
      options =
        defaultOptions
        { unwrapUnaryRecords = True
        }

instance FromJSON Length where
  parseJSON l = genericParseJSON options l
    where
      options =
        defaultOptions
        { unwrapUnaryRecords = True
        }

newtype PieceLengthUnits = PieceLengthUnits
  { unPieceLengthUnits :: Text
  } deriving (Eq, Generic, Show)

instance FromJSON PieceLengthUnits where
  parseJSON plu = genericParseJSON options plu
    where
      options =
        defaultOptions
        { unwrapUnaryRecords = True
        }

instance FromJSON PieceLength where
  parseJSON pl = genericParseJSON options pl
    where
      options =
        defaultOptions
        { fieldLabelModifier = downcaseHead . drop 11
        }

data PieceWidth = PieceWidth
  { pieceWidthAmount :: Width
  , pieceWidthUnits  :: PieceWidthUnits
  } deriving (Eq, Generic, Show)

newtype Width = Width
  { unWidth :: Double
  } deriving (Eq, Generic, Show)

instance ToJSON Width where
  toJSON w = genericToJSON options w
    where
      options =
        defaultOptions
        { unwrapUnaryRecords = True
        }

instance FromJSON Width where
  parseJSON w = genericParseJSON options w
    where
      options =
        defaultOptions
        { unwrapUnaryRecords = True
        }

newtype PieceWidthUnits = PieceWidthUnits
  { unPieceWidthUnits :: Text
  } deriving (Eq, Generic, Show)

instance FromJSON PieceWidthUnits where
  parseJSON pwu = genericParseJSON options pwu
    where
      options =
        defaultOptions
        { unwrapUnaryRecords = True
        }

instance FromJSON PieceWidth where
  parseJSON pw = genericParseJSON options pw
    where
      options =
        defaultOptions
        { fieldLabelModifier = downcaseHead . drop 10
        }

data PieceHeight = PieceHeight
  { pieceHeightAmount :: Height
  , pieceHeightUnits  :: PieceHeightUnits
  } deriving (Eq, Generic, Show)

newtype Height = Height
  { unHeight :: Double
  } deriving (Eq, Generic, Show)

instance ToJSON Height where
  toJSON h = genericToJSON options h
    where
      options =
        defaultOptions
        { unwrapUnaryRecords = True
        }

instance FromJSON Height where
  parseJSON h = genericParseJSON options h
    where
      options =
        defaultOptions
        { unwrapUnaryRecords = True
        }

newtype PieceHeightUnits = PieceHeightUnits
  { unPieceHeightUnits :: Text
  } deriving (Eq, Generic, Show)

instance FromJSON PieceHeightUnits where
  parseJSON phu = genericParseJSON options phu
    where
      options =
        defaultOptions
        { unwrapUnaryRecords = True
        }

instance FromJSON PieceHeight where
  parseJSON ph = genericParseJSON options ph
    where
      options =
        defaultOptions
        { fieldLabelModifier = downcaseHead . drop 11
        }

data PieceWeight = PieceWeight
  { pieceWeightAmount :: Weight
  , pieceWeightUnits  :: PieceWeightUnits
  , pieceWeightType   :: PieceWeightType
  } deriving (Eq, Generic, Show)

newtype Weight = Weight
  { unWeight :: Double
  } deriving (Eq, Generic, Show)

instance ToJSON Weight where
  toJSON w = genericToJSON options w
    where
      options =
        defaultOptions
        { unwrapUnaryRecords = True
        }

instance FromJSON Weight where
  parseJSON w = genericParseJSON options w
    where
      options =
        defaultOptions
        { unwrapUnaryRecords = True
        }

newtype PieceWeightUnits = PieceWeightUnits
  { unPieceWeightUnits :: Text
  } deriving (Eq, Generic, Show)

instance FromJSON PieceWeightUnits where
  parseJSON pwu = genericParseJSON options pwu
    where
      options =
        defaultOptions
        { unwrapUnaryRecords = True
        }

newtype PieceWeightType = PieceWeightType
  { unPieceWeightType :: Text
  } deriving (Eq, Generic, Show)

instance FromJSON PieceWeightType where
  parseJSON pwt = genericParseJSON options pwt
    where
      options =
        defaultOptions
        { unwrapUnaryRecords = True
        }

instance FromJSON PieceWeight where
  parseJSON pw = genericParseJSON options pw
    where
      options =
        defaultOptions
        { fieldLabelModifier = downcaseHead . drop 11
        }

data PieceSubWeight = PieceSubWeight
  { pieceSubWeightAmount :: Weight
  , pieceSubWeightUnits  :: PieceSubWeightUnits
  , pieceSubWeightType   :: PieceSubWeightType
  } deriving (Eq, Generic, Show)

newtype PieceSubWeightUnits = PieceSubWeightUnits
  { unPieceSubWeightUnits :: Text
  } deriving (Eq, Generic, Show)

instance FromJSON PieceSubWeightUnits where
  parseJSON pwu = genericParseJSON options pwu
    where
      options =
        defaultOptions
        { unwrapUnaryRecords = True
        }

newtype PieceSubWeightType = PieceSubWeightType
  { unPieceSubWeightType :: Text
  } deriving (Eq, Generic, Show)

instance FromJSON PieceSubWeightType where
  parseJSON pwt = genericParseJSON options pwt
    where
      options =
        defaultOptions
        { unwrapUnaryRecords = True
        }

instance FromJSON PieceSubWeight where
  parseJSON psw = genericParseJSON options psw
    where
      options =
        defaultOptions
        { fieldLabelModifier = downcaseHead . drop 14
        }

data PieceContent = PieceContent
  { pieceContentSku      :: SKU
  , pieceContentQuantity :: Quantity
  } deriving (Eq, Generic, Show)

instance FromJSON PieceContent where
  parseJSON pc = genericParseJSON options pc
    where
      options =
        defaultOptions
        { fieldLabelModifier = downcaseHead . drop 12
        }

type Reply = Network.HTTP.Client.Response BSL.ByteString
type Method = NHTM.Method

data ShipwireRequest a b c = ShipwireRequest
  { rMethod  :: Method -- ^ Method of ShipwireRequest
  , endpoint :: Text -- ^ Endpoint of ShipwireRequest
  , params   :: [Params TupleBS8 BSL.ByteString] -- ^ Request params of ShipwireRequest
  }

mkShipwireRequest :: Method
                  -> Text
                  -> [Params TupleBS8 BSL.ByteString]
                  -> ShipwireRequest a b c
mkShipwireRequest m e p = ShipwireRequest m e p

type family ShipwireReturn a :: *

data RateRequest
type instance ShipwireReturn RateRequest = RateResponse

---------------------------------------------------------------------
-- Stock Endpoint -- https://www.shipwire.com/w/developers/stock/
---------------------------------------------------------------------

data StockRequest
type instance ShipwireReturn StockRequest = StockResponse
instance ShipwireHasParam StockRequest SKU
instance ShipwireHasParam StockRequest ParentId
instance ShipwireHasParam StockRequest ProductIdParam
instance ShipwireHasParam StockRequest ProductExternalIdParam
instance ShipwireHasParam StockRequest WarehouseIdParam
instance ShipwireHasParam StockRequest WarehouseExternalIdParam
instance ShipwireHasParam StockRequest WarehouseRegionParam
instance ShipwireHasParam StockRequest WarehouseAreaParam
instance ShipwireHasParam StockRequest ChannelName
instance ShipwireHasParam StockRequest IncludeEmpty
instance ShipwireHasParam StockRequest VendorIdParam
instance ShipwireHasParam StockRequest VendorExternalIdParam
instance ShipwireHasParam StockRequest DisableAutoBreakLots
instance ShipwireHasParam StockRequest Mode
instance ShipwireHasParam StockRequest IncludeEmptyShipwireAnywhere
instance ShipwireHasParam StockRequest Offset
instance ShipwireHasParam StockRequest Total
instance ShipwireHasParam StockRequest Previous
instance ShipwireHasParam StockRequest Next
instance ShipwireHasParam StockRequest Limit

data StockResponse = StockResponse
  { stockResponseStatus           :: ResponseStatus
  , stockResponseMessage          :: ResponseMessage
  , stockResponseWarnings         :: Maybe ResponseWarnings
  , stockResponseErrors           :: Maybe ResponseErrors
  , stockResponseResourceLocation :: Maybe ResponseResourceLocation
  , stockResponseResource         :: StockResource
  } deriving (Eq, Generic, Show)

newtype ParentId = ParentId
  { parentId :: Text
  } deriving (Eq, Generic, Show)

newtype ProductIdParam = ProductIdParam
  { productIdParam :: [Text]
  } deriving (Eq, Generic, Show)

newtype ProductExternalIdParam = ProductExternalIdParam
  { productExternalIdParam :: [Text]
  } deriving (Eq, Generic, Show)

newtype WarehouseIdParam = WarehouseIdParam
  { warehouseIdParam :: [Text]
  } deriving (Eq, Generic, Show)

newtype WarehouseExternalIdParam = WarehouseExternalIdParam
  { warehouseExternalIdParam :: [Text]
  } deriving (Eq, Generic, Show)

newtype WarehouseRegionParam = WarehouseRegionParam
  { warehouseRegionParam :: [Text]
  } deriving (Eq, Generic, Show)

newtype WarehouseAreaParam = WarehouseAreaParam
  { warehouseAreaParam :: [Text]
  } deriving (Eq, Generic, Show)

newtype ChannelName = ChannelName
  { channelName :: Text
  } deriving (Eq, Generic, Show)

instance ToJSON ChannelName where
  toJSON cn = genericToJSON options cn
    where
      options =
        defaultOptions
        { unwrapUnaryRecords = True
        }

newtype IncludeEmpty = IncludeEmpty
  { includeEmpty :: Integer
  } deriving (Eq, Generic, Show)

newtype VendorIdParam = VendorIdParam
  { vendorIdParam :: [Integer]
  } deriving (Eq, Generic, Show)

newtype VendorExternalIdParam = VendorExternalIdParam
  { vendorExternalIdParam :: [Integer]
  } deriving (Eq, Generic, Show)

newtype DisableAutoBreakLots = DisableAutoBreakLots
  { disableAutoBreakLots :: Text
  } deriving (Eq, Generic, Show)

data Mode
  = IncludingHigherLevelQuantitiesWithLots
  | IncludingHigherLevelQuantitiesWithoutLots
  | NotIncludingHigherLevelQuantitiesWithLots
  | NotIncludingHigherLevelQuantitiesWithoutLots
  deriving (Eq, Generic, Show)

modeToBS8 :: Mode -> BS8.ByteString
modeToBS8 IncludingHigherLevelQuantitiesWithLots = "IncludingHigherLevelQuantitiesWithLots"
modeToBS8 IncludingHigherLevelQuantitiesWithoutLots = "IncludingHigherLevelQuantitiesWithoutLots"
modeToBS8 NotIncludingHigherLevelQuantitiesWithLots = "NotIncludingHigherLevelQuantitiesWithLots"
modeToBS8 NotIncludingHigherLevelQuantitiesWithoutLots = "NotIncludingHigherLevelQuantitiesWithoutLots"
modeToBS8 _ = error "Bad input"

newtype IncludeEmptyShipwireAnywhere = IncludeEmptyShipwireAnywhere
  { inclEmptyShipwireAnywhere :: Text
  } deriving (Eq, Generic, Show)

newtype Offset = Offset
  { unOffset :: Integer
  } deriving (Eq, Generic, Show)

instance FromJSON Offset where
  parseJSON o = genericParseJSON options o
    where
      options =
        defaultOptions
        { fieldLabelModifier = downcaseHead . drop 2
        , unwrapUnaryRecords = True
        }

newtype Total = Total
  { unTotal :: Integer
  } deriving (Eq, Generic, Show)

instance FromJSON Total where
  parseJSON t = genericParseJSON options t
    where
      options =
        defaultOptions
        { fieldLabelModifier = downcaseHead . drop 2
        , unwrapUnaryRecords = True
        }

newtype Previous = Previous
  { unPrevious :: Text
  } deriving (Eq, Generic, Show)

instance FromJSON Previous where
  parseJSON p = genericParseJSON options p
    where
      options =
        defaultOptions
        { fieldLabelModifier = downcaseHead . drop 2
        , unwrapUnaryRecords = True
        }

newtype Next = Next
  { unNext :: Text
  } deriving (Eq, Generic, Show)

instance FromJSON Next where
  parseJSON n = genericParseJSON options n
    where
      options =
        defaultOptions
        { fieldLabelModifier = downcaseHead . drop 2
        , unwrapUnaryRecords = True
        }

newtype Limit = Limit
  { unLimit :: Integer
  } deriving (Eq, Generic, Show)

instance FromJSON StockResponse where
  parseJSON sr = genericParseJSON options sr
    where
      options =
        defaultOptions
        { fieldLabelModifier = downcaseHead . drop 13
        , omitNothingFields  = True
        }

data StockResource = StockResource
  { stockResponseOffset   :: ResponseOffset
  , stockResponseTotal    :: ResponseTotal
  , stockResponsePrevious :: Maybe ResponsePrevious
  , stockResponseNext     :: Maybe ResponseNext
  , stockResponseItems    :: [StockItem]
  } deriving (Eq, Generic, Show)

newtype ResponseOffset = ResponseOffset
  { unResponseOffset :: Integer
  } deriving (Eq, Generic, Show)

instance FromJSON ResponseOffset where
  parseJSON ro = genericParseJSON options ro
    where
      options =
        defaultOptions
        { unwrapUnaryRecords = True
        }

newtype ResponseTotal = ResponseTotal
  { unResponseTotal :: Integer
  } deriving (Eq, Generic, Show)

instance FromJSON ResponseTotal where
  parseJSON rt = genericParseJSON options rt
    where
      options =
        defaultOptions
        { unwrapUnaryRecords = True
        }

newtype ResponsePrevious = ResponsePrevious
  { unResponsePrevious :: Text
  } deriving (Eq, Generic, Show)

instance FromJSON ResponsePrevious where
  parseJSON rp = genericParseJSON options rp
    where
      options =
        defaultOptions
        { unwrapUnaryRecords = True
        }

newtype ResponseNext = ResponseNext
  { unResponseNext :: Text
  } deriving (Eq, Generic, Show)

instance FromJSON ResponseNext where
  parseJSON rn = genericParseJSON options rn
    where
      options =
        defaultOptions
        { unwrapUnaryRecords = True
        }

instance FromJSON StockResource where
  parseJSON sr = genericParseJSON options sr
    where
      options =
        defaultOptions
        { fieldLabelModifier = downcaseHead . drop 13
        , omitNothingFields  = True
        }

data StockItem = StockItem
  { stockItemResourceLocation :: Maybe ResponseResourceLocation
  , stockItemResource         :: StockItemResource
  } deriving (Eq, Generic, Show)

instance FromJSON StockItem where
  parseJSON si = genericParseJSON options si
    where
      options =
        defaultOptions
        { fieldLabelModifier = downcaseHead . drop 9
        , omitNothingFields  = True
        }

data StockItemResource = StockItemResource
  { sirProductId           :: ProductId
  , sirProductExternalId   :: Maybe ProductExternalId
  , sirSku                 :: SKU
  , sirIsBundle            :: IsBundle
  , sirIsAlias             :: IsAlias
  , sirWarehouseRegion     :: WarehouseRegion
  , sirWarehouseId         :: WarehouseId
  , sirWarehouseExternalId :: Maybe WarehouseExternalId
  , sirPending             :: StockItemResourcePending
  , sirGood                :: StockItemResourceGood
  , sirReserved            :: StockItemResourceReserved
  , sirBackordered         :: StockItemResourceBackordered
  , sirShipping            :: StockItemResourceShipping
  , sirShipped             :: StockItemResourceShipped
  , sirCreating            :: StockItemResourceCreating
  , sirConsuming           :: StockItemResourceConsuming
  , sirConsumed            :: StockItemResourceConsumed
  , sirCreated             :: StockItemResourceCreated
  , sirDamaged             :: StockItemResourceDamaged
  , sirReturned            :: StockItemResourceReturned
  , sirInreview            :: StockItemResourceInReview
  , sirAvailableDate       :: Maybe StockItemResourceAvailableDate
  , sirShippedLastDay      :: StockItemResourceShippedLastDay
  , sirShippedLastWeek     :: StockItemResourceShippedLastWeek
  , sirShippedLast4Weeks   :: StockItemResourceShippedLast4Weeks
  , sirOrderedLastDay      :: StockItemResourceOrderedLastDay
  , sirOrderedLastWeek     :: StockItemResourceOrderedLastWeek
  , sirOrderedLast4Weeks   :: StockItemResourceOrderedLast4Weeks
  } deriving (Eq, Generic, Show)

type ProductId = Id

type ProductExternalId = ExternalId

newtype WarehouseRegion = WarehouseRegion
  { unWarehouseRegion :: Text
  } deriving (Eq, Generic, Show)

instance ToJSON WarehouseRegion where
  toJSON w = genericToJSON options w
    where
      options =
        defaultOptions
        { unwrapUnaryRecords = True
        }

instance FromJSON WarehouseRegion where
  parseJSON wr = genericParseJSON options wr
    where
      options =
        defaultOptions
        { unwrapUnaryRecords = True
        }

newtype StockItemResourcePending = StockItemResourcePending
  { unStockItemResourcePending :: Integer
  } deriving (Eq, Generic, Show)

instance FromJSON StockItemResourcePending where
  parseJSON p = genericParseJSON options p
    where
      options =
        defaultOptions
        { unwrapUnaryRecords = True
        }

newtype StockItemResourceGood = StockItemResourceGood
  { unStockItemResourceGood :: Integer
  } deriving (Eq, Generic, Show)

instance FromJSON StockItemResourceGood where
  parseJSON g = genericParseJSON options g
    where
      options =
        defaultOptions
        { unwrapUnaryRecords = True
        }

newtype StockItemResourceReserved = StockItemResourceReserved
  { unStockItemResourceReserved :: Integer
  } deriving (Eq, Generic, Show)

instance FromJSON StockItemResourceReserved where
  parseJSON r = genericParseJSON options r
    where
      options =
        defaultOptions
        { unwrapUnaryRecords = True
        }

newtype StockItemResourceBackordered = StockItemResourceBackordered
  { unStockItemResourceBackorderd :: Integer
  } deriving (Eq, Generic, Show)

instance FromJSON StockItemResourceBackordered where
  parseJSON b = genericParseJSON options b
    where
      options =
        defaultOptions
        { unwrapUnaryRecords = True
        }

newtype StockItemResourceShipping = StockItemResourceShipping
  { unStockItemResourceShipping :: Integer
  } deriving (Eq, Generic, Show)

instance FromJSON StockItemResourceShipping where
  parseJSON s = genericParseJSON options s
    where
      options =
        defaultOptions
        { unwrapUnaryRecords = True
        }

newtype StockItemResourceShipped = StockItemResourceShipped
  { unStockItemResourceShipped :: Integer
  } deriving (Eq, Generic, Show)

instance FromJSON StockItemResourceShipped where
  parseJSON s = genericParseJSON options s
    where
      options =
        defaultOptions
        { unwrapUnaryRecords = True
        }

newtype StockItemResourceCreating = StockItemResourceCreating
  { unStockItemResourceCreating :: Integer
  } deriving (Eq, Generic, Show)

instance FromJSON StockItemResourceCreating where
  parseJSON c = genericParseJSON options c
    where
      options =
        defaultOptions
        { unwrapUnaryRecords = True
        }

newtype StockItemResourceConsuming = StockItemResourceConsuming
  { unStockItemResourceConsuming :: Integer
  } deriving (Eq, Generic, Show)

instance FromJSON StockItemResourceConsuming where
  parseJSON c = genericParseJSON options c
    where
      options =
        defaultOptions
        { unwrapUnaryRecords = True
        }

newtype StockItemResourceConsumed = StockItemResourceConsumed
  { unStockItemResourceConsumed :: Integer
  } deriving (Eq, Generic, Show)

instance FromJSON StockItemResourceConsumed where
  parseJSON s = genericParseJSON options s
    where
      options =
        defaultOptions
        { unwrapUnaryRecords = True
        }

newtype StockItemResourceCreated = StockItemResourceCreated
  { unStockItemResourceCreated :: Integer
  } deriving (Eq, Generic, Show)

instance FromJSON StockItemResourceCreated where
  parseJSON c = genericParseJSON options c
    where
      options =
        defaultOptions
        { unwrapUnaryRecords = True
        }

newtype StockItemResourceDamaged = StockItemResourceDamaged
  { unStockItemResourceDamaged :: Integer
  } deriving (Eq, Generic, Show)

instance FromJSON StockItemResourceDamaged where
  parseJSON d = genericParseJSON options d
    where
      options =
        defaultOptions
        { unwrapUnaryRecords = True
        }

newtype StockItemResourceReturned = StockItemResourceReturned
  { unStockItemResourceReturned :: Integer
  } deriving (Eq, Generic, Show)

instance FromJSON StockItemResourceReturned where
  parseJSON r = genericParseJSON options r
    where
      options =
        defaultOptions
        { unwrapUnaryRecords = True
        }

newtype StockItemResourceInReview = StockItemResourceInReview
  { unStockItemResourceInReview :: Integer
  } deriving (Eq, Generic, Show)

instance FromJSON StockItemResourceInReview where
  parseJSON i = genericParseJSON options i
    where
      options =
        defaultOptions
        { unwrapUnaryRecords = True
        }

type StockItemResourceAvailableDate = ExpectedShipDate

newtype StockItemResourceShippedLastDay = StockItemResourceShippedLastDay
  { unStockItemResourceShippedLastDay :: Integer
  } deriving (Eq, Generic, Show)

instance FromJSON StockItemResourceShippedLastDay where
  parseJSON sld = genericParseJSON options sld
    where
      options =
        defaultOptions
        { unwrapUnaryRecords = True
        }

newtype StockItemResourceShippedLastWeek = StockItemResourceShippedLastWeek
  { unStockItemResourceShippedLastWeek :: Integer
  } deriving (Eq, Generic, Show)

instance FromJSON StockItemResourceShippedLastWeek where
  parseJSON slw = genericParseJSON options slw
    where
      options =
        defaultOptions
        { unwrapUnaryRecords = True
        }

newtype StockItemResourceShippedLast4Weeks = StockItemResourceShippedLast4Weeks
  { unStockItemResourceShippedLast4Weeks :: Integer
  } deriving (Eq, Generic, Show)

instance FromJSON StockItemResourceShippedLast4Weeks where
  parseJSON sls = genericParseJSON options sls
    where
      options =
        defaultOptions
        { unwrapUnaryRecords = True
        }

newtype StockItemResourceOrderedLastDay = StockItemResourceOrderedLastDay
  { unStockItemResourceOrderedLastDay :: Integer
  } deriving (Eq, Generic, Show)

instance FromJSON StockItemResourceOrderedLastDay where
  parseJSON old = genericParseJSON options old
    where
      options =
        defaultOptions
        { unwrapUnaryRecords = True
        }

newtype StockItemResourceOrderedLastWeek = StockItemResourceOrderedLastWeek
  { unStockItemResourceOrderedLastWeek :: Integer
  } deriving (Eq, Generic, Show)

instance FromJSON StockItemResourceOrderedLastWeek where
  parseJSON olw = genericParseJSON options olw
    where
      options =
        defaultOptions
        { unwrapUnaryRecords = True
        }

newtype StockItemResourceOrderedLast4Weeks = StockItemResourceOrderedLast4Weeks
  { unStockItemResourceOrderedLast4Weeks :: Integer
  } deriving (Eq, Generic, Show)

instance FromJSON StockItemResourceOrderedLast4Weeks where
  parseJSON ols = genericParseJSON options ols
    where
      options =
        defaultOptions
        { unwrapUnaryRecords = True
        }

instance FromJSON StockItemResource where
  parseJSON sir = genericParseJSON options sir
    where
      options =
        defaultOptions
        { fieldLabelModifier = downcaseHead . drop 3
        , omitNothingFields  = True
        }

data IsBundle
  = Bundle
  | NotBundle
  deriving (Eq, Generic, Show)

instance FromJSON IsBundle where
  parseJSON (Number 1) = pure Bundle
  parseJSON (Number 0) = pure NotBundle
  parseJSON o = fail ("Unexpected isBundle value: " <> show o)

data IsAlias
  = Alias
  | NotAlias
  deriving (Eq, Generic, Show)

instance FromJSON IsAlias where
  parseJSON (Number 1) = pure Alias
  parseJSON (Number 0) = pure NotAlias
  parseJSON o = fail ("Unexpected isAlias value: " <> show o)

-- | Either production or sandbox API host
type Host = Text

-- baseUrl :: Host
-- baseUrl = "https://api.shipwire.com/api/v3"

-- sandboxUrl :: Host
-- sandboxUrl = "https://api.beta.shipwire.com/api/v3"

data ShipwireHost =
    ShipwireProduction
  | ShipwireSandbox
  deriving (Eq, Show)

hostUri :: ShipwireHost -> Text
hostUri ShipwireProduction = "https://api.shipwire.com/api/v3"
hostUri ShipwireSandbox = "https://api.beta.shipwire.com/api/v3"

-- | Shipwire authenticates through
data ShipwireConfig = ShipwireConfig
  { host  :: ShipwireHost
  , email :: Username
  , pass  :: Password
  }

-- Possibly a bad idea. I don't know
-- why they auth like this.
credentialsEnv :: IO (Username, Password)
credentialsEnv = do
  login <- getEnv "SHIPWIRE_USER"
  passw <- getEnv "SHIPWIRE_PASS"
  return (Username $ BS8.pack login, Password $ BS8.pack passw)

prodEnvConfig :: IO ShipwireConfig
prodEnvConfig = do
  (login, passw) <- credentialsEnv
  return $ ShipwireConfig ShipwireProduction login passw

sandboxEnvConfig :: IO ShipwireConfig
sandboxEnvConfig = do
  (login, passw) <- credentialsEnv
  return $ ShipwireConfig ShipwireSandbox login passw

-- | Parameters for each request which include both the query and the body of a
-- request
data Params b c
  = Query TupleBS8
  | Body BSL.ByteString
  deriving (Show)

-- | Type alias for query parameters
type TupleBS8 = (BS8.ByteString, BS8.ByteString)

-- | Convert a parameter to a key/value
class ToShipwireParam param where
  toShipwireParam :: param -> [Params TupleBS8 c] -> [Params TupleBS8 c]

instance ToShipwireParam SKU where
  toShipwireParam (SKU i) =
    (Query ("sku", TE.encodeUtf8 i) :)

instance ToShipwireParam ParentId where
  toShipwireParam (ParentId i) =
    (Query ("parentId", TE.encodeUtf8 i) :)

instance ToShipwireParam ProductIdParam where
  toShipwireParam (ProductIdParam xs) =
    (Query ("productId", TE.encodeUtf8 (T.intercalate "," xs)) :)

instance ToShipwireParam ProductExternalIdParam where
  toShipwireParam (ProductExternalIdParam xs) =
    (Query ("productExternalId", TE.encodeUtf8 (T.intercalate "," xs)) :)

instance ToShipwireParam WarehouseIdParam where
  toShipwireParam (WarehouseIdParam xs) =
    (Query ("warehouseId", TE.encodeUtf8 (T.intercalate "," xs)) :)

instance ToShipwireParam WarehouseExternalIdParam where
  toShipwireParam (WarehouseExternalIdParam xs) =
    (Query ("warehouseExternalId", TE.encodeUtf8 (T.intercalate "," xs)) :)

instance ToShipwireParam WarehouseRegionParam where
  toShipwireParam (WarehouseRegionParam xs) =
    (Query ("warehouseRegion", TE.encodeUtf8 (T.intercalate "," xs)) :)

instance ToShipwireParam WarehouseAreaParam where
  toShipwireParam (WarehouseAreaParam xs) =
    (Query ("warehouseArea", TE.encodeUtf8 (T.intercalate "," xs)) :)

instance ToShipwireParam ChannelName where
  toShipwireParam (ChannelName n) =
    (Query ("channelName", TE.encodeUtf8 n) :)

instance ToShipwireParam IncludeEmpty where
  toShipwireParam (IncludeEmpty b) =
    (Query ("includeEmpty", TE.encodeUtf8 $ (T.pack . show) b) :)

instance ToShipwireParam VendorIdParam where
  toShipwireParam (VendorIdParam vi) =
    (Query ("vendorId", TE.encodeUtf8 $ T.intercalate "," $ map (T.pack . show) vi) :)

instance ToShipwireParam VendorExternalIdParam where
  toShipwireParam (VendorExternalIdParam vei) =
    (Query ("vendorExternalId", TE.encodeUtf8 $ T.intercalate "," $ map (T.pack . show) vei) :)

instance ToShipwireParam DisableAutoBreakLots where
  toShipwireParam (DisableAutoBreakLots d) =
    (Query ("disableAutoBreakLots", TE.encodeUtf8 d) :)

instance ToShipwireParam Mode where
  toShipwireParam m =
    (Query ("mode", (modeToBS8 m)) :)

instance ToShipwireParam IncludeEmptyShipwireAnywhere where
  toShipwireParam (IncludeEmptyShipwireAnywhere i) =
    (Query ("includeEmptyShipwireAnywhere", TE.encodeUtf8 i) :)

instance ToShipwireParam Offset where
  toShipwireParam (Offset o) =
    (Query ("offset", TE.encodeUtf8 $ (T.pack . show) o) :)

instance ToShipwireParam Total where
  toShipwireParam (Total t) =
    (Query ("total", TE.encodeUtf8 $ (T.pack . show) t) :)

instance ToShipwireParam Previous where
  toShipwireParam (Previous p) =
    (Query ("previous", TE.encodeUtf8 p) :)

instance ToShipwireParam Next where
  toShipwireParam (Next n) =
    (Query ("next", TE.encodeUtf8 n) :)

instance ToShipwireParam Limit where
  toShipwireParam (Limit l) =
    (Query ("limit", TE.encodeUtf8 $ (T.pack . show) l) :)
    
class (ToShipwireParam param) => ShipwireHasParam request param where
  
-- | Add an optional query parameter
(-&-)
  :: ShipwireHasParam request param
  => ShipwireRequest request b c -> param -> ShipwireRequest request b c
stripeRequest -&- param =
  stripeRequest
  { params = toShipwireParam param (params stripeRequest)
  }

-- | Find the body from the list of [Params TupleBS8 BSL.ByteString]
filterBody :: [Params b c] -> BSL.ByteString
filterBody [] = ""
filterBody xs = case [c | Body c <- xs] of
               [] -> ""
               [c] -> c
               _ -> error "Bad input"

-- | Find the query parameters from the list of
-- [Params TupleBS8 BSL.ByteString]
filterQuery :: [Params (BS8.ByteString, BS8.ByteString) c] -> [(BS8.ByteString, BS8.ByteString)]
filterQuery [] = []
filterQuery xs = [b | Query b <- xs]

-------------------------------------------------------------------------
-- Receiving Endpoint -- https://www.shipwire.com/w/developers/receiving
-------------------------------------------------------------------------

data CreateReceivingRequest
data GetReceivingsRequest
type instance ShipwireReturn CreateReceivingRequest = CreateReceivingResponse
type instance ShipwireReturn GetReceivingsRequest = GetReceivingsResponse
instance ShipwireHasParam GetReceivingsRequest ExpandParamReceivings
instance ShipwireHasParam GetReceivingsRequest CommerceNameParam
instance ShipwireHasParam GetReceivingsRequest TransactionIdParam
instance ShipwireHasParam GetReceivingsRequest ExternalIdParam
instance ShipwireHasParam GetReceivingsRequest OrderIdParam
instance ShipwireHasParam GetReceivingsRequest OrderNoParam
instance ShipwireHasParam GetReceivingsRequest StatusParams
instance ShipwireHasParam GetReceivingsRequest UpdatedAfter
instance ShipwireHasParam GetReceivingsRequest WarehouseIdParam
instance ShipwireHasParam GetReceivingsRequest WarehouseExternalIdParam

-- | ISO 8601 format, ex: "2014-05-30T13:08:29-07:00"
newtype UpdatedAfter = UpdatedAfter
  { updatedAfter :: Text
  } deriving (Eq, Show)

instance ToShipwireParam UpdatedAfter where
  toShipwireParam (UpdatedAfter x) =
    (Query ("updatedAfter", TE.encodeUtf8 x) :)

newtype StatusParams = StatusParams
  { statusParam :: [StatusParam]
  } deriving (Eq, Show)

data StatusParam = StatusProcessed
  | StatusCanceled
  | StatusCompleted
  | StatusDelivered
  | StatusReturned
  | StatusSubmitted
  | StatusHeld
  | StatusTracked
  deriving (Eq, Show)

statusParamToTx :: StatusParam -> Text
statusParamToTx StatusProcessed = "processed"
statusParamToTx StatusCanceled  = "canceled"
statusParamToTx StatusCompleted = "completed"
statusParamToTx StatusDelivered = "delivered"
statusParamToTx StatusReturned  = "returned"
statusParamToTx StatusSubmitted = "submitted"
statusParamToTx StatusHeld      = "held"
statusParamToTx StatusTracked   = "tracked"
statusParamToTx _               = error "Bad input"

instance ToShipwireParam StatusParams where
  toShipwireParam (StatusParams xs) =
    (Query ("status", TE.encodeUtf8 (T.intercalate "," (map statusParamToTx xs))) :)

newtype OrderNoParam = OrderNoParam
  { orderNoParam :: [Text]
  } deriving (Eq, Show)

instance ToShipwireParam OrderNoParam where
  toShipwireParam (OrderNoParam xs) =
    (Query ("orderNo", TE.encodeUtf8 (T.intercalate "," xs)) :)

newtype OrderIdParam = OrderIdParam
  { orderIdParam :: [Text]
  } deriving (Eq, Show)

instance ToShipwireParam OrderIdParam where
  toShipwireParam (OrderIdParam xs) =
    (Query ("orderId", TE.encodeUtf8 (T.intercalate "," xs)) :)

newtype ExternalIdParam = ExternalIdParam
  { externalIdParam :: [Text]
  } deriving (Eq, Show)

instance ToShipwireParam ExternalIdParam where
  toShipwireParam (ExternalIdParam xs) =
    (Query ("externalId", TE.encodeUtf8 (T.intercalate "," xs)) :)

newtype TransactionIdParam = TransactionIdParam
  { transactionIdParam :: [Text]
  } deriving (Eq, Show)

instance ToShipwireParam TransactionIdParam where
  toShipwireParam (TransactionIdParam xs) =
    (Query ("transactionId", TE.encodeUtf8 (T.intercalate "," xs)) :)
    
newtype ExpandParamReceivings = ExpandParamReceivings
  { expandParamReceivings :: [ExpandReceivings]
  } deriving (Eq, Show)

data ExpandReceivings = ExpandHolds
  | ExpandInstructionsRecipients
  | ExpandItems
  | ExpandShipments
  | ExpandLabels
  | ExpandTrackings
  | ExpandAll
  deriving (Eq, Show)

expandReceivingsToTx :: ExpandReceivings -> Text
expandReceivingsToTx ExpandHolds                  = "holds"
expandReceivingsToTx ExpandInstructionsRecipients = "instructionsRecipients"
expandReceivingsToTx ExpandItems                  = "items"
expandReceivingsToTx ExpandShipments              = "shipments"
expandReceivingsToTx ExpandLabels                 = "labels"
expandReceivingsToTx ExpandTrackings              = "trackings"
expandReceivingsToTx ExpandAll                    = "all"
expandReceivingsToTx _                            = error "Bad input"

instance ToShipwireParam ExpandParamReceivings where
  toShipwireParam (ExpandParamReceivings xs) =
    (Query ("expand", TE.encodeUtf8 (T.intercalate "," (map expandReceivingsToTx xs))) :)

newtype CommerceNameParam = CommerceNameParam
  { commerceNameParam :: [Text]
  } deriving (Eq, Generic, Show)

instance ToShipwireParam CommerceNameParam where
  toShipwireParam (CommerceNameParam ns) =
    (Query ("commerceName", TE.encodeUtf8 (T.intercalate "," ns)) :)

type CreateReceivingResponse = ReceivingsResponse
type GetReceivingsResponse = ReceivingsResponse

data ReceivingsResponse = ReceivingsResponse
  { receivingsResponseResourceLocation :: ResponseResourceLocation
  , receivingsResponseStatus           :: ResponseStatus
  , receivingsResponseMessage          :: ResponseMessage
  , receivingsResponseResource         :: ReceivingsResource
  } deriving (Eq, Generic, Show)

instance FromJSON ReceivingsResponse where
  parseJSON rr = genericParseJSON options rr
    where
      options =
        defaultOptions
        { fieldLabelModifier = downcaseHead . drop 18
        }

data ReceivingsResource = ReceivingsResource
  { receivingsResponseNext     :: Maybe ResponseNext
  , receivingsResponseOffset   :: ResponseOffset
  , receivingsResponsePrevious :: Maybe ResponsePrevious
  , receivingsResponseTotal    :: ResponseTotal
  , receivingsResponseItems    :: ReceivingsItems
  } deriving (Eq, Generic, Show)

instance FromJSON ReceivingsResource where
  parseJSON rr = genericParseJSON options rr
    where
      options =
        defaultOptions
        { fieldLabelModifier = downcaseHead . drop 18
        }

newtype ReceivingsItems = ReceivingsItems
  { unReceivingsItems :: [ReceivingsItem]
  } deriving (Eq, Generic, Show)

instance FromJSON ReceivingsItems where
  parseJSON ri = genericParseJSON options ri
    where
      options =
        defaultOptions
        { fieldLabelModifier = downcaseHead . drop 12
        , unwrapUnaryRecords = True
        }

data ReceivingsItem = ReceivingsItem
  { receivingsItemResourceLocation :: Maybe ResponseResourceLocation
  , receivingsItemResource         :: ReceivingsItemResource
  } deriving (Eq, Generic, Show)

instance FromJSON ReceivingsItem where
  parseJSON ri = genericParseJSON options ri
    where
      options =
        defaultOptions
        { fieldLabelModifier = downcaseHead . drop 14
        }

data ReceivingsItemResource = ReceivingsItemResource
  { rirId                     :: Id
  , rirExternalId             :: Maybe ExternalId
  , rirOrderNo                :: Maybe OrderNo
  , rirTransactionId          :: TransactionId
  , rirExpectedDate           :: Maybe ExpectedDateUTCTime
  , rirCommerceName           :: CommerceName
  , rirLastUpdatedDate        :: LastUpdatedDate
  , rirStatus                 :: ItemResourceStatus
  , rirHolds                  :: ItemResourceHolds
  , rirItems                  :: ItemResourceItems
  , rirTrackings              :: ItemResourceTrackings
  , rirShipments              :: ItemResourceShipments
  , rirLabels                 :: ItemResourceLabels
  , rirOptions                :: ItemResourceOptions
  , rirArrangement            :: ItemResourceArrangement
  , rirShipFrom               :: ItemResourceShipFrom
  , rirEvents                 :: ItemResourceEvents
  , rirRouting                :: ItemResourceRouting
  , rirInstructionsRecipients :: ItemResourceInstructionsRecipients
  } deriving (Eq, Generic, Show)

instance FromJSON ReceivingsItemResource where
  parseJSON rir = genericParseJSON options rir
    where
      options =
        defaultOptions
        { fieldLabelModifier = downcaseHead . drop 3
        }

data ItemResourceInstructionsRecipients = ItemResourceInstructionsRecipients
  { irirResource         :: Maybe ItemResourceInstructionsRecipientsResource
  , irirResourceLocation :: Maybe ResponseResourceLocation
  } deriving (Eq, Generic, Show)

instance FromJSON ItemResourceInstructionsRecipients where
  parseJSON iri = genericParseJSON options iri
    where
      options =
        defaultOptions
        { fieldLabelModifier = downcaseHead . drop 4
        }

data ItemResourceInstructionsRecipientsResource = ItemResourceInstructionsRecipientsResource
  { irirrItems    :: ItemResourceInstructionsRecipientsResourceItems
  , irirrNext     :: Maybe Next
  , irirrOffset   :: Offset
  , irirrPrevious :: Maybe Previous
  , irirrTotal    :: Total
  } deriving (Eq, Generic, Show)

instance FromJSON ItemResourceInstructionsRecipientsResource where
  parseJSON iri = genericParseJSON options iri
    where
      options =
        defaultOptions
        { fieldLabelModifier = downcaseHead . drop 5
        }

newtype ItemResourceInstructionsRecipientsResourceItems = ItemResourceInstructionsRecipientsResourceItems
  { iririItems :: [ItemResourceInstructionsRecipientsItem]
  } deriving (Eq, Generic, Show)

instance FromJSON ItemResourceInstructionsRecipientsResourceItems where
  parseJSON iri = genericParseJSON options iri
    where
      options =
        defaultOptions
        { fieldLabelModifier = downcaseHead . drop 5
        , unwrapUnaryRecords = True
        }

data ItemResourceInstructionsRecipientsItem = ItemResourceInstructionsRecipientsItem
  { iririResource         :: ItemResourceInstructionsRecipientsItemResource
  , iririResourceLocation :: Maybe ResponseResourceLocation
  } deriving (Eq, Generic, Show)

instance FromJSON ItemResourceInstructionsRecipientsItem where
  parseJSON iri = genericParseJSON options iri
    where
      options =
        defaultOptions
        { fieldLabelModifier = downcaseHead . drop 5
        }

data ItemResourceInstructionsRecipientsItemResource = ItemResourceInstructionsRecipientsItemResource
  { iririrEmail :: Email
  , iririrName  :: Name
  , iririrNote  :: Maybe Note
  } deriving (Eq, Generic, Show)

instance FromJSON ItemResourceInstructionsRecipientsItemResource where
  parseJSON iri = genericParseJSON options iri
    where
      options =
        defaultOptions
        { fieldLabelModifier = downcaseHead . drop 6
        }

data ItemResourceRouting = ItemResourceRouting
  { irrResource         :: ItemResourceRoutingResource
  , irrResourceLocation :: Maybe ResponseResourceLocation
  } deriving (Eq, Generic, Show)

instance FromJSON ItemResourceRouting where
  parseJSON irr = genericParseJSON options irr
    where
      options =
        defaultOptions
        { fieldLabelModifier = downcaseHead . drop 3
        }

data ItemResourceRoutingResource = ItemResourceRoutingResource
  { irrrOriginLatitude      :: Latitude
  , irrrOriginLongitude     :: Longitude
  , irrrWarehouseExternalId :: Maybe WarehouseExternalId
  , irrrWarehouseId         :: WarehouseId
  , irrrWarehouseName       :: WarehouseName
  , irrrWarehouseRegion     :: WarehouseRegion
  } deriving (Eq, Generic, Show)

instance FromJSON ItemResourceRoutingResource where
  parseJSON irr = genericParseJSON options irr
    where
      options =
        defaultOptions
        { fieldLabelModifier = downcaseHead . drop 4
        }

newtype Latitude = Latitude
  { unLatitude :: Double
  } deriving (Eq, Generic, Show)

instance FromJSON Latitude where
  parseJSON l = genericParseJSON options l
    where
      options =
        defaultOptions
        { fieldLabelModifier = downcaseHead . drop 2
        , unwrapUnaryRecords = True
        }

type Longitude = Latitude

type ExpectedDateUTCTime = ExpectedShipDate

type LastUpdatedDate = ExpectedShipDate

data ItemResourceEvents = ItemResourceEvents
  { ireResource         :: ItemResourceEventsResource
  , ireResourceLocation :: Maybe ResponseResourceLocation
  } deriving (Eq, Generic, Show)

instance FromJSON ItemResourceEvents where
  parseJSON ire = genericParseJSON options ire
    where
      options =
        defaultOptions
        { fieldLabelModifier = downcaseHead . drop 3
        }

data ItemResourceEventsResource = ItemResourceEventsResource
  { irerCreatedDate          :: CreatedDate
  , irerPickedUpDate         :: Maybe PickedUpDate
  , irerSubmittedDate        :: Maybe SubmittedDate
  , irerProcessedDate        :: ProcessedDate
  , irerCompletedDate        :: Maybe CompletedDate
  , irerExpectedDate         :: Maybe ExpectedDateText
  , irerDeliveredDate        :: Maybe DeliveredDate
  , irerCancelledDate        :: Maybe CancelledDate
  , irerReturnedDate         :: Maybe ReturnedDate
  , irerLastManualUpdateDate :: Maybe LastManualUpdateDate
  } deriving (Eq, Generic, Show)

instance FromJSON ItemResourceEventsResource where
  parseJSON ire = genericParseJSON options ire
    where
      options =
        defaultOptions
        { fieldLabelModifier = downcaseHead . drop 4
        }

type CreatedDate          = ExpectedShipDate

type PickedUpDate         = ExpectedShipDate

type SubmittedDate        = ExpectedShipDate

type ProcessedDate        = ExpectedShipDate

type CompletedDate        = ExpectedShipDate

type CancelledDate        = ExpectedShipDate

type ReturnedDate         = ExpectedShipDate

type LastManualUpdateDate = ExpectedShipDate

data ItemResourceShipFrom = ItemResourceShipFrom
  { irsfResource         :: ItemResourceShipFromResource
  , irsfResourceLocation :: Maybe ResponseResourceLocation
  } deriving (Eq, Generic, Show)

instance FromJSON ItemResourceShipFrom where
  parseJSON irs = genericParseJSON options irs
    where
      options =
        defaultOptions
        { fieldLabelModifier = downcaseHead . drop 4
        }

data ItemResourceShipFromResource = ItemResourceShipFromResource
  { irsfrEmail      :: Maybe Email
  , irsfrName       :: Maybe Name
  , irsfrAddress1   :: Maybe AddressLine
  , irsfrAddress2   :: Maybe AddressLine
  , irsfrAddress3   :: Maybe AddressLine
  , irsfrCity       :: Maybe City
  , irsfrState      :: Maybe State
  , irsfrPostalCode :: Maybe PostalCode
  , irsfrCountry    :: Maybe Country
  , irsfrPhone      :: Maybe Phone
  } deriving (Eq, Generic, Show)

instance FromJSON ItemResourceShipFromResource where
  parseJSON irs = genericParseJSON options irs
    where
      options =
        defaultOptions
        { fieldLabelModifier = downcaseHead . drop 5
        }

data ItemResourceArrangement = ItemResourceArrangement
  { iraResource         :: ItemResourceArrangementResource
  , iraResourceLocation :: Maybe ResponseResourceLocation
  } deriving (Eq, Generic, Show)

instance FromJSON ItemResourceArrangement where
  parseJSON ira = genericParseJSON options ira
    where
      options =
        defaultOptions
        { fieldLabelModifier = downcaseHead . drop 3
        }

data ItemResourceArrangementResource = ItemResourceArrangementResource
  { irarContact :: Contact
  , irarPhone   :: Phone
  , irarType    :: ArrangementType
  } deriving (Eq, Generic, Show)

instance FromJSON ItemResourceArrangementResource where
  parseJSON ira = genericParseJSON options ira
    where
      options =
        defaultOptions
        { fieldLabelModifier = downcaseHead . drop 4
        }

data ItemResourceOptions = ItemResourceOptions
  { iroResource         :: ItemResourceOptionsResource
  , iroResourceLocation :: Maybe ResponseResourceLocation
  } deriving (Eq, Generic, Show)

instance FromJSON ItemResourceOptions where
  parseJSON iro = genericParseJSON options iro
    where
      options =
        defaultOptions
        { fieldLabelModifier = downcaseHead . drop 3
        }

data ItemResourceOptionsResource = ItemResourceOptionsResource
  { irorWarehouseExternalid :: Maybe WarehouseExternalId
  , irorWarehouseId         :: WarehouseId
  , irorWarehouseRegion     :: WarehouseRegion
  } deriving (Eq, Generic, Show)

instance FromJSON ItemResourceOptionsResource where
  parseJSON iro = genericParseJSON options iro
    where
      options =
        defaultOptions
        { fieldLabelModifier = downcaseHead . drop 4
        }

data ItemResourceLabels = ItemResourceLabels
  { irlResource         :: Maybe ItemResourceLabelsResource
  , irlResourceLocation :: ResponseResourceLocation
  } deriving (Eq, Generic, Show)

instance FromJSON ItemResourceLabels where
  parseJSON irl = genericParseJSON options irl
    where
      options =
        defaultOptions
        { fieldLabelModifier = downcaseHead . drop 3
        }

data ItemResourceLabelsResource = ItemResourceLabelsResource
  { irlrItems    :: ItemResourceLabelsResourceItems
  , irlrNext     :: Maybe Next
  , irlrOffset   :: Offset
  , irlrPrevious :: Maybe Previous
  , irlrTotal    :: Total
  } deriving (Eq, Generic, Show)

instance FromJSON ItemResourceLabelsResource where
  parseJSON irl = genericParseJSON options irl
    where
      options =
        defaultOptions
        { fieldLabelModifier = downcaseHead . drop 4
        }

newtype ItemResourceLabelsResourceItems = ItemResourceLabelsResourceItems
  { irlriItems :: [ItemResourceLabelsResourceItem]
  } deriving (Eq, Generic, Show)

instance FromJSON ItemResourceLabelsResourceItems where
  parseJSON irl = genericParseJSON options irl
    where
      options =
        defaultOptions
        { fieldLabelModifier = downcaseHead . drop 5
        , unwrapUnaryRecords = True
        }

data ItemResourceLabelsResourceItem = ItemResourceLabelsResourceItem
  { irlriResource         :: ItemResourceLabelsResourceItemResource
  , irlriResourceLocation :: Maybe ResponseResourceLocation
  } deriving (Eq, Generic, Show)

instance FromJSON ItemResourceLabelsResourceItem where
  parseJSON irl = genericParseJSON options irl
    where
      options =
        defaultOptions
        { fieldLabelModifier = downcaseHead . drop 5
        }

data ItemResourceLabelsResourceItemResource = ItemResourceLabelsResourceItemResource
  { irlrirLabelId         :: LabelId
  , irlrirOrderId         :: OrderId
  , irlrirOrderExternalId :: OrderExternalId
  } deriving (Eq, Generic, Show)

instance FromJSON ItemResourceLabelsResourceItemResource where
  parseJSON irl = genericParseJSON options irl
    where
      options =
        defaultOptions
        { fieldLabelModifier = downcaseHead . drop 6
        }

type LabelId = Id

type OrderId = Id

type OrderExternalId = ExternalId

newtype ItemResourceStatus = ItemResourceStatus
  { unItemResourceStatus :: Text
  } deriving (Eq, Generic, Show)

instance FromJSON ItemResourceStatus where
  parseJSON irs = genericParseJSON options irs
    where
      options =
        defaultOptions
        { fieldLabelModifier = downcaseHead . drop 2
        , unwrapUnaryRecords = True
        }

data ItemResourceShipments = ItemResourceShipments
  { irsResource         :: Maybe ItemResourceShipmentsResource
  , irsResourceLocation :: ResponseResourceLocation
  } deriving (Eq, Generic, Show)

instance FromJSON ItemResourceShipments where
  parseJSON irs = genericParseJSON options irs
    where
      options =
        defaultOptions
        { fieldLabelModifier = downcaseHead . drop 3
        }

data ItemResourceShipmentsResource = ItemResourceShipmentsResource
  { irsrItems    :: ItemResourceShipmentsResourceItems
  , irsrNext     :: Maybe Next
  , irsrOffset   :: Offset
  , irsrPrevious :: Maybe Previous
  , irsrTotal    :: Total
  } deriving (Eq, Generic, Show)

instance FromJSON ItemResourceShipmentsResource where
  parseJSON irs = genericParseJSON options irs
    where
      options =
        defaultOptions
        { fieldLabelModifier = downcaseHead . drop 4
        }

newtype ItemResourceShipmentsResourceItems = ItemResourceShipmentsResourceItems
  { irsriItems :: [ItemResourceShipmentsResourceItem]
  } deriving (Eq, Generic, Show)

instance FromJSON ItemResourceShipmentsResourceItems where
  parseJSON irs = genericParseJSON options irs
    where
      options =
        defaultOptions
        { fieldLabelModifier = downcaseHead . drop 5
        , unwrapUnaryRecords = True
        }

data ItemResourceShipmentsResourceItem = ItemResourceShipmentsResourceItem
  { irsriResource         :: ItemResourceShipmentsResourceItemResource
  , irsriResourceLocation :: ResponseResourceLocation
  } deriving (Eq, Generic, Show)

instance FromJSON ItemResourceShipmentsResourceItem where
  parseJSON irs = genericParseJSON options irs
    where
      options =
        defaultOptions
        { fieldLabelModifier = downcaseHead . drop 5
        }

data ItemResourceShipmentsResourceItemResource = ItemResourceShipmentsResourceItemResource
  { irsrirShipmentId :: ShipmentId
  , irsrirType       :: Type
  , irsrirHeight     :: Maybe TextHeight
  , irsrirLength     :: Maybe TextLength
  , irsrirWeight     :: Maybe TextWeight
  , irsrirWidth      :: Maybe TextWidth
  } deriving (Eq, Generic, Show)

instance FromJSON ItemResourceShipmentsResourceItemResource where
  parseJSON irs = genericParseJSON options irs
    where
      options =
        defaultOptions
        { fieldLabelModifier = downcaseHead . drop 6
        }

type ShipmentId = Id

newtype TextHeight = TextHeight
  { unTextHeight :: Text
  } deriving (Eq, Generic, Show)

instance FromJSON TextHeight where
  parseJSON th = genericParseJSON options th
    where
      options =
        defaultOptions
        { fieldLabelModifier = downcaseHead . drop 6
        , unwrapUnaryRecords = True
        }

newtype TextLength = TextLength
  { unTextLength :: Text
  } deriving (Eq, Generic, Show)

instance FromJSON TextLength where
  parseJSON tl = genericParseJSON options tl
    where
      options =
        defaultOptions
        { fieldLabelModifier = downcaseHead . drop 6
        , unwrapUnaryRecords = True
        }

newtype TextWeight = TextWeight
  { unTextWeight :: Text
  } deriving (Eq, Generic, Show)

instance FromJSON TextWeight where
  parseJSON tw = genericParseJSON options tw
    where
      options =
        defaultOptions
        { fieldLabelModifier = downcaseHead . drop 6
        , unwrapUnaryRecords = True
        }

newtype TextWidth = TextWidth
  { unTextWidth :: Text
  } deriving (Eq, Generic, Show)

instance FromJSON TextWidth where
  parseJSON tw = genericParseJSON options tw
    where
      options =
        defaultOptions
        { fieldLabelModifier = downcaseHead . drop 6
        , unwrapUnaryRecords = True
        }

data ItemResourceTrackings = ItemResourceTrackings
  { irtResource         :: Maybe ItemResourceTrackingsResource
  , irtResourceLocation :: ResponseResourceLocation
  } deriving (Eq, Generic, Show)

instance FromJSON ItemResourceTrackings where
  parseJSON irt = genericParseJSON options irt
    where
      options =
        defaultOptions
        { fieldLabelModifier = downcaseHead . drop 3
        }
        
data ItemResourceTrackingsResource = ItemResourceTrackingsResource
  { irtrItems    :: ItemResourceTrackingsResourceItems
  , irtrNext     :: Maybe Next
  , irtrOffset   :: Offset
  , irtrPrevious :: Maybe Previous
  , irtrTotal    :: Total
  } deriving (Eq, Generic, Show)

instance FromJSON ItemResourceTrackingsResource where
  parseJSON irt = genericParseJSON options irt
    where
      options =
        defaultOptions
        { fieldLabelModifier = downcaseHead . drop 4
        }

newtype ItemResourceTrackingsResourceItems = ItemResourceTrackingsResourceItems
  { irtriItems :: [ItemResourceTrackingsResourceItem]
  } deriving (Eq, Generic, Show)

instance FromJSON ItemResourceTrackingsResourceItems where
  parseJSON irt = genericParseJSON options irt
    where
      options =
        defaultOptions
        { fieldLabelModifier = downcaseHead . drop 5
        , unwrapUnaryRecords = True
        }

data ItemResourceTrackingsResourceItem = ItemResourceTrackingsResourceItem
  { irtriResource         :: ItemResourceTrackingsResourceItemResource
  , irtriResourceLocation :: ResponseResourceLocation
  } deriving (Eq, Generic, Show)

instance FromJSON ItemResourceTrackingsResourceItem where
  parseJSON irt = genericParseJSON options irt
    where
      options =
        defaultOptions
        { fieldLabelModifier = downcaseHead . drop 5
        }

data ItemResourceTrackingsResourceItemResource = ItemResourceTrackingsResourceItemResource
  { irtrirId              :: Id
  , irtrirOrderExternalId :: Maybe OrderExternalId
  , irtrirOrderId         :: OrderId
  , irtrirTracking        :: Tracking
  , irtrirCarrier         :: CarrierName
  , irtrirContact         :: Maybe Contact
  , irtrirPhone           :: Maybe Phone
  , irtrirUrl             :: Maybe URL
  , irtrirSummary         :: Maybe Summary
  , irtrirSummaryDate     :: Maybe SummaryDate
  , irtrirTrackedDate     :: Maybe TrackedDate
  , irtrirDeliveredDate   :: Maybe DeliveredDate
  } deriving (Eq, Generic, Show)

instance FromJSON ItemResourceTrackingsResourceItemResource where
  parseJSON irt = genericParseJSON options irt
    where
      options =
        defaultOptions
        { fieldLabelModifier = downcaseHead . drop 6
        }

type TrackedDate = ExpectedShipDate

type DeliveredDate = ExpectedShipDate

newtype Summary = Summary
  { unSummary :: Text
  } deriving (Eq, Generic, Show)

instance FromJSON Summary where
  parseJSON s = genericParseJSON options s
    where
      options =
        defaultOptions
        { fieldLabelModifier = downcaseHead . drop 2
        , unwrapUnaryRecords = True
        }

type SummaryDate = ExpectedShipDate

newtype URL = URL
  { unURL :: Text
  } deriving (Eq, Generic, Show)

instance FromJSON URL where
  parseJSON u = genericParseJSON options u
    where
      options =
        defaultOptions
        { fieldLabelModifier = downcaseHead . drop 2
        , unwrapUnaryRecords = True
        }

data ItemResourceItems = ItemResourceItems
  { iriResource         :: Maybe ItemResourceItemsResource
  , iriResourceLocation :: ResponseResourceLocation
  } deriving (Eq, Generic, Show)

instance FromJSON ItemResourceItems where
  parseJSON iri = genericParseJSON options iri
    where
      options =
        defaultOptions
        { fieldLabelModifier = downcaseHead . drop 3
        }

data ItemResourceItemsResource = ItemResourceItemsResource
  { irirItems    :: ItemResourceItemsResourceItems
  , irirNext     :: Maybe Next
  , irirOffset   :: Offset
  , irirPrevious :: Maybe Previous
  , irirTotal    :: Total
  } deriving (Eq, Generic, Show)

instance FromJSON ItemResourceItemsResource where
  parseJSON iri = genericParseJSON options iri
    where
      options =
        defaultOptions
        { fieldLabelModifier = downcaseHead . drop 4
        }

newtype ItemResourceItemsResourceItems = ItemResourceItemsResourceItems
  { iririsItems :: [ItemResourceItemsResourceItem]
  } deriving (Eq, Generic, Show)

instance FromJSON ItemResourceItemsResourceItems where
  parseJSON iri = genericParseJSON options iri
    where
      options =
        defaultOptions
        { fieldLabelModifier = downcaseHead . drop 6
        , unwrapUnaryRecords = True
        }

data ItemResourceItemsResourceItem = ItemResourceItemsResourceItem
  { iririsResource         :: ItemResourceItemsResourceItemResource
  , iririsResourceLocation :: Maybe ResponseResourceLocation
  } deriving (Eq, Generic, Show)

instance FromJSON ItemResourceItemsResourceItem where
  parseJSON iri = genericParseJSON options iri
    where
      options =
        defaultOptions
        { fieldLabelModifier = downcaseHead . drop 6
        }

data ItemResourceItemsResourceItemResource = ItemResourceItemsResourceItemResource
  { iririrOrderExternalId   :: Maybe OrderExternalId
  , iririrOrderId           :: OrderId
  , iririrProductExternalId :: Maybe ProductExternalId
  , iririrProductId         :: ProductId
  , iririrQuantity          :: Quantity
  , iririrSku               :: SKU
  , iririrExpected          :: Expected
  , iririrPending           :: Pending
  , iririrGood              :: Good
  , iririrInReview          :: InReview
  , iririrDamaged           :: Damaged
  } deriving (Eq, Generic, Show)

instance FromJSON ItemResourceItemsResourceItemResource where
  parseJSON iri = genericParseJSON options iri
    where
      options =
        defaultOptions
        { fieldLabelModifier = downcaseHead . drop 6
        }

newtype Expected = Expected
  { unExpected :: Integer
  } deriving (Eq, Generic, Show)

instance FromJSON Expected where
  parseJSON e = genericParseJSON options e
    where
      options =
        defaultOptions
        { fieldLabelModifier = downcaseHead . drop 2
        , unwrapUnaryRecords = True
        }

type Pending = StockItemResourcePending

type Good = StockItemResourceGood

type InReview = StockItemResourceInReview

type Damaged = StockItemResourceDamaged

data ItemResourceHolds = ItemResourceHolds
  { irhResource         :: Maybe ItemResourceHoldsResource
  , irhResourceLocation :: ResponseResourceLocation
  } deriving (Eq, Generic, Show)

instance FromJSON ItemResourceHolds where
  parseJSON irh = genericParseJSON options irh
    where
      options =
        defaultOptions
        { fieldLabelModifier = downcaseHead . drop 3
        }

data ItemResourceHoldsResource = ItemResourceHoldsResource
  { irhrItems    :: ItemResourceHoldsResourceItems
  , irhrNext     :: Maybe Next
  , irhrOffset   :: Offset
  , irhrPrevious :: Maybe Previous
  , irhrTotal    :: Total
  } deriving (Eq, Generic, Show)

instance FromJSON ItemResourceHoldsResource where
  parseJSON irh = genericParseJSON options irh
    where
      options =
        defaultOptions
        { fieldLabelModifier = downcaseHead . drop 4
        }

newtype ItemResourceHoldsResourceItems = ItemResourceHoldsResourceItems
  { irhriItems :: [ItemResourceHoldsResourceItem]
  } deriving (Eq, Generic, Show)

instance FromJSON ItemResourceHoldsResourceItems where
  parseJSON irh = genericParseJSON options irh
    where
      options =
        defaultOptions
        { fieldLabelModifier = downcaseHead . drop 5
        , unwrapUnaryRecords = True
        }

data ItemResourceHoldsResourceItem = ItemResourceHoldsResourceItem
  { irhriResource         :: ItemResourceHoldsResourceItemResource
  , irhriResourceLocation :: Maybe ResponseResourceLocation
  } deriving (Eq, Generic, Show)

instance FromJSON ItemResourceHoldsResourceItem where
  parseJSON irh = genericParseJSON options irh
    where
      options =
        defaultOptions
        { fieldLabelModifier = downcaseHead . drop 5
        }

data ItemResourceHoldsResourceItemResource = ItemResourceHoldsResourceItemResource
  { irhrirAppliedDate     :: AppliedDate
  , irhrirClearedDate     :: Maybe ClearedDate
  , irhrirDescription     :: Description
  , irhrirExternalOrderId :: Maybe ExternalOrderId
  , irhrirId              :: Id
  , irhrirOrderId         :: OrderId
  , irhrirType            :: Type
  } deriving (Eq, Generic, Show)

instance FromJSON ItemResourceHoldsResourceItemResource where
  parseJSON irh = genericParseJSON options irh
    where
      options =
        defaultOptions
        { fieldLabelModifier = downcaseHead . drop 6
        }

type ExternalOrderId = ExternalId

newtype Description = Description
  { unDescription :: Text
  } deriving (Eq, Generic, Show)

instance FromJSON Description where
  parseJSON d = genericParseJSON options d
    where
      options =
        defaultOptions
        { fieldLabelModifier = downcaseHead . drop 2
        , unwrapUnaryRecords = True
        }

type ClearedDate = ExpectedShipDate

type AppliedDate = ExpectedShipDate

newtype ItemStatus = ItemStatus
  { unItemStatus :: Text
  } deriving (Eq, Generic, Show)

newtype CommerceName = CommerceName
  { unCommerceName :: Text
  } deriving (Eq, Generic, Show)

instance FromJSON CommerceName where
  parseJSON cn = genericParseJSON options cn
    where
      options =
        defaultOptions
        { fieldLabelModifier = downcaseHead . drop 2
        , unwrapUnaryRecords = True
        }

type TransactionId = ExternalId

data CreateReceiving = CreateReceiving
  { createReceivingExternalId            :: Maybe ExternalId
  , createReceivingOrderNo               :: Maybe OrderNo
  , createReceivingExpectedDate          :: Maybe ExpectedDateText
  , createReceivingOptions               :: ReceivingOptions
  , createReceivingArrangement           :: ReceivingArrangement
  , createReceivingShipments             :: ReceivingShipments
  , createReceivingLabels                :: Maybe ReceivingLabels
  , createReceivingTrackings             :: Maybe ReceivingTrackings
  , createReceivingItems                 :: ReceivingItems
  , createReceivingShipFrom              :: ReceivingShipFrom
  , crateReceivingInstructionsRecipients :: Maybe ReceivingInstructionsRecipients
  } deriving (Eq, Generic, Show)

instance ToJSON CreateReceiving where
  toJSON r = genericToJSON options r
    where
      options =
        defaultOptions
        { fieldLabelModifier = downcaseHead . drop 15
        , omitNothingFields  = True
        }

-- | The expected format is YYYY-MM-DDThh:mm:ssTZD (ISO8601)
-- "2014-05-27T00:00:00-07:00"
newtype ExpectedDateText = ExpectedDateText
  { unExpectedDate :: Text
  } deriving (Eq, Generic, Show)

instance FromJSON ExpectedDateText where
  parseJSON edt = genericParseJSON options edt
    where
      options =
        defaultOptions
        { fieldLabelModifier = downcaseHead . drop 2
        , unwrapUnaryRecords = True
        }

instance ToJSON ExpectedDateText where
  toJSON ed = genericToJSON options ed
    where
      options =
        defaultOptions
        { unwrapUnaryRecords = True
        }

newtype OrderNo = OrderNo
  { unOrderNo :: Text
  } deriving (Eq, Generic, Show)

instance FromJSON OrderNo where
  parseJSON o = genericParseJSON options o
    where
      options =
        defaultOptions
        { fieldLabelModifier = downcaseHead . drop 2
        , unwrapUnaryRecords = True
        }

instance ToJSON OrderNo where
  toJSON o = genericToJSON options o
    where
      options =
        defaultOptions
        { unwrapUnaryRecords = True
        }

data ReceivingOptions = ReceivingOptions
  { ropWarehouseId         :: Maybe WarehouseId
  , ropWarehouseExternalId :: Maybe WarehouseExternalId
  , ropWarehouseRegion     :: Maybe WarehouseRegion
  } deriving (Eq, Generic, Show)

type WarehouseId = Id

type WarehouseExternalId = ExternalId

instance ToJSON ReceivingOptions where
  toJSON r = genericToJSON options r
    where
      options =
        defaultOptions
        { fieldLabelModifier = downcaseHead . drop 3
        , omitNothingFields  = True
        }

data ReceivingArrangement = ReceivingArrangement
  { rapType    :: ArrangementType
  , rapContact :: Maybe Contact
  , rapPhone   :: Maybe Phone
  } deriving (Eq, Generic, Show)

instance ToJSON ReceivingArrangement where
  toJSON r = genericToJSON options r
    where
      options =
        defaultOptions
        { fieldLabelModifier = downcaseHead . drop 3
        , omitNothingFields  = True
        }

newtype Contact = Contact
  { unContact :: Text
  } deriving (Eq, Generic, Show)

instance FromJSON Contact where
  parseJSON c = genericParseJSON options c
    where
      options =
        defaultOptions
        { fieldLabelModifier = downcaseHead . drop 2
        , unwrapUnaryRecords = True
        }

instance ToJSON Contact where
  toJSON c = genericToJSON options c
    where
      options =
        defaultOptions
        { unwrapUnaryRecords = True
        }

newtype Phone = Phone
  { unPhone :: Text
  } deriving (Eq, Generic, Show)

instance FromJSON Phone where
  parseJSON p = genericParseJSON options p
    where
      options =
        defaultOptions
        { fieldLabelModifier = downcaseHead . drop 2
        , unwrapUnaryRecords = True
        }

instance ToJSON Phone where
  toJSON p = genericToJSON options p
    where
      options =
        defaultOptions
        { unwrapUnaryRecords = True
        }

newtype Type = Type
  { unType :: Text
  } deriving (Eq, Generic, Show)

instance FromJSON Type where
  parseJSON t = genericParseJSON options t
    where
      options =
        defaultOptions
        { fieldLabelModifier = downcaseHead . drop 2
        , unwrapUnaryRecords = True
        }

instance ToJSON Type where
  toJSON t = genericToJSON options t
    where
      options =
        defaultOptions
        { unwrapUnaryRecords = True
        }

data ArrangementType = ArrangementTypeNone
  | ArrangementTypeOverseas
  | ArrangementTypeLabel
  | ArrangementTypePickup
  deriving (Eq, Generic, Show)

instance FromJSON ArrangementType where
  parseJSON = withText "arrangementType" parse
    where
      parse "none"     = pure ArrangementTypeNone
      parse "overseas" = pure ArrangementTypeOverseas
      parse "label"    = pure ArrangementTypeLabel
      parse "pickup"   = pure ArrangementTypePickup
      parse o          = fail ("Unexpected arrangementType value: " <> show o)

instance ToJSON ArrangementType where
  toJSON ArrangementTypeNone = String "none"
  toJSON ArrangementTypeOverseas = String "overseas"
  toJSON ArrangementTypeLabel = String "label"
  toJSON ArrangementTypePickup = String "pickup"

newtype ReceivingShipments = ReceivingShipments
  { rShipments :: [ReceivingShipment]
  } deriving (Eq, Generic, Show)

instance ToJSON ReceivingShipments where
  toJSON r = genericToJSON options r
    where
      options =
        defaultOptions
        { unwrapUnaryRecords = True
        }

data ReceivingShipment = ReceivingShipment
  { rsiLength :: Maybe Length
  , rsiWidth  :: Maybe Width
  , rsiHeight :: Maybe Height
  , rsiWeight :: Maybe Weight
  , rsiType   :: Type
  } deriving (Eq, Generic, Show)

instance ToJSON ReceivingShipment where
  toJSON rs = genericToJSON options rs
    where
      options =
        defaultOptions
        { fieldLabelModifier = downcaseHead . drop 3
        , omitNothingFields  = True
        }

newtype ReceivingLabels = ReceivingLabels
  { rLabels :: [ReceivingLabel]
  } deriving (Eq, Generic, Show)

instance ToJSON ReceivingLabels where
  toJSON r = genericToJSON options r
    where
      options =
        defaultOptions
        { unwrapUnaryRecords = True
        }

data ReceivingLabel = ReceivingLabel
  { rlLabelId         :: Maybe LabelId
  , rlOrderId         :: Maybe OrderId
  , rlOrderExternalId :: Maybe OrderExternalId
  } deriving (Eq, Generic, Show)

instance ToJSON ReceivingLabel where
  toJSON rl = genericToJSON options rl
    where
      options =
        defaultOptions
        { fieldLabelModifier = downcaseHead . drop 2
        , omitNothingFields  = True
        }

newtype ReceivingTrackings = ReceivingTrackings
  { rTrackings :: [ReceivingTracking]
  } deriving (Eq, Generic, Show)

instance ToJSON ReceivingTrackings where
  toJSON r = genericToJSON options r
    where
      options =
        defaultOptions
        { unwrapUnaryRecords = True
        }

data ReceivingTracking = ReceivingTracking
  { rtTracking :: Tracking
  , rtCarrier  :: Maybe CarrierName
  , rtContact  :: Maybe Contact
  , rtPhone    :: Maybe Phone
  } deriving (Eq, Generic, Show)

instance ToJSON ReceivingTracking where
  toJSON rt = genericToJSON options rt
    where
      options =
        defaultOptions
        { fieldLabelModifier = downcaseHead . drop 2
        , omitNothingFields  = True
        }

newtype Tracking = Tracking
  { unTracking :: Text
  } deriving (Eq, Generic, Show)

instance FromJSON Tracking where
  parseJSON t = genericParseJSON options t
    where
      options =
        defaultOptions
        { unwrapUnaryRecords = True
        , fieldLabelModifier = downcaseHead . drop 2
        }

instance ToJSON Tracking where
  toJSON t = genericToJSON options t
    where
      options =
        defaultOptions
        { unwrapUnaryRecords = True
        }

newtype ReceivingItems = ReceivingItems
  { rItems :: [ReceivingItem]
  } deriving (Eq, Generic, Show)

instance ToJSON ReceivingItems where
  toJSON r = genericToJSON options r
    where
      options =
        defaultOptions
        { unwrapUnaryRecords = True
        }

data ReceivingItem = ReceivingItem
  { rSku      :: SKU
  , rQuantity :: Quantity
  } deriving (Eq, Generic, Show)

instance ToJSON ReceivingItem where
  toJSON ri = genericToJSON options ri
    where
      options =
        defaultOptions
        { fieldLabelModifier = downcaseHead . drop 1
        }

data ReceivingShipFrom = ReceivingShipFrom
  { rsfpEmail      :: Maybe Email
  , rsfpName       :: Name
  , rsfpAddress1   :: AddressLine
  , rsfpAddress2   :: Maybe AddressLine
  , rsfpCity       :: City
  , rsfpState      :: State
  , rsfpPostalCode :: PostalCode
  , rsfpCountry    :: Country
  , rsfpPhone      :: Phone
  } deriving (Eq, Generic, Show)

instance ToJSON ReceivingShipFrom where
  toJSON r = genericToJSON options r
    where
      options =
        defaultOptions
        { fieldLabelModifier = downcaseHead . drop 4
        , omitNothingFields  = True
        }

newtype Email = Email
  { unEmail :: Text
  } deriving (Eq, Generic, Show)

instance FromJSON Email where
  parseJSON e = genericParseJSON options e
    where
      options =
        defaultOptions
        { fieldLabelModifier = downcaseHead . drop 2
        , unwrapUnaryRecords = True
        }

instance ToJSON Email where
  toJSON e = genericToJSON options e
    where
      options =
        defaultOptions
        { unwrapUnaryRecords = True
        }

newtype Name = Name
  { unName :: Text
  } deriving (Eq, Generic, Show)

instance FromJSON Name where
  parseJSON n = genericParseJSON options n
    where
      options =
        defaultOptions
        { fieldLabelModifier = downcaseHead . drop 2
        , unwrapUnaryRecords = True
        }

instance ToJSON Name where
  toJSON n = genericToJSON options n
    where
      options =
        defaultOptions
        { unwrapUnaryRecords = True
        }

newtype State = State
  { unState :: Text
  } deriving (Eq, Generic, Show)

instance FromJSON State where
  parseJSON s = genericParseJSON options s
    where
      options =
        defaultOptions
        { fieldLabelModifier = downcaseHead . drop 2
        , unwrapUnaryRecords = True
        }

instance ToJSON State where
  toJSON s = genericToJSON options s
    where
      options =
        defaultOptions
        { unwrapUnaryRecords = True
        }

newtype ReceivingInstructionsRecipients = ReceivingInstructionsRecipients
  { rirsInstructionsRecipients :: [ReceivingInstructionsRecipient]
  } deriving (Eq, Generic, Show)

instance ToJSON ReceivingInstructionsRecipients where
  toJSON r = genericToJSON options r
    where
      options =
        defaultOptions
        { unwrapUnaryRecords = True
        }

newtype Note = Note
  { unNote :: Text
  } deriving (Eq, Generic, Show)

instance FromJSON Note where
  parseJSON n = genericParseJSON options n
    where
      options =
        defaultOptions
        { fieldLabelModifier = downcaseHead . drop 2
        , unwrapUnaryRecords = True
        }

instance ToJSON Note where
  toJSON n = genericToJSON options n
    where
      options =
        defaultOptions
        { unwrapUnaryRecords = True
        }

data ReceivingInstructionsRecipient = ReceivingInstructionsRecipient
  { rirEmail :: Email
  , rirName  :: Maybe Name
  , rirNote  :: Maybe Note
  } deriving (Eq, Generic, Show)

instance ToJSON ReceivingInstructionsRecipient where
  toJSON rip = genericToJSON options rip
    where
      options =
        defaultOptions
        { fieldLabelModifier = downcaseHead . drop 3
        , omitNothingFields  = True
        }

