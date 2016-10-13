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
  , ParentId
  , ProductIdParam
  , ProductExternalIdParam
  , WarehouseIdParam
  , WarehouseExternalIdParam
  , WarehouseRegionParam
  , WarehouseAreaParam
  , ChannelName
  , IncludeEmpty
  , VendorIdParam
  , VendorExternalIdParam
  , DisableAutoBreakLots
  , Mode(..)
  , IncludeEmptyShipwireAnywhere
  , Offset
  , Total
  , Previous
  , Next
  , Limit
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
  , sOptsGroupId         :: Maybe Id
  , sOptsGroupExternalId :: Maybe ExternalId
  } deriving (Eq, Generic, Show)

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

newtype ExpectedDeliveryDateMin = ExpectedDeliveryDateMin
  { unExpectedDeliveryDateMin :: UTCTime
  } deriving (Eq, Generic, Show)

instance FromJSON ExpectedDeliveryDateMin where
  parseJSON edm = genericParseJSON options edm
    where
      options =
        defaultOptions
        { unwrapUnaryRecords = True
        }

newtype ExpectedDeliveryDateMax = ExpectedDeliveryDateMax
  { unExpectedDeliveryDateMax :: UTCTime
  } deriving (Eq, Generic, Show)

instance FromJSON ExpectedDeliveryDateMax where
  parseJSON edx = genericParseJSON options edx
    where
      options =
        defaultOptions
        { unwrapUnaryRecords = True
        }

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
  { offset :: Integer
  } deriving (Eq, Generic, Show)

newtype Total = Total
  { total :: Integer
  } deriving (Eq, Generic, Show)

newtype Previous = Previous
  { previous :: Text
  } deriving (Eq, Generic, Show)

newtype Next = Next
  { next :: Text
  } deriving (Eq, Generic, Show)

newtype Limit = Limit
  { limit :: Integer
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
  { stockItemResourceLocation :: Maybe ItemResourceLocation
  , stockItemResource         :: StockItemResource
  } deriving (Eq, Generic, Show)

newtype ItemResourceLocation = ItemResourceLocation
  { unItemResourceLocation :: Text
  } deriving (Eq, Generic, Show)

instance FromJSON ItemResourceLocation where
  parseJSON irl = genericParseJSON options irl
    where
      options =
        defaultOptions
        { unwrapUnaryRecords = True
        }

instance FromJSON StockItem where
  parseJSON si = genericParseJSON options si
    where
      options =
        defaultOptions
        { fieldLabelModifier = downcaseHead . drop 9
        , omitNothingFields  = True
        }

data StockItemResource = StockItemResource
  { sirProductId           :: Id
  , sirProductExternalId   :: Maybe Id
  , sirSku                 :: SKU
  , sirIsBundle            :: IsBundle
  , sirIsAlias             :: IsAlias
  , sirWarehouseRegion     :: WarehouseRegion
  , sirWarehouseId         :: Id
  , sirWarehouseExternalId :: Maybe ExternalId
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
  , sirInreview            :: StockItemResourceInterview
  , sirAvailableDate       :: Maybe StockItemResourceAvailableDate
  , sirShippedLastDay      :: StockItemResourceShippedLastDay
  , sirShippedLastWeek     :: StockItemResourceShippedLastWeek
  , sirShippedLast4Weeks   :: StockItemResourceShippedLast4Weeks
  , sirOrderedLastDay      :: StockItemResourceOrderedLastDay
  , sirOrderedLastWeek     :: StockItemResourceOrderedLastWeek
  , sirOrderedLast4Weeks   :: StockItemResourceOrderedLast4Weeks
  } deriving (Eq, Generic, Show)

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

newtype StockItemResourceInterview = StockItemResourceInterview
  { unStockItemResourceInterview :: Integer
  } deriving (Eq, Generic, Show)

instance FromJSON StockItemResourceInterview where
  parseJSON i = genericParseJSON options i
    where
      options =
        defaultOptions
        { unwrapUnaryRecords = True
        }

newtype StockItemResourceAvailableDate = StockItemResourceAvailableDate
  { unStockItemREsourceAvailableDate :: UTCTime
  } deriving (Eq, Generic, Show)

instance FromJSON StockItemResourceAvailableDate where
  parseJSON ad = genericParseJSON options ad
    where
      options =
        defaultOptions
        { unwrapUnaryRecords = True
        }

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

-- | Find the query parameters froom the list of
-- [Params TupleBS8 BSL.ByteString]
filterQuery :: [Params (BS8.ByteString, BS8.ByteString) c] -> [(BS8.ByteString, BS8.ByteString)]
filterQuery [] = []
filterQuery xs = [b | Query b <- xs]

-------------------------------------------------------------------------
-- Receiving Endpoint -- https://www.shipwire.com/w/developers/receiving
-------------------------------------------------------------------------

data CreateReceiving = CreateReceiving
  { createReceivingExternalId            :: Maybe ExternalId
  , createReceivingOrderNo               :: Maybe OrderNo
  , createReceivingExpectedDate          :: Maybe ExpectedDate
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

-- | The expected format is YYYY-MM-DDThh:mm:ssTZD
-- "2014-05-27T00:00:00-07:00"
newtype ExpectedDate = ExpectedDate
  { unExpectedDate :: Text
  } deriving (Eq, Generic, Show)

instance ToJSON ExpectedDate where
  toJSON ed = genericToJSON options ed
    where
      options =
        defaultOptions
        { unwrapUnaryRecords = True
        }

newtype OrderNo = OrderNo
  { unOrderNo :: Text
  } deriving (Eq, Generic, Show)

instance ToJSON OrderNo where
  toJSON o = genericToJSON options o
    where
      options =
        defaultOptions
        { unwrapUnaryRecords = True
        }

data ReceivingOptions = ReceivingOptions
  { ropWarehouseId         :: Maybe Id
  , ropWarehouseExternalId :: Maybe ExternalId
  , ropWarehouseRegion     :: Maybe WarehouseRegion
  } deriving (Eq, Generic, Show)

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
  { rlLabelId         :: Maybe Id
  , rlOrderId         :: Maybe Id
  , rlOrderExternalId :: Maybe ExternalId
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

instance ToJSON State where
  toJSON s = genericToJSON options s
    where
      options =
        defaultOptions
        { unwrapUnaryRecords = True
        }

newtype ReceivingInstructionsRecipients = ReceivingInstructionsRecipients
  { rirInstructionsRecipients :: [ReceivingInstructionsRecipient]
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

