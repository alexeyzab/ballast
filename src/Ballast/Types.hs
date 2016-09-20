{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}

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
  , AddressLine(..)
  , City(..)
  , PostalCode(..)
  , Region(..)
  , Country(..)
  , IsCommercial(..)
  , IsPoBox(..)
  , Rates(..)
  , ServiceOptions(..)
  , RateResource(..)
  , ShipWireRequest(..)
  , RateRequest
  , StockRequest
  , mkShipWireRequest
  , ShipWireReturn
  , defaultGetRate
  , Reply
  , Method
  ) where

import           Data.Aeson
import           Data.Aeson.Types
import           Data.ByteString            (ByteString)
import qualified Data.ByteString.Lazy.Char8 as BSL
import qualified Data.Char                  as DC
import           Data.Fixed
import           Data.Monoid                ((<>))
import           Data.Text                  (Text)
import qualified Data.Text                  as T
import           Data.Time.Clock            (UTCTime)
import           GHC.Generics
import           Network.HTTP.Client
import qualified Network.HTTP.Types.Method  as NHTM

-- | Username type used for HTTP Basic authentication.
newtype Username = Username
  { username :: ByteString
  } deriving (Read, Show, Eq)

-- | Password type used for HTTP Basic authentication.
newtype Password = Password
  { password :: ByteString
  } deriving (Read, Show, Eq)

---------------------------------------------------------------------
-- Rate Endpoint -- https://www.shipwire.com/w/developers/rate/
---------------------------------------------------------------------

newtype SKU = SKU
  { sku :: Text
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
  , rateOptionCanSplit      :: Integer
  , rateOptionWarehouseArea :: WarehouseArea
  , rateOptionChannelName   :: Maybe Text
  } deriving (Eq, Generic, Show)

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

type Quantity = Integer

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
defaultItems = [ItemInfo ((SKU "Ballasttest"), 1)]

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
  { addressLine :: Text
  } deriving (Eq, Generic, Show)

instance ToJSON AddressLine where
  toJSON address = genericToJSON options address
    where
      options =
        defaultOptions
        { unwrapUnaryRecords = True
        }

newtype City = City
  { city :: Text
  } deriving (Eq, Generic, Show)

instance ToJSON City where
  toJSON city = genericToJSON options city
    where
      options =
        defaultOptions
        { unwrapUnaryRecords = True
        }

newtype PostalCode = PostalCode
  { postalCode :: Text
  } deriving (Eq, Generic, Show)

instance ToJSON PostalCode where
  toJSON code = genericToJSON options code
    where
      options =
        defaultOptions
        { unwrapUnaryRecords = True
        }

newtype Region = Region
  { region :: Text
  } deriving (Eq, Generic, Show)

instance ToJSON Region where
  toJSON region = genericToJSON options region
    where
      options =
        defaultOptions
        { unwrapUnaryRecords = True
        }

newtype Country = Country
  { country :: Text
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
defaultRateOptions = RateOptions USD GroupByAll 1 WarehouseAreaUS Nothing

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

defaultRateResponse :: IO StockResponse
defaultRateResponse = do
  file <- BSL.readFile "test5.json"
  let decoded = eitherDecode file
  case decoded of
    Right info -> return info
    Left err -> error err

data RateResponse = RateResponse
  { rateResponseStatus           :: Integer
  , rateResponseMessage          :: Text
  , rateResponseWarnings         :: Maybe [Warnings]
  , rateResponseErrors           :: Maybe [Errors]
  , rateResponseResourceLocation :: Maybe Text
  , rateResponseResource         :: Maybe RateResource
  } deriving (Eq, Generic, Show)

instance FromJSON RateResponse where
  parseJSON rr = genericParseJSON options rr
    where
      options =
        defaultOptions
        { fieldLabelModifier = downcaseHead . drop 12
        }

data Warnings = Warnings
  { warningsCode    :: Text
  , warningsMessage :: Text
  , warningsType    :: WarningType
  } deriving (Eq, Generic, Show)

data WarningType
  = WarningsWarning
  | WarningsError
  deriving (Eq, Generic, Show)

instance FromJSON WarningType where
  parseJSON = withText "warningType" parse
    where
      parse "warning" = pure WarningsWarning
      parse "error" = pure WarningsError
      parse o = fail ("Unexpected warningType value: " <> show o)

instance FromJSON Warnings where
  parseJSON w = genericParseJSON options w
    where
      options =
        defaultOptions
        { fieldLabelModifier = downcaseHead . drop 8
        }

data Errors = Errors
  { errorsCode    :: Text
  , errorsMessage :: Text
  , errorsType    :: ErrorType
  } deriving (Eq, Generic, Show)

data ErrorType
  = ErrorsWarning
  | ErrorsError
  deriving (Eq, Generic, Show)

instance FromJSON ErrorType where
  parseJSON = withText "errorType" parse
    where
      parse "warning" = pure ErrorsWarning
      parse "error" = pure ErrorsError
      parse o = fail ("Unexpected errorType value: " <> show o)

instance FromJSON Errors where
  parseJSON w = genericParseJSON options w
    where
      options =
        defaultOptions
        { fieldLabelModifier = downcaseHead . drop 6
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
  , sOptsGroupId         :: Maybe Integer
  , sOptsGroupExternalId :: Maybe Text
  } deriving (Eq, Generic, Show)

instance FromJSON ServiceOptions where
  parseJSON sop = genericParseJSON options sop
    where
      options =
        defaultOptions
        { fieldLabelModifier = downcaseHead . drop 5
        }

data ServiceOption = ServiceOption
  { sOptServiceLevelCode :: ServiceLevelCode
  , sOptServiceLevelName :: Text
  , sOptShipments        :: [Shipment]
  } deriving (Eq, Generic, Show)

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
  { shipmentWarehouseName           :: Text
  , shipmentCarrier                 :: Carrier
  , shipmentCost                    :: Cost
  , shipmentSubtotals               :: [Subtotal]
  , shipmentPieces                  :: [Piece]
  , shipmentExpectedShipDate        :: Maybe UTCTime
  , shipmentExpectedDeliveryDateMin :: Maybe UTCTime
  , shipmentExpectedDeliveryDateMax :: Maybe UTCTime
  } deriving (Eq, Generic, Show)

instance FromJSON Shipment where
  parseJSON sh = genericParseJSON options sh
    where
      options =
        defaultOptions
        { fieldLabelModifier = downcaseHead . drop 8
        }

data Carrier = Carrier
  { carrierCode       :: Text
  , carrierName       :: Text
  , carrierProperties :: [Text]
  } deriving (Eq, Generic, Show)

instance FromJSON Carrier where
  parseJSON car = genericParseJSON options car
    where
      options =
        defaultOptions
        { fieldLabelModifier = downcaseHead . drop 7
        }

data Cost = Cost
  { costCurrency         :: Text
  , costType             :: Text
  , costName             :: Text
  , costAmount           :: Centi
  , costConverted        :: Bool
  , costOriginalCost     :: Centi
  , costOriginalCurrency :: Text
  } deriving (Eq, Generic, Show)

instance FromJSON Cost where
  parseJSON cost = genericParseJSON options cost
    where
      options =
        defaultOptions
        { fieldLabelModifier = downcaseHead . drop 4
        }

data Subtotal = Subtotal
  { subtotalCurrency         :: Text
  , sbutotalType             :: Text
  , subtotalName             :: Text
  , subtotalAmount           :: Centi
  , subtotalConverted        :: Bool
  , subtotalOriginalCost     :: Centi
  , subtotalOriginalCurrency :: Text
  } deriving (Eq, Generic, Show)

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
  { pieceLengthAmount :: Integer
  , pieceLengthUnits  :: Text
  } deriving (Eq, Generic, Show)

instance FromJSON PieceLength where
  parseJSON pl = genericParseJSON options pl
    where
      options =
        defaultOptions
        { fieldLabelModifier = downcaseHead . drop 11
        }

data PieceWidth = PieceWidth
  { pieceWidthAmount :: Integer
  , pieceWidthUnits  :: Text
  } deriving (Eq, Generic, Show)

instance FromJSON PieceWidth where
  parseJSON pw = genericParseJSON options pw
    where
      options =
        defaultOptions
        { fieldLabelModifier = downcaseHead . drop 10
        }

data PieceHeight = PieceHeight
  { pieceHeightAmount :: Integer
  , pieceHeightUnits  :: Text
  } deriving (Eq, Generic, Show)

instance FromJSON PieceHeight where
  parseJSON ph = genericParseJSON options ph
    where
      options =
        defaultOptions
        { fieldLabelModifier = downcaseHead . drop 11
        }

data PieceWeight = PieceWeight
  { pieceWeightAmount :: Double
  , pieceWeightUnits  :: Text
  , pieceWeightType   :: Text
  } deriving (Eq, Generic, Show)

instance FromJSON PieceWeight where
  parseJSON pw = genericParseJSON options pw
    where
      options =
        defaultOptions
        { fieldLabelModifier = downcaseHead . drop 11
        }

data PieceSubWeight = PieceSubWeight
  { pieceSubWeightAmount :: Double
  , pieceSubWeightUnits  :: Text
  , pieceSubWeightType   :: Text
  } deriving (Eq, Generic, Show)

instance FromJSON PieceSubWeight where
  parseJSON psw = genericParseJSON options psw
    where
      options =
        defaultOptions
        { fieldLabelModifier = downcaseHead . drop 14
        }

data PieceContent = PieceContent
  { pieceContentSku      :: Text
  , pieceContentQuantity :: Integer
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

data ShipWireRequest a = ShipWireRequest
  { rMethod  :: Method -- ^ Method of ShipWireRequest
  , endpoint :: Text -- ^ Endpoint of ShipWireRequest
  , body     :: Maybe BSL.ByteString -- ^ Request body of ShipWireRequest
  }

mkShipWireRequest :: Method -> Text -> Maybe BSL.ByteString -> ShipWireRequest a
mkShipWireRequest m e b = ShipWireRequest m e b

type family ShipWireReturn a :: *

data RateRequest
type instance ShipWireReturn RateRequest = RateResponse

---------------------------------------------------------------------
-- Stock Endpoint -- https://www.shipwire.com/w/developers/stock/
---------------------------------------------------------------------

data StockRequest
type instance ShipWireReturn StockRequest = StockResponse

data StockResponse = StockResponse
  { stockResponseStatus           :: Integer
  , stockResponseMessage          :: Text
  , stockResponseWarnings         :: Maybe [Warnings]
  , stockResponseErrors           :: Maybe [Errors]
  , stockResponseResourceLocation :: Maybe Text
  , stockResponseResource         :: StockResource
  } deriving (Eq, Generic, Show)

instance FromJSON StockResponse where
  parseJSON sr = genericParseJSON options sr
    where
      options =
        defaultOptions
        { fieldLabelModifier = downcaseHead . drop 13
        }

data StockResource = StockResource
  { stockResponseOffset   :: Integer
  , stockResponseTotal    :: Integer
  , stockResponsePrevious :: Maybe Integer
  , stockResponseNext     :: Maybe Integer
  , stockResponseItems    :: [StockItem]
  } deriving (Eq, Generic, Show)

instance FromJSON StockResource where
  parseJSON sr = genericParseJSON options sr
    where
      options =
        defaultOptions
        { fieldLabelModifier = downcaseHead . drop 13
        }

data StockItem = StockItem
  { stockItemResourceLocation :: Maybe Text
  , stockItemResource         :: StockItemResource
  } deriving (Eq, Generic, Show)

instance FromJSON StockItem where
  parseJSON si = genericParseJSON options si
    where
      options =
        defaultOptions
        { fieldLabelModifier = downcaseHead . drop 9
        }

data StockItemResource = StockItemResource
  { sirProductId           :: Integer
  , sirProductExternalId   :: Maybe Integer
  , sirSku                 :: Text
  , sirIsBundle            :: IsBundle
  , sirIsAlias             :: IsAlias
  , sirWarehouseRegion     :: Text
  , sirWarehouseId         :: Integer
  , sirWarehouseExternalId :: Maybe Integer
  , sirPending             :: Integer
  , sirGood                :: Integer
  , sirReserved            :: Integer
  , sirBackordered         :: Integer
  , sirShipping            :: Integer
  , sirShipped             :: Integer
  , sirCreating            :: Integer
  , sirConsuming           :: Integer
  , sirConsumed            :: Integer
  , sirCreated             :: Integer
  , sirDamaged             :: Integer
  , sirReturned            :: Integer
  , sirInreview            :: Integer
  , sirAvailableDate       :: Maybe UTCTime
  , sirShippedLastDay      :: Integer
  , sirShippedLastWeek     :: Integer
  , sirShippedLast4Weeks   :: Integer
  , sirOrderedLastDay      :: Integer
  , sirOrderedLastWeek     :: Integer
  , sirOrderedLast4Weeks   :: Integer
  } deriving (Eq, Generic, Show)

instance FromJSON StockItemResource where
  parseJSON sir = genericParseJSON options sir
    where
      options =
        defaultOptions
        { fieldLabelModifier = downcaseHead . drop 3
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
