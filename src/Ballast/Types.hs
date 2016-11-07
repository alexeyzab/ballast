{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TypeFamilies               #-}

module Ballast.Types
  ( Username(..)
  , Password(..)
  , SKU(..)
  , mkSku
  , GetRate(..)
  , RateOptions(..)
  , CanSplit(..)
  , RateOrder(..)
  , ShipTo(..)
  , IsCommercial(..)
  , IsPoBox(..)
  , Items
  , ItemInfo(..)
  , Quantity(..)
  , AddressLine(..)
  , City(..)
  , PostalCode(..)
  , Region(..)
  , Country(..)
  , Currency(..)
  , GroupBy(..)  
  , WarehouseArea(..)
  , RateResponse(..)
  , ResponseStatus(..)
  , ResponseMessage(..)
  , ResponseWarnings(..)
  , ResponseErrors(..)
  , ResponseResourceLocation(..)
  , Warning(..)
  , WarningCode(..)
  , WarningMessage(..)
  , WarningType(..)
  , Error(..)
  , ErrorCode(..)
  , ErrorMessage(..)
  , ErrorType(..)
  , RateResource(..)
  , Rates(..)
  , ServiceOptions(..)
  , GroupId
  , GroupExternalId
  , Id(..)
  , ExternalId(..)
  , ServiceOption(..)
  , ServiceOptionServiceLevelName(..)
  , ServiceLevelCode(..)
  , Shipment(..)
  , WarehouseName(..)
  , ExpectedShipDate(..)
  , ExpectedDeliveryDateMin
  , ExpectedDeliveryDateMax
  , Carrier(..)
  , CarrierCode(..)
  , CarrierName(..)
  , CarrierProperties(..)
  , Cost(..)
  , CostCurrency(..)
  , CostType(..)
  , CostName(..)
  , CostAmount(..)
  , CostConverted(..)
  , CostOriginalCost(..)
  , CostOriginalCurrency(..)
  , Subtotal(..)
  , SubtotalCurrency(..)
  , SubtotalType(..)
  , SubtotalName(..)
  , SubtotalAmount(..)
  , SubtotalConverted(..)
  , SubtotalOriginalCost(..)
  , SubtotalOriginalCurrency(..)
  , Piece(..)
  , PieceLength(..)
  , Length(..)
  , PieceLengthUnits(..)
  , PieceWidth(..)
  , Width(..)
  , PieceWidthUnits(..)
  , PieceHeight(..)
  , Height(..)
  , PieceHeightUnits(..)
  , PieceWeight(..)
  , Weight(..)
  , PieceWeightUnits(..)
  , PieceWeightType(..)
  , PieceSubWeight(..)
  , PieceSubWeightUnits(..)
  , PieceSubWeightType(..)
  , PieceContent(..)
  , Reply
  , Method
  , ShipwireRequest(..)
  , mkShipwireRequest
  , ShipwireReturn
  , RateRequest
  , StockRequest
  , StockResponse(..)
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
  , modeToBS8
  , IncludeEmptyShipwireAnywhere(..)
  , Offset(..)
  , Total(..)
  , Previous(..)
  , Next(..)
  , Limit(..)
  , StockResource(..)
  , ResponseOffset(..)
  , ResponseTotal(..)
  , ResponsePrevious(..)
  , ResponseNext(..)
  , StockItem(..)
  , StockItemResource(..)
  , ProductId
  , ProductExternalId
  , WarehouseRegion(..)
  , StockItemResourcePending(..)
  , StockItemResourceGood(..)
  , StockItemResourceReserved(..)
  , StockItemResourceBackordered(..)
  , StockItemResourceShipping(..)
  , StockItemResourceShipped(..)
  , StockItemResourceCreating(..)
  , StockItemResourceConsuming(..)
  , StockItemResourceConsumed(..)
  , StockItemResourceCreated(..)
  , StockItemResourceDamaged(..)
  , StockItemResourceReturned(..)
  , StockItemResourceInReview(..)
  , StockItemResourceAvailableDate
  , StockItemResourceShippedLastDay(..)
  , StockItemResourceShippedLastWeek(..)
  , StockItemResourceShippedLast4Weeks(..)
  , StockItemResourceOrderedLastDay(..)
  , StockItemResourceOrderedLastWeek(..)
  , StockItemResourceOrderedLast4Weeks(..)
  , IsBundle(..)
  , IsAlias(..)
  , Host
  , ShipwireHost(..)
  , hostUri
  , ShipwireConfig(..)
  , prodEnvConfig
  , sandboxEnvConfig
  , Params(..)
  , TupleBS8
  , (-&-)
  , filterBody
  , filterQuery
  , CreateReceivingRequest
  , GetReceivingsRequest
  , UpdatedAfter(..)
  , StatusParams(..)
  , StatusParam(..)
  , statusParamToTx
  , OrderNoParam(..)
  , OrderIdParam(..)
  , ExternalIdParam(..)
  , TransactionIdParam(..)
  , ExpandParamReceivings(..)
  , ExpandReceivings(..)
  , expandReceivingsToTx
  , CommerceNameParam(..)
  , CreateReceivingResponse
  , GetReceivingsResponse
  , ReceivingsResponse(..)
  , ReceivingsResource(..)
  , ReceivingsItems(..)
  , ReceivingsItem(..)
  , ReceivingsItemResource(..)
  , ItemResourceInstructionsRecipients(..)
  , ItemResourceInstructionsRecipientsResource(..)
  , ItemResourceInstructionsRecipientsResourceItems(..)
  , ItemResourceInstructionsRecipientsItem(..)
  , ItemResourceInstructionsRecipientsItemResource(..)
  , ItemResourceRouting(..)
  , ItemResourceRoutingResource(..)
  , Latitude(..)
  , Longitude
  , ExpectedDateUTCTime
  , LastUpdatedDate
  , ItemResourceEvents(..)
  , ItemResourceEventsResource(..)
  , CreatedDate
  , PickedUpDate
  , SubmittedDate
  , ProcessedDate
  , CompletedDate
  , CancelledDate
  , ReturnedDate
  , LastManualUpdateDate
  , ItemResourceShipFrom(..)
  , ItemResourceShipFromResource(..)
  , ItemResourceArrangement(..)
  , ItemResourceArrangementResource(..)
  , ItemResourceOptions(..)
  , ItemResourceOptionsResource(..)
  , ItemResourceLabels(..)
  , ItemResourceLabelsResource(..)
  , ItemResourceLabelsResourceItems(..)
  , ItemResourceLabelsResourceItem(..)
  , ItemResourceLabelsResourceItemResource(..)
  , LabelId
  , OrderId
  , OrderExternalId
  , ItemResourceStatus(..)
  , ItemResourceShipments(..)
  , ItemResourceShipmentsResource(..)
  , ItemResourceShipmentsResourceItems(..)
  , ItemResourceShipmentsResourceItem(..)
  , ItemResourceShipmentsResourceItemResource(..)
  , ShipmentId
  , TextHeight(..)
  , TextLength(..)
  , TextWeight(..)
  , TextWidth(..)
  , ItemResourceTrackings(..)
  , ItemResourceTrackingsResource(..)
  , ItemResourceTrackingsResourceItems(..)
  , ItemResourceTrackingsResourceItem(..)
  , ItemResourceTrackingsResourceItemResource(..)
  , TrackedDate
  , DeliveredDate
  , Summary(..)
  , SummaryDate
  , URL(..)
  , ItemResourceItems(..)
  , ItemResourceItemsResource(..)
  , ItemResourceItemsResourceItems(..)
  , ItemResourceItemsResourceItem(..)
  , ItemResourceItemsResourceItemResource(..)
  , Expected(..)
  , Pending
  , Good
  , InReview
  , ItemResourceHolds(..)
  , ItemResourceHoldsResource(..)
  , ItemResourceHoldsResourceItems(..)
  , ItemResourceHoldsResourceItem(..)
  , ItemResourceHoldsResourceItemResource(..)
  , ExternalOrderId
  , Description(..)
  , ClearedDate
  , AppliedDate
  , ItemStatus(..)
  , CommerceName(..)
  , TransactionId
  , CreateReceiving(..)
  , ExpectedDateText(..)
  , OrderNo(..)
  , ReceivingOptions(..)
  , WarehouseId
  , WarehouseExternalId
  , ReceivingArrangement(..)
  , Contact(..)
  , Phone(..)
  , Type(..)
  , ArrangementType(..)
  , ReceivingShipments(..)
  , ReceivingShipment(..)
  , ReceivingLabels(..)
  , ReceivingLabel(..)
  , ReceivingTrackings(..)
  , ReceivingTracking(..)
  , Tracking(..)
  , ReceivingItems(..)
  , ReceivingItem(..)
  , ReceivingShipFrom(..)
  , Email(..)
  , Name(..)
  , State(..)
  , ReceivingInstructionsRecipients(..)
  , Note(..)
  , ReceivingInstructionsRecipient(..)
  , GetReceivingRequest
  , ReceivingResponse(..)
  , ReceivingId(..)
  , getReceivingId
  ) where

import           Data.Aeson
import qualified Data.ByteString.Char8      as BS8
import qualified Data.ByteString.Lazy.Char8 as BSL
import           Data.Fixed
import           Data.Monoid                ((<>))
import           Data.Text                  (Text)
import qualified Data.Text                  as T
import qualified Data.Text.Encoding         as TE
import           Data.Time.Clock            (UTCTime)
import qualified Data.Vector                        as V
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

---------------------------------------------------------------------
-- Rate Endpoint -- https://www.shipwire.com/w/developers/rate/
---------------------------------------------------------------------

data RateRequest
type instance ShipwireReturn RateRequest = RateResponse

newtype SKU = SKU
  { unSku :: Text
  } deriving (Eq, Show, ToJSON, FromJSON)

mkSku :: Text -> Maybe SKU
mkSku sku
  | T.length sku > 16 = Nothing
  | T.length sku < 1 = Nothing
  | otherwise = Just (SKU sku)

-- max 16 characters
-- haskellbookskuty
data GetRate = GetRate
  { rateOptions :: RateOptions
  , rateOrder   :: RateOrder
  } deriving (Eq, Show)

instance ToJSON GetRate where
  toJSON GetRate {..} = object ["options" .= rateOptions
                               ,"order"   .= rateOrder]

data RateOptions = RateOptions
  { rateOptionCurrency      :: Currency
  , rateOptionGroupBy       :: GroupBy
  , rateOptionCanSplit      :: CanSplit
  , rateOptionWarehouseArea :: WarehouseArea
  , rateOptionChannelName   :: Maybe ChannelName
  } deriving (Eq, Show)

instance ToJSON RateOptions where
  toJSON RateOptions {..} = omitNulls ["currency"      .= rateOptionCurrency
                                      ,"groupBy"       .= rateOptionGroupBy
                                      ,"canSplit"      .= rateOptionCanSplit
                                      ,"warehouseArea" .= rateOptionWarehouseArea
                                      ,"channelName"   .= rateOptionChannelName]
                                                                            
newtype CanSplit = CanSplit
  { unCanSplit :: Integer
  } deriving (Eq, Show, ToJSON)

data RateOrder = RateOrder
  { rateOrderShipTo :: ShipTo
  , rateOrderItems  :: Items
  } deriving (Eq, Show)

instance ToJSON RateOrder where
  toJSON RateOrder {..} = object ["shipTo" .= rateOrderShipTo
                                 ,"items"  .= rateOrderItems]

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
  } deriving (Eq, Show)

instance ToJSON ShipTo where
  toJSON ShipTo {..} = object ["address1"     .= shipToAddress1
                              ,"address2"     .= shipToAddress2
                              ,"address3"     .= shipToAddress3
                              ,"city"         .= shipToCity
                              ,"postalCode"   .= shipToPostalCode
                              ,"region"       .= shipToRegion
                              ,"country"      .= shipToCountry
                              ,"isCommercial" .= shipToIsCommercial
                              ,"isPoBox"      .= shipToIsPoBox]

data IsCommercial
  = Commercial
  | NotCommercial
  deriving (Eq, Show)

instance ToJSON IsCommercial where
  toJSON Commercial    = Number 1
  toJSON NotCommercial = Number 0

data IsPoBox
  = PoBox
  | NotPoBox
  deriving (Eq, Show)

instance ToJSON IsPoBox where
  toJSON PoBox    = Number 1
  toJSON NotPoBox = Number 0

type Items = [ItemInfo]

data ItemInfo =
  ItemInfo (SKU, Quantity)
  deriving (Eq, Show)

instance ToJSON ItemInfo where
  toJSON (ItemInfo (sku, q)) = object ["sku"      .= sku
                                      ,"quantity" .= q]

newtype Quantity = Quantity
  { unQuantity :: Integer
  } deriving (Eq, Show, ToJSON, FromJSON)

-- defaultGetRate :: GetRate
-- defaultGetRate = GetRate defaultRateOptions defaultRateOrder

-- defaultRateOrder :: RateOrder
-- defaultRateOrder = RateOrder defaultShipTo defaultItems

-- defaultItems :: Items
-- defaultItems = [ItemInfo ((SKU "Ballasttest"), Quantity 1)]

-- defaultShipTo :: ShipTo
-- defaultShipTo =
--   ShipTo
--     (AddressLine "6501 Railroad Avenue SE")
--     (AddressLine "Room 315")
--     (AddressLine "")
--     (City "Snoqualmie")
--     (PostalCode "85283")
--     (Region "WA")
--     (Country "US")
--     Commercial
--     NotPoBox

newtype AddressLine = AddressLine
  { unAddressLine :: Text
  } deriving (Eq, Show, ToJSON, FromJSON)

newtype City = City
  { unCity :: Text
  } deriving (Eq, Show, ToJSON, FromJSON)

newtype PostalCode = PostalCode
  { unPostalCode :: Text
  } deriving (Eq, Show, ToJSON, FromJSON)

newtype Region = Region
  { unRegion :: Text
  } deriving (Eq, Show, ToJSON)

newtype Country = Country
  { unCountry :: Text
  } deriving (Eq, Show, ToJSON, FromJSON)

-- downcaseHead :: [Char] -> [Char]
-- downcaseHead [] = []
-- downcaseHead (x:xs) = (DC.toLower x) : xs

-- defaultRateOptions :: RateOptions
-- defaultRateOptions = RateOptions USD GroupByAll (CanSplit 1) WarehouseAreaUS Nothing

data Currency =
  USD
  deriving (Eq, Show)

instance ToJSON Currency where
  toJSON USD = String "USD"

omitNulls :: [(Text, Value)] -> Value
omitNulls = object . filter notNull
  where
    notNull (_, Null) = False
    notNull (_, Array a) = (not . V.null) a
    notNull _ = True

data GroupBy
  = GroupByAll
  | GroupByWarehouse
  deriving (Eq, Show)

instance ToJSON GroupBy where
  toJSON GroupByAll       = String "all"
  toJSON GroupByWarehouse = String "warehouse"

instance FromJSON GroupBy where
  parseJSON = withText "groupBy" parse
    where
      parse "all"       = pure GroupByAll
      parse "warehouse" = pure GroupByWarehouse
      parse o           = fail ("Unexpected groupBy value: " <> show o)

data WarehouseArea =
  WarehouseAreaUS
  deriving (Eq, Show)

instance ToJSON WarehouseArea where
  toJSON WarehouseAreaUS = String "US"

-- defaultRateResponse :: IO RateResponse
-- defaultRateResponse = do
--   file <- BSL.readFile "rateresponse.json"
--   let decoded = eitherDecode file
--   case decoded of
--     Right info -> return info
--     Left err -> error err

data RateResponse = RateResponse
  { rateResponseStatus           :: ResponseStatus
  , rateResponseMessage          :: ResponseMessage
  , rateResponseWarnings         :: Maybe ResponseWarnings
  , rateResponseErrors           :: Maybe ResponseErrors
  , rateResponseResourceLocation :: Maybe ResponseResourceLocation
  , rateResponseResource         :: Maybe RateResource
  } deriving (Eq, Show)

instance FromJSON RateResponse where
  parseJSON = withObject "RateResponse" parse
    where
      parse o = RateResponse
                <$> o .:  "status"
                <*> o .:  "message"
                <*> o .:? "warnings"
                <*> o .:? "errors"
                <*> o .:? "resourceLocation"
                <*> o .:? "resource"

newtype ResponseStatus = ResponseStatus
  { unResponseStatus :: Integer
  } deriving (Eq, Show, FromJSON)

newtype ResponseMessage = ResponseMessage
  { unResponseMessage :: Text
  } deriving (Eq, Show, FromJSON)

newtype ResponseWarnings = ResponseWarnings
  { unResponseWarnings :: [Warning]
  } deriving (Eq, Show, FromJSON)

newtype ResponseErrors = ResponseErrors
  { unResponseErrors :: [Error]
  } deriving (Eq, Show, FromJSON)

newtype ResponseResourceLocation = ResponseResourceLocation
  { unResponseResourceLocation :: Text
  } deriving (Eq, Show, FromJSON)

data Warning = Warning
  { warningCode    :: WarningCode
  , warningMessage :: WarningMessage
  , warningType    :: WarningType
  } deriving (Eq, Show)

instance FromJSON Warning where
  parseJSON = withObject "Warning" parse
    where
      parse o = Warning
                <$> o .: "code"
                <*> o .: "message"
                <*> o .: "type"

newtype WarningCode = WarningCode
  { unWarningCode :: Text
  } deriving (Eq, Show, FromJSON)

newtype WarningMessage = WarningMessage
  { unWarningMessage :: Text
  } deriving (Eq, Show, FromJSON)

data WarningType
  = WarningWarning
  | WarningError
  deriving (Eq, Show)

instance FromJSON WarningType where
  parseJSON = withText "warningType" parse
    where
      parse "warning" = pure WarningWarning
      parse "error"   = pure WarningError
      parse o         = fail ("Unexpected warningType value: " <> show o)

data Error = Error
  { errorCode    :: ErrorCode
  , errorMessage :: ErrorMessage
  , errorType    :: ErrorType
  } deriving (Eq, Show)

instance FromJSON Error where
  parseJSON = withObject "Error" parse
    where
      parse o = Ballast.Types.Error
                <$> o .: "code"
                <*> o .: "message"
                <*> o .: "type"

newtype ErrorCode = ErrorCode
  { unErrorCode :: Text
  } deriving (Eq, Show, FromJSON)

newtype ErrorMessage = ErrorMessage
  { unErrorMessage :: Text
  } deriving (Eq, Show, FromJSON)

data ErrorType
  = ErrorWarning
  | ErrorError
  deriving (Eq, Show)

instance FromJSON ErrorType where
  parseJSON = withText "errorType" parse
    where
      parse "warning" = pure ErrorWarning
      parse "error"   = pure ErrorError
      parse o         = fail ("Unexpected errorType value: " <> show o)
        
data RateResource = RateResource
  { resourceGroupBy :: GroupBy
  , resourceRates   :: Rates
  } deriving (Eq, Show)

instance FromJSON RateResource where
  parseJSON = withObject "RateResource" parse
    where parse o = RateResource
                    <$> o .: "groupBy"
                    <*> o .: "rates"

newtype Rates = Rates
  { rateServiceOptions :: [ServiceOptions]
  } deriving (Eq, Show, FromJSON)

data ServiceOptions = ServiceOptions
  { sOptsServiceOptions  :: [ServiceOption]
  , sOptsGroupId         :: Maybe GroupId
  , sOptsGroupExternalId :: Maybe GroupExternalId
  } deriving (Eq, Show)

instance FromJSON ServiceOptions where
  parseJSON = withObject "ServiceOptions" parse
    where
      parse o = ServiceOptions
                <$> o .:  "serviceOptions"
                <*> o .:? "groupId"
                <*> o .:? "groupExternalId"

type GroupId = Id

type GroupExternalId = ExternalId

newtype Id = Id
  { unId :: Integer
  } deriving (Eq, Show, ToJSON, FromJSON)

newtype ExternalId = ExternalId
  { unExternalId :: Text
  } deriving (Eq, Show, ToJSON, FromJSON)

data ServiceOption = ServiceOption
  { sOptServiceLevelCode :: ServiceLevelCode
  , sOptServiceLevelName :: ServiceOptionServiceLevelName
  , sOptShipments        :: [Shipment]
  } deriving (Eq, Show)

instance FromJSON ServiceOption where
  parseJSON = withObject "ServiceOption" parse
    where
      parse o = ServiceOption
                <$> o .: "serviceLevelCode"
                <*> o .: "serviceLevelName"
                <*> o .: "shipments"

newtype ServiceOptionServiceLevelName = ServiceOptionServiceLevelName
  { unServiceOptionServiceLevelName :: Text
  } deriving (Eq, Show, FromJSON)

data ServiceLevelCode
  = DomesticGround
  | DomesticTwoDay
  | DomesticOneDay
  | InternationalEconomy
  | InternationalStandard
  | InternationalPlus
  | InternationalPremium
  deriving (Eq, Show)

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
      parse o         = fail ("Unexpected serviceLevelCode value: " <> show o)

data Shipment = Shipment
  { shipmentWarehouseName           :: WarehouseName
  , shipmentCarrier                 :: Carrier
  , shipmentCost                    :: Cost
  , shipmentSubtotals               :: [Subtotal]
  , shipmentPieces                  :: [Piece]
  , shipmentExpectedShipDate        :: Maybe ExpectedShipDate
  , shipmentExpectedDeliveryDateMin :: Maybe ExpectedDeliveryDateMin
  , shipmentExpectedDeliveryDateMax :: Maybe ExpectedDeliveryDateMax
  } deriving (Eq, Show)

instance FromJSON Shipment where
  parseJSON = withObject "Shipment" parse
    where
      parse o = Shipment
                <$> o .:  "warehouseName"
                <*> o .:  "carrier"
                <*> o .:  "cost"
                <*> o .:  "subtotals"
                <*> o .:  "pieces"
                <*> o .:? "expectedShipDate"
                <*> o .:? "expectedDeliveryDateMin"
                <*> o .:? "expectedDeliveryDateMax"

newtype WarehouseName = WarehouseName
  { unWarehouseName :: Text
  } deriving (Eq, Show, FromJSON)

newtype ExpectedShipDate = ExpectedShipDate
  { unExpectedShipDate :: UTCTime
  } deriving (Eq, Show, FromJSON)

type ExpectedDeliveryDateMin = ExpectedShipDate

type ExpectedDeliveryDateMax = ExpectedShipDate

data Carrier = Carrier
  { carrierCode       :: CarrierCode
  , carrierName       :: CarrierName
  , carrierProperties :: CarrierProperties
  } deriving (Eq, Show)

instance FromJSON Carrier where
  parseJSON = withObject "Carrier" parse
    where
      parse o = Carrier
                <$> o .: "code"
                <*> o .: "name"
                <*> o .: "properties"

newtype CarrierCode = CarrierCode
  { unCarrierCode :: Text
  } deriving (Eq, Show, FromJSON)

newtype CarrierName = CarrierName
  { unCarrierName :: Text
  } deriving (Eq, Show, ToJSON, FromJSON)

newtype CarrierProperties = CarrierProperties
  { unCarrierProperties :: [Text]
  } deriving (Eq, Show, FromJSON)

data Cost = Cost
  { costCurrency         :: CostCurrency
  , costType             :: CostType
  , costName             :: CostName
  , costAmount           :: CostAmount
  , costConverted        :: CostConverted
  , costOriginalCost     :: CostOriginalCost
  , costOriginalCurrency :: CostOriginalCurrency
  } deriving (Eq, Show)

instance FromJSON Cost where
  parseJSON = withObject "Cost" parse
    where
      parse o = Cost
                <$> o .: "currency"
                <*> o .: "type"
                <*> o .: "name"
                <*> o .: "amount"
                <*> o .: "converted"
                <*> o .: "originalCost"
                <*> o .: "originalCurrency"

newtype CostCurrency = CostCurrency
  { unCostCurrency :: Text
  } deriving (Eq, Show, FromJSON)

newtype CostType = CostType
  { unCostType :: Text
  } deriving (Eq, Show, FromJSON)

newtype CostName = CostName
  { unCostName :: Text
  } deriving (Eq, Show, FromJSON)

newtype CostAmount = CostAmount
  { unCostAmount :: Centi
  } deriving (Eq, Show, FromJSON)

newtype CostConverted = CostConverted
  { unCostConverted :: Bool
  } deriving (Eq, Show, FromJSON)

newtype CostOriginalCost = CostOriginalCost
  { unCostOriginalCost :: Centi
  } deriving (Eq, Show, FromJSON)

newtype CostOriginalCurrency = CostOriginalCurrency
  { unCostOriginalCurrency :: Text
  } deriving (Eq, Show, FromJSON)

data Subtotal = Subtotal
  { subtotalCurrency         :: SubtotalCurrency
  , sbutotalType             :: SubtotalType
  , subtotalName             :: SubtotalName
  , subtotalAmount           :: SubtotalAmount
  , subtotalConverted        :: SubtotalConverted
  , subtotalOriginalCost     :: SubtotalOriginalCost
  , subtotalOriginalCurrency :: SubtotalOriginalCurrency
  } deriving (Eq, Show)

instance FromJSON Subtotal where
  parseJSON = withObject "Subtotal" parse
    where
      parse o = Subtotal
                <$> o .: "currency"
                <*> o .: "type"
                <*> o .: "name"
                <*> o .: "amount"
                <*> o .: "converted"
                <*> o .: "originalCost"
                <*> o .: "originalCurrency"

newtype SubtotalCurrency = SubtotalCurrency
  { unSubtotalCurrency :: Text
  } deriving (Eq, Show, FromJSON)

newtype SubtotalType = SubtotalType
  { unSubtotalType :: Text
  } deriving (Eq, Show, FromJSON)

newtype SubtotalName = SubtotalName
  { unSubtotalName :: Text
  } deriving (Eq, Show, FromJSON)

newtype SubtotalAmount = SubtotalAmount
  { unSubtotalAmount :: Centi
  } deriving (Eq, Show, FromJSON)

newtype SubtotalConverted = SubtotalConverted
  { unSubtotalConverted :: Bool
  } deriving (Eq, Show, FromJSON)

newtype SubtotalOriginalCost = SubtotalOriginalCost
  { unSubtotalOriginalCost :: Centi
  } deriving (Eq, Show, FromJSON)

newtype SubtotalOriginalCurrency = SubtotalOriginalCurrency
  { unSubtotalOriginalCurrency :: Text
  } deriving (Eq, Show, FromJSON)

data Piece = Piece
  { pieceLength     :: PieceLength
  , pieceWidth      :: PieceWidth
  , pieceHeight     :: PieceHeight
  , pieceWeight     :: PieceWeight
  , pieceSubweights :: [PieceSubWeight]
  , pieceContents   :: [PieceContent]
  } deriving (Eq, Show)

instance FromJSON Piece where
  parseJSON = withObject "Piece" parse
    where
      parse o = Piece
                <$> o .: "length"
                <*> o .: "width"
                <*> o .: "height"
                <*> o .: "weight"
                <*> o .: "subweights"
                <*> o .: "contents"

data PieceLength = PieceLength
  { pieceLengthAmount :: Length
  , pieceLengthUnits  :: PieceLengthUnits
  } deriving (Eq, Show)

instance FromJSON PieceLength where
  parseJSON = withObject "PieceLength" parse
    where
      parse o = PieceLength
                <$> o .: "amount"
                <*> o .: "units"

newtype Length = Length
  { unLength :: Double
  } deriving (Eq, Show, ToJSON, FromJSON)

newtype PieceLengthUnits = PieceLengthUnits
  { unPieceLengthUnits :: Text
  } deriving (Eq, Show, FromJSON)

data PieceWidth = PieceWidth
  { pieceWidthAmount :: Width
  , pieceWidthUnits  :: PieceWidthUnits
  } deriving (Eq, Show)

instance FromJSON PieceWidth where
  parseJSON = withObject "PieceWidth" parse
    where
      parse o = PieceWidth
                <$> o .: "amount"
                <*> o .: "units"

newtype Width = Width
  { unWidth :: Double
  } deriving (Eq, Show, ToJSON, FromJSON)

newtype PieceWidthUnits = PieceWidthUnits
  { unPieceWidthUnits :: Text
  } deriving (Eq, Show, FromJSON)

data PieceHeight = PieceHeight
  { pieceHeightAmount :: Height
  , pieceHeightUnits  :: PieceHeightUnits
  } deriving (Eq, Show)

instance FromJSON PieceHeight where
  parseJSON = withObject "PieceHeight" parse
    where
      parse o = PieceHeight
                <$> o .: "amount"
                <*> o .: "units"

newtype Height = Height
  { unHeight :: Double
  } deriving (Eq, Show, ToJSON, FromJSON)

newtype PieceHeightUnits = PieceHeightUnits
  { unPieceHeightUnits :: Text
  } deriving (Eq, Show, FromJSON)

data PieceWeight = PieceWeight
  { pieceWeightAmount :: Weight
  , pieceWeightUnits  :: PieceWeightUnits
  , pieceWeightType   :: PieceWeightType
  } deriving (Eq, Show)

instance FromJSON PieceWeight where
  parseJSON = withObject "PieceWeight" parse
    where
      parse o = PieceWeight
                <$> o .: "amount"
                <*> o .: "units"
                <*> o .: "type"

newtype Weight = Weight
  { unWeight :: Double
  } deriving (Eq, Show, ToJSON, FromJSON)

newtype PieceWeightUnits = PieceWeightUnits
  { unPieceWeightUnits :: Text
  } deriving (Eq, Show, FromJSON)

newtype PieceWeightType = PieceWeightType
  { unPieceWeightType :: Text
  } deriving (Eq, Show, FromJSON)

data PieceSubWeight = PieceSubWeight
  { pieceSubWeightAmount :: Weight
  , pieceSubWeightUnits  :: PieceSubWeightUnits
  , pieceSubWeightType   :: PieceSubWeightType
  } deriving (Eq, Show)

instance FromJSON PieceSubWeight where
  parseJSON = withObject "PieceSubWeight" parse
    where
      parse o = PieceSubWeight
                <$> o .: "amount"
                <*> o .: "units"
                <*> o .: "type"

newtype PieceSubWeightUnits = PieceSubWeightUnits
  { unPieceSubWeightUnits :: Text
  } deriving (Eq, Show, FromJSON)

newtype PieceSubWeightType = PieceSubWeightType
  { unPieceSubWeightType :: Text
  } deriving (Eq, Show, FromJSON)

data PieceContent = PieceContent
  { pieceContentSku      :: SKU
  , pieceContentQuantity :: Quantity
  } deriving (Eq, Show)

instance FromJSON PieceContent where
  parseJSON = withObject "PieceContent" parse
    where
      parse o = PieceContent
                <$> o .: "sku"
                <*> o .: "quantity"

type Reply = Network.HTTP.Client.Response BSL.ByteString
type Method = NHTM.Method

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
  } deriving (Eq, Show)

instance FromJSON StockResponse where
  parseJSON = withObject "StockResponse" parse
    where
      parse o = StockResponse
                <$> o .:  "status"
                <*> o .:  "message"
                <*> o .:? "warnings"
                <*> o .:? "errors"
                <*> o .:? "resourceLocation"
                <*> o .:  "resource"

newtype ParentId = ParentId
  { parentId :: Text
  } deriving (Eq, Show)

newtype ProductIdParam = ProductIdParam
  { productIdParam :: [Text]
  } deriving (Eq, Show)

newtype ProductExternalIdParam = ProductExternalIdParam
  { productExternalIdParam :: [Text]
  } deriving (Eq, Show)

newtype WarehouseIdParam = WarehouseIdParam
  { warehouseIdParam :: [Text]
  } deriving (Eq, Show)

newtype WarehouseExternalIdParam = WarehouseExternalIdParam
  { warehouseExternalIdParam :: [Text]
  } deriving (Eq, Show)

newtype WarehouseRegionParam = WarehouseRegionParam
  { warehouseRegionParam :: [Text]
  } deriving (Eq, Show)

newtype WarehouseAreaParam = WarehouseAreaParam
  { warehouseAreaParam :: [Text]
  } deriving (Eq, Show)

newtype ChannelName = ChannelName
  { channelName :: Text
  } deriving (Eq, Show, ToJSON)

newtype IncludeEmpty = IncludeEmpty
  { includeEmpty :: Integer
  } deriving (Eq, Show)

newtype VendorIdParam = VendorIdParam
  { vendorIdParam :: [Integer]
  } deriving (Eq, Show)

newtype VendorExternalIdParam = VendorExternalIdParam
  { vendorExternalIdParam :: [Integer]
  } deriving (Eq, Show)

newtype DisableAutoBreakLots = DisableAutoBreakLots
  { disableAutoBreakLots :: Text
  } deriving (Eq, Show)

data Mode
  = IncludingHigherLevelQuantitiesWithLots
  | IncludingHigherLevelQuantitiesWithoutLots
  | NotIncludingHigherLevelQuantitiesWithLots
  | NotIncludingHigherLevelQuantitiesWithoutLots
  deriving (Eq, Show)

modeToBS8 :: Mode -> BS8.ByteString
modeToBS8 IncludingHigherLevelQuantitiesWithLots       = "IncludingHigherLevelQuantitiesWithLots"
modeToBS8 IncludingHigherLevelQuantitiesWithoutLots    = "IncludingHigherLevelQuantitiesWithoutLots"
modeToBS8 NotIncludingHigherLevelQuantitiesWithLots    = "NotIncludingHigherLevelQuantitiesWithLots"
modeToBS8 NotIncludingHigherLevelQuantitiesWithoutLots = "NotIncludingHigherLevelQuantitiesWithoutLots"

newtype IncludeEmptyShipwireAnywhere = IncludeEmptyShipwireAnywhere
  { inclEmptyShipwireAnywhere :: Text
  } deriving (Eq, Show)

newtype Offset = Offset
  { unOffset :: Integer
  } deriving (Eq, Show, FromJSON)

newtype Total = Total
  { unTotal :: Integer
  } deriving (Eq, Show, FromJSON)

newtype Previous = Previous
  { unPrevious :: Text
  } deriving (Eq, Show, FromJSON)

newtype Next = Next
  { unNext :: Text
  } deriving (Eq, Show, FromJSON)

newtype Limit = Limit
  { unLimit :: Integer
  } deriving (Eq, Show)

data StockResource = StockResource
  { stockResponseOffset   :: ResponseOffset
  , stockResponseTotal    :: ResponseTotal
  , stockResponsePrevious :: Maybe ResponsePrevious
  , stockResponseNext     :: Maybe ResponseNext
  , stockResponseItems    :: [StockItem]
  } deriving (Eq, Show)

instance FromJSON StockResource where
  parseJSON = withObject "StockResource" parse
    where
      parse o = StockResource
                <$> o .:  "offset"
                <*> o .:  "total"
                <*> o .:? "previous"
                <*> o .:? "next"
                <*> o .:  "items"

newtype ResponseOffset = ResponseOffset
  { unResponseOffset :: Integer
  } deriving (Eq, Show, FromJSON)

newtype ResponseTotal = ResponseTotal
  { unResponseTotal :: Integer
  } deriving (Eq, Show, FromJSON)

newtype ResponsePrevious = ResponsePrevious
  { unResponsePrevious :: Text
  } deriving (Eq, Show, FromJSON)

newtype ResponseNext = ResponseNext
  { unResponseNext :: Text
  } deriving (Eq, Show, FromJSON)

data StockItem = StockItem
  { stockItemResourceLocation :: Maybe ResponseResourceLocation
  , stockItemResource         :: StockItemResource
  } deriving (Eq, Show)

instance FromJSON StockItem where
  parseJSON = withObject "StockItem" parse
    where
      parse o = StockItem
                <$> o .:? "resourceLocation"
                <*> o .:  "resource"

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
  } deriving (Eq, Show)

instance FromJSON StockItemResource where
  parseJSON = withObject "StockItemResource" parse
    where
      parse o = StockItemResource
                <$> o .:  "productId"
                <*> o .:? "productExternalId"
                <*> o .:  "sku"
                <*> o .:  "isBundle"
                <*> o .:  "isAlias"
                <*> o .:  "warehouseRegion"
                <*> o .:  "warehouseId"
                <*> o .:? "warehouseExternalId"
                <*> o .:  "pending"
                <*> o .:  "good"
                <*> o .:  "reserved"
                <*> o .:  "backordered"
                <*> o .:  "shipping"
                <*> o .:  "shipped"
                <*> o .:  "creating"
                <*> o .:  "consuming"
                <*> o .:  "consumed"
                <*> o .:  "created"
                <*> o .:  "damaged"
                <*> o .:  "returned"
                <*> o .:  "inreview"
                <*> o .:? "availableDate"
                <*> o .:  "shippedLastDay"
                <*> o .:  "shippedLastWeek"
                <*> o .:  "shippedLast4Weeks"
                <*> o .:  "orderedLastDay"
                <*> o .:  "orderedLastWeek"
                <*> o .:  "orderedLast4Weeks"

type ProductId = Id

type ProductExternalId = ExternalId

newtype WarehouseRegion = WarehouseRegion
  { unWarehouseRegion :: Text
  } deriving (Eq, Show, ToJSON, FromJSON)

newtype StockItemResourcePending = StockItemResourcePending
  { unStockItemResourcePending :: Integer
  } deriving (Eq, Show, FromJSON)

newtype StockItemResourceGood = StockItemResourceGood
  { unStockItemResourceGood :: Integer
  } deriving (Eq, Show, FromJSON)

newtype StockItemResourceReserved = StockItemResourceReserved
  { unStockItemResourceReserved :: Integer
  } deriving (Eq, Show, FromJSON)

newtype StockItemResourceBackordered = StockItemResourceBackordered
  { unStockItemResourceBackorderd :: Integer
  } deriving (Eq, Show, FromJSON)

newtype StockItemResourceShipping = StockItemResourceShipping
  { unStockItemResourceShipping :: Integer
  } deriving (Eq, Show, FromJSON)

newtype StockItemResourceShipped = StockItemResourceShipped
  { unStockItemResourceShipped :: Integer
  } deriving (Eq, Show, FromJSON)

newtype StockItemResourceCreating = StockItemResourceCreating
  { unStockItemResourceCreating :: Integer
  } deriving (Eq, Show, FromJSON)

newtype StockItemResourceConsuming = StockItemResourceConsuming
  { unStockItemResourceConsuming :: Integer
  } deriving (Eq, Show, FromJSON)

newtype StockItemResourceConsumed = StockItemResourceConsumed
  { unStockItemResourceConsumed :: Integer
  } deriving (Eq, Show, FromJSON)

newtype StockItemResourceCreated = StockItemResourceCreated
  { unStockItemResourceCreated :: Integer
  } deriving (Eq, Show, FromJSON)

newtype StockItemResourceDamaged = StockItemResourceDamaged
  { unStockItemResourceDamaged :: Integer
  } deriving (Eq, Show, FromJSON)

newtype StockItemResourceReturned = StockItemResourceReturned
  { unStockItemResourceReturned :: Integer
  } deriving (Eq, Show, FromJSON)

newtype StockItemResourceInReview = StockItemResourceInReview
  { unStockItemResourceInReview :: Integer
  } deriving (Eq, Show, FromJSON)

type StockItemResourceAvailableDate = ExpectedShipDate

newtype StockItemResourceShippedLastDay = StockItemResourceShippedLastDay
  { unStockItemResourceShippedLastDay :: Integer
  } deriving (Eq, Show, FromJSON)

newtype StockItemResourceShippedLastWeek = StockItemResourceShippedLastWeek
  { unStockItemResourceShippedLastWeek :: Integer
  } deriving (Eq, Show, FromJSON)

newtype StockItemResourceShippedLast4Weeks = StockItemResourceShippedLast4Weeks
  { unStockItemResourceShippedLast4Weeks :: Integer
  } deriving (Eq, Show, FromJSON)

newtype StockItemResourceOrderedLastDay = StockItemResourceOrderedLastDay
  { unStockItemResourceOrderedLastDay :: Integer
  } deriving (Eq, Show, FromJSON)

newtype StockItemResourceOrderedLastWeek = StockItemResourceOrderedLastWeek
  { unStockItemResourceOrderedLastWeek :: Integer
  } deriving (Eq, Show, FromJSON)

newtype StockItemResourceOrderedLast4Weeks = StockItemResourceOrderedLast4Weeks
  { unStockItemResourceOrderedLast4Weeks :: Integer
  } deriving (Eq, Show, FromJSON)

data IsBundle
  = Bundle
  | NotBundle
  deriving (Eq, Show)

instance FromJSON IsBundle where
  parseJSON (Number 1) = pure Bundle
  parseJSON (Number 0) = pure NotBundle
  parseJSON o          = fail ("Unexpected isBundle value: " <> show o)

data IsAlias
  = Alias
  | NotAlias
  deriving (Eq, Show)

instance FromJSON IsAlias where
  parseJSON (Number 1) = pure Alias
  parseJSON (Number 0) = pure NotAlias
  parseJSON o          = fail ("Unexpected isAlias value: " <> show o)

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
hostUri ShipwireSandbox    = "https://api.beta.shipwire.com/api/v3"

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

-- | GET /api/v3/receivings
data GetReceivingsRequest
type instance ShipwireReturn GetReceivingsRequest = GetReceivingsResponse

type CreateReceivingResponse = ReceivingsResponse

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

-- | POST /api/v3/receivings
data CreateReceivingRequest
type instance ShipwireReturn CreateReceivingRequest = CreateReceivingResponse

type GetReceivingsResponse = ReceivingsResponse

instance ShipwireHasParam CreateReceivingRequest ExpandParamReceivings

-- | GET /api/v3/receivings/{id}

data GetReceivingRequest
type instance ShipwireReturn GetReceivingRequest = ReceivingResponse

instance ShipwireHasParam GetReceivingRequest ExpandParamReceivings

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

instance ToShipwireParam ExpandParamReceivings where
  toShipwireParam (ExpandParamReceivings xs) =
    (Query ("expand", TE.encodeUtf8 (T.intercalate "," (map expandReceivingsToTx xs))) :)

newtype CommerceNameParam = CommerceNameParam
  { commerceNameParam :: [Text]
  } deriving (Eq, Show)

instance ToShipwireParam CommerceNameParam where
  toShipwireParam (CommerceNameParam ns) =
    (Query ("commerceName", TE.encodeUtf8 (T.intercalate "," ns)) :)

data ReceivingsResponse = ReceivingsResponse
  { receivingsResponseResourceLocation :: ResponseResourceLocation
  , receivingsResponseStatus           :: ResponseStatus
  , receivingsResponseMessage          :: ResponseMessage
  , receivingsResponseWarnings         :: Maybe ResponseWarnings
  , receivingsResponseErrors           :: Maybe ResponseErrors
  , receivingsResponseResource         :: ReceivingsResource
  } deriving (Eq, Show)

instance FromJSON ReceivingsResponse where
  parseJSON = withObject "ReceivingsResponse" parse
    where
      parse o = ReceivingsResponse
                <$> o .:  "resourceLocation"
                <*> o .:  "status"
                <*> o .:  "message"
                <*> o .:? "warnings"
                <*> o .:? "errors"
                <*> o .:  "resource"

data ReceivingsResource = ReceivingsResource
  { receivingsResponseNext     :: Maybe ResponseNext
  , receivingsResponseOffset   :: ResponseOffset
  , receivingsResponsePrevious :: Maybe ResponsePrevious
  , receivingsResponseTotal    :: ResponseTotal
  , receivingsResponseItems    :: ReceivingsItems
  } deriving (Eq, Show)

instance FromJSON ReceivingsResource where
  parseJSON = withObject "ReceivingsResource" parse
    where
      parse o = ReceivingsResource
                <$> o .:? "next"
                <*> o .:  "offset"
                <*> o .:? "previous"
                <*> o .:  "total"
                <*> o .:  "items"

newtype ReceivingsItems = ReceivingsItems
  { unReceivingsItems :: [ReceivingsItem]
  } deriving (Eq, Show, FromJSON)

data ReceivingsItem = ReceivingsItem
  { receivingsItemResourceLocation :: Maybe ResponseResourceLocation
  , receivingsItemResource         :: ReceivingsItemResource
  } deriving (Eq, Show)

instance FromJSON ReceivingsItem where
  parseJSON = withObject "ReceivingsItem" parse
    where
      parse o = ReceivingsItem
                <$> o .:? "resourceLocation"
                <*> o .:  "resource"

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
  } deriving (Eq, Show)

instance FromJSON ReceivingsItemResource where
  parseJSON = withObject "ReceivingsItemResource" parse
    where
      parse o = ReceivingsItemResource
                <$> o .:  "id"
                <*> o .:? "externalId"
                <*> o .:? "orderNo"
                <*> o .:  "transactionId"
                <*> o .:? "expectedDate"
                <*> o .:  "commerceName"
                <*> o .:  "lastUpdatedDate"
                <*> o .:  "status"
                <*> o .:  "holds"
                <*> o .:  "items"
                <*> o .:  "trackings"
                <*> o .:  "shipments"
                <*> o .:  "labels"
                <*> o .:  "options"
                <*> o .:  "arrangement"
                <*> o .:  "shipFrom"
                <*> o .:  "events"
                <*> o .:  "routing"
                <*> o .:  "instructionsRecipients"

data ItemResourceInstructionsRecipients = ItemResourceInstructionsRecipients
  { irirResource         :: Maybe ItemResourceInstructionsRecipientsResource
  , irirResourceLocation :: Maybe ResponseResourceLocation
  } deriving (Eq, Show)

instance FromJSON ItemResourceInstructionsRecipients where
  parseJSON = withObject "ItemResourceInstructionsRecipients" parse
    where
      parse o = ItemResourceInstructionsRecipients
                <$> o .:? "resource"
                <*> o .:? "resourceLocation"

data ItemResourceInstructionsRecipientsResource = ItemResourceInstructionsRecipientsResource
  { irirrItems    :: ItemResourceInstructionsRecipientsResourceItems
  , irirrNext     :: Maybe Next
  , irirrOffset   :: Offset
  , irirrPrevious :: Maybe Previous
  , irirrTotal    :: Total
  } deriving (Eq, Show)

instance FromJSON ItemResourceInstructionsRecipientsResource where
  parseJSON = withObject "ItemResourceInstructionsRecipientsResource" parse
    where
      parse o = ItemResourceInstructionsRecipientsResource
                <$> o .:  "items"
                <*> o .:? "next"
                <*> o .:  "offset"
                <*> o .:? "previous"
                <*> o .:  "total"

newtype ItemResourceInstructionsRecipientsResourceItems = ItemResourceInstructionsRecipientsResourceItems
  { iririItems :: [ItemResourceInstructionsRecipientsItem]
  } deriving (Eq, Show, FromJSON)

data ItemResourceInstructionsRecipientsItem = ItemResourceInstructionsRecipientsItem
  { iririResource         :: ItemResourceInstructionsRecipientsItemResource
  , iririResourceLocation :: Maybe ResponseResourceLocation
  } deriving (Eq, Show)

instance FromJSON ItemResourceInstructionsRecipientsItem where
  parseJSON = withObject "ItemResourceInstructionsRecipientsItem" parse
    where
      parse o = ItemResourceInstructionsRecipientsItem
                <$> o .: "resource"
                <*> o .: "resourceLocation"

data ItemResourceInstructionsRecipientsItemResource = ItemResourceInstructionsRecipientsItemResource
  { iririrEmail :: Email
  , iririrName  :: Name
  , iririrNote  :: Maybe Note
  } deriving (Eq, Show)

instance FromJSON ItemResourceInstructionsRecipientsItemResource where
  parseJSON = withObject "ItemResourceInstructionsRecipientsItemResource" parse
    where
      parse o = ItemResourceInstructionsRecipientsItemResource
                <$> o .:  "email"
                <*> o .:  "name"
                <*> o .:? "note"

data ItemResourceRouting = ItemResourceRouting
  { irrResource         :: ItemResourceRoutingResource
  , irrResourceLocation :: Maybe ResponseResourceLocation
  } deriving (Eq, Show)

instance FromJSON ItemResourceRouting where
  parseJSON = withObject "ItemResourceRouting" parse
    where
      parse o = ItemResourceRouting
                <$> o .:  "resource"
                <*> o .:? "resourceLocation"

data ItemResourceRoutingResource = ItemResourceRoutingResource
  { irrrOriginLatitude      :: Latitude
  , irrrOriginLongitude     :: Longitude
  , irrrWarehouseExternalId :: Maybe WarehouseExternalId
  , irrrWarehouseId         :: WarehouseId
  , irrrWarehouseName       :: WarehouseName
  , irrrWarehouseRegion     :: WarehouseRegion
  } deriving (Eq, Show)

instance FromJSON ItemResourceRoutingResource where
  parseJSON = withObject "ItemResourceRoutingResource" parse
    where
      parse o = ItemResourceRoutingResource
                <$> o .:  "originLatitude"
                <*> o .:  "originLongitude"
                <*> o .:? "warehouseExternalId"
                <*> o .:  "warehouseId"
                <*> o .:  "warehouseName"
                <*> o .:  "warehouseRegion"

newtype Latitude = Latitude
  { unLatitude :: Double
  } deriving (Eq, Show, FromJSON)

type Longitude = Latitude

type ExpectedDateUTCTime = ExpectedShipDate

type LastUpdatedDate = ExpectedShipDate

data ItemResourceEvents = ItemResourceEvents
  { ireResource         :: ItemResourceEventsResource
  , ireResourceLocation :: Maybe ResponseResourceLocation
  } deriving (Eq, Show)

instance FromJSON ItemResourceEvents where
  parseJSON = withObject "ItemResourceEvents" parse
    where
      parse o = ItemResourceEvents
                <$> o .:  "resource"
                <*> o .:? "resourceLocation"

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
  } deriving (Eq, Show)

instance FromJSON ItemResourceEventsResource where
  parseJSON = withObject "ItemResourceEventsResource" parse
    where
      parse o = ItemResourceEventsResource
                <$> o .:  "createdDate"
                <*> o .:? "pickedUpDate"
                <*> o .:? "submittedDate"
                <*> o .:  "processedDate"
                <*> o .:? "completedDate"
                <*> o .:? "expectedDateText"
                <*> o .:? "deliveredDate"
                <*> o .:? "cancelledDate"
                <*> o .:? "returnedDate"
                <*> o .:? "lastManualUpdateDate"

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
  } deriving (Eq, Show)

instance FromJSON ItemResourceShipFrom where
  parseJSON = withObject "ItemResourceShipFrom" parse
    where
      parse o = ItemResourceShipFrom
                <$> o .:  "resource"
                <*> o .:? "resourceLocation"

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
  } deriving (Eq, Show)

instance FromJSON ItemResourceShipFromResource where
  parseJSON = withObject "ItemResourceShipFromResource" parse
    where
      parse o = ItemResourceShipFromResource
                <$> o .:? "email"
                <*> o .:? "name"
                <*> o .:? "address1"
                <*> o .:? "address2"
                <*> o .:? "address3"
                <*> o .:? "city"
                <*> o .:? "state"
                <*> o .:? "postalCode"
                <*> o .:? "country"
                <*> o .:? "phone"

data ItemResourceArrangement = ItemResourceArrangement
  { iraResource         :: ItemResourceArrangementResource
  , iraResourceLocation :: Maybe ResponseResourceLocation
  } deriving (Eq, Show)

instance FromJSON ItemResourceArrangement where
  parseJSON = withObject "ItemResourceArrangement" parse
    where
      parse o = ItemResourceArrangement
                <$> o .:  "resource"
                <*> o .:? "resourceLocation"

data ItemResourceArrangementResource = ItemResourceArrangementResource
  { irarContact :: Contact
  , irarPhone   :: Phone
  , irarType    :: ArrangementType
  } deriving (Eq, Show)

instance FromJSON ItemResourceArrangementResource where
  parseJSON = withObject "ItemResourceArrangementResource" parse
    where
      parse o = ItemResourceArrangementResource
                <$> o .: "contact"
                <*> o .: "phone"
                <*> o .: "type"

data ItemResourceOptions = ItemResourceOptions
  { iroResource         :: ItemResourceOptionsResource
  , iroResourceLocation :: Maybe ResponseResourceLocation
  } deriving (Eq, Show)

instance FromJSON ItemResourceOptions where
  parseJSON = withObject "ItemResourceOptions" parse
    where
      parse o = ItemResourceOptions
                <$> o .:  "resource"
                <*> o .:? "resourceLocation"

data ItemResourceOptionsResource = ItemResourceOptionsResource
  { irorWarehouseExternalid :: Maybe WarehouseExternalId
  , irorWarehouseId         :: WarehouseId
  , irorWarehouseRegion     :: WarehouseRegion
  } deriving (Eq, Show)

instance FromJSON ItemResourceOptionsResource where
  parseJSON = withObject "ItemResourceOptionsResource" parse
    where
      parse o = ItemResourceOptionsResource
                <$> o .:? "externalId"
                <*> o .:  "warehouseId"
                <*> o .:  "warehouseRegion"

data ItemResourceLabels = ItemResourceLabels
  { irlResource         :: Maybe ItemResourceLabelsResource
  , irlResourceLocation :: ResponseResourceLocation
  } deriving (Eq, Show)

instance FromJSON ItemResourceLabels where
  parseJSON = withObject "ItemResourceLabels" parse
    where
      parse o = ItemResourceLabels
                <$> o .:? "resource"
                <*> o .:  "resourceLocation"
   
data ItemResourceLabelsResource = ItemResourceLabelsResource
  { irlrItems    :: ItemResourceLabelsResourceItems
  , irlrNext     :: Maybe Next
  , irlrOffset   :: Offset
  , irlrPrevious :: Maybe Previous
  , irlrTotal    :: Total
  } deriving (Eq, Show)

instance FromJSON ItemResourceLabelsResource where
  parseJSON = withObject "ItemResourceLabelsResource" parse
    where
      parse o = ItemResourceLabelsResource
                <$> o .:  "items"
                <*> o .:? "next"
                <*> o .:  "offset"
                <*> o .:? "previous"
                <*> o .:  "total"

newtype ItemResourceLabelsResourceItems = ItemResourceLabelsResourceItems
  { irlriItems :: [ItemResourceLabelsResourceItem]
  } deriving (Eq, Show, FromJSON)

data ItemResourceLabelsResourceItem = ItemResourceLabelsResourceItem
  { irlriResource         :: ItemResourceLabelsResourceItemResource
  , irlriResourceLocation :: Maybe ResponseResourceLocation
  } deriving (Eq, Show)

instance FromJSON ItemResourceLabelsResourceItem where
  parseJSON = withObject "ItemResourceLabelsResourceItem" parse
    where
      parse o = ItemResourceLabelsResourceItem
                <$> o .:  "resource"
                <*> o .:? "resourceLocation"

data ItemResourceLabelsResourceItemResource = ItemResourceLabelsResourceItemResource
  { irlrirLabelId         :: LabelId
  , irlrirOrderId         :: OrderId
  , irlrirOrderExternalId :: OrderExternalId
  } deriving (Eq, Show)

instance FromJSON ItemResourceLabelsResourceItemResource where
  parseJSON = withObject "ItemResourceLabelsResourceItemResource" parse
    where
      parse o = ItemResourceLabelsResourceItemResource
                <$> o .: "labelId"
                <*> o .: "orderId"
                <*> o .: "orderExternalId"

type LabelId = Id

type OrderId = Id

type OrderExternalId = ExternalId

newtype ItemResourceStatus = ItemResourceStatus
  { unItemResourceStatus :: Text
  } deriving (Eq, Show, FromJSON)

data ItemResourceShipments = ItemResourceShipments
  { irsResource         :: Maybe ItemResourceShipmentsResource
  , irsResourceLocation :: ResponseResourceLocation
  } deriving (Eq, Show)

instance FromJSON ItemResourceShipments where
  parseJSON = withObject "ItemResourceShipments" parse
    where
      parse o = ItemResourceShipments
                <$> o .:? "resource"
                <*> o .:  "resourceLocation"

data ItemResourceShipmentsResource = ItemResourceShipmentsResource
  { irsrItems    :: ItemResourceShipmentsResourceItems
  , irsrNext     :: Maybe Next
  , irsrOffset   :: Offset
  , irsrPrevious :: Maybe Previous
  , irsrTotal    :: Total
  } deriving (Eq, Show)

instance FromJSON ItemResourceShipmentsResource where
  parseJSON = withObject "ItemResourceShipmentsResource" parse
    where
      parse o = ItemResourceShipmentsResource
                <$> o .:  "items"
                <*> o .:? "next"
                <*> o .:  "offset"
                <*> o .:? "previous"
                <*> o .:  "total"

newtype ItemResourceShipmentsResourceItems = ItemResourceShipmentsResourceItems
  { irsriItems :: [ItemResourceShipmentsResourceItem]
  } deriving (Eq, Show, FromJSON)

data ItemResourceShipmentsResourceItem = ItemResourceShipmentsResourceItem
  { irsriResource         :: ItemResourceShipmentsResourceItemResource
  , irsriResourceLocation :: ResponseResourceLocation
  } deriving (Eq, Show)

instance FromJSON ItemResourceShipmentsResourceItem where
  parseJSON = withObject "ItemResourceShipmentsResourceItem" parse
    where
      parse o = ItemResourceShipmentsResourceItem
                <$> o .: "resource"
                <*> o .: "resourceLocation"

data ItemResourceShipmentsResourceItemResource = ItemResourceShipmentsResourceItemResource
  { irsrirShipmentId :: ShipmentId
  , irsrirType       :: Type
  , irsrirHeight     :: Maybe TextHeight
  , irsrirLength     :: Maybe TextLength
  , irsrirWeight     :: Maybe TextWeight
  , irsrirWidth      :: Maybe TextWidth
  } deriving (Eq, Show)

instance FromJSON ItemResourceShipmentsResourceItemResource where
  parseJSON = withObject "ItemResourceShipmentsResourceItemResource" parse
    where
      parse o = ItemResourceShipmentsResourceItemResource
                <$> o .:  "shipmentId"
                <*> o .:  "type"
                <*> o .:? "height"
                <*> o .:? "length"
                <*> o .:? "weight"
                <*> o .:? "width"

type ShipmentId = Id

newtype TextHeight = TextHeight
  { unTextHeight :: Text
  } deriving (Eq, Show, FromJSON)

newtype TextLength = TextLength
  { unTextLength :: Text
  } deriving (Eq, Show, FromJSON)

newtype TextWeight = TextWeight
  { unTextWeight :: Text
  } deriving (Eq, Show, FromJSON)

newtype TextWidth = TextWidth
  { unTextWidth :: Text
  } deriving (Eq, Show, FromJSON)

data ItemResourceTrackings = ItemResourceTrackings
  { irtResource         :: Maybe ItemResourceTrackingsResource
  , irtResourceLocation :: ResponseResourceLocation
  } deriving (Eq, Show)

instance FromJSON ItemResourceTrackings where
  parseJSON = withObject "ItemResourceTrackings" parse
    where
      parse o = ItemResourceTrackings
                <$> o .:? "resource"
                <*> o .:  "resourceLocation"

data ItemResourceTrackingsResource = ItemResourceTrackingsResource
  { irtrItems    :: ItemResourceTrackingsResourceItems
  , irtrNext     :: Maybe Next
  , irtrOffset   :: Offset
  , irtrPrevious :: Maybe Previous
  , irtrTotal    :: Total
  } deriving (Eq, Show)

instance FromJSON ItemResourceTrackingsResource where
  parseJSON = withObject "ItemResourceTrackingsResource" parse
    where
      parse o = ItemResourceTrackingsResource
                <$> o .:  "items"
                <*> o .:? "next"
                <*> o .:  "offset"
                <*> o .:? "previous"
                <*> o .:  "total"

newtype ItemResourceTrackingsResourceItems = ItemResourceTrackingsResourceItems
  { irtriItems :: [ItemResourceTrackingsResourceItem]
  } deriving (Eq, Show, FromJSON)

data ItemResourceTrackingsResourceItem = ItemResourceTrackingsResourceItem
  { irtriResource         :: ItemResourceTrackingsResourceItemResource
  , irtriResourceLocation :: ResponseResourceLocation
  } deriving (Eq, Show)

instance FromJSON ItemResourceTrackingsResourceItem where
  parseJSON = withObject "ItemResourceTrackingsResourceItem" parse
    where
      parse o = ItemResourceTrackingsResourceItem
                <$> o .: "resource"
                <*> o .: "resourceLocation"

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
  } deriving (Eq, Show)

instance FromJSON ItemResourceTrackingsResourceItemResource where
  parseJSON = withObject "ItemResourceTrackingsResourceItemResource" parse
    where
      parse o = ItemResourceTrackingsResourceItemResource
                <$> o .:  "id"
                <*> o .:? "orderExternalId"
                <*> o .:  "orderId"
                <*> o .:  "tracking"
                <*> o .:  "carrier"
                <*> o .:? "contact"
                <*> o .:? "phone"
                <*> o .:? "url"
                <*> o .:? "summary"
                <*> o .:? "summaryDate"
                <*> o .:? "trackedDate"
                <*> o .:? "deliveredDate"

type TrackedDate = ExpectedShipDate

type DeliveredDate = ExpectedShipDate

newtype Summary = Summary
  { unSummary :: Text
  } deriving (Eq, Show, FromJSON)

type SummaryDate = ExpectedShipDate

newtype URL = URL
  { unURL :: Text
  } deriving (Eq, Show, FromJSON)

data ItemResourceItems = ItemResourceItems
  { iriResource         :: Maybe ItemResourceItemsResource
  , iriResourceLocation :: ResponseResourceLocation
  } deriving (Eq, Show)

instance FromJSON ItemResourceItems where
  parseJSON = withObject "ItemResourceItems" parse
    where
      parse o = ItemResourceItems
                <$> o .:? "resource"
                <*> o .:  "resourceLocation"

data ItemResourceItemsResource = ItemResourceItemsResource
  { irirItems    :: ItemResourceItemsResourceItems
  , irirNext     :: Maybe Next
  , irirOffset   :: Offset
  , irirPrevious :: Maybe Previous
  , irirTotal    :: Total
  } deriving (Eq, Show)

instance FromJSON ItemResourceItemsResource where
  parseJSON = withObject "ItemResourceItemsResource" parse
    where
      parse o = ItemResourceItemsResource
                <$> o .:  "items"
                <*> o .:? "next"
                <*> o .:  "offset"
                <*> o .:? "previous"
                <*> o .:  "total"

newtype ItemResourceItemsResourceItems = ItemResourceItemsResourceItems
  { iririsItems :: [ItemResourceItemsResourceItem]
  } deriving (Eq, Show, FromJSON)

data ItemResourceItemsResourceItem = ItemResourceItemsResourceItem
  { iririsResource         :: ItemResourceItemsResourceItemResource
  , iririsResourceLocation :: Maybe ResponseResourceLocation
  } deriving (Eq, Show)

instance FromJSON ItemResourceItemsResourceItem where
  parseJSON = withObject "ItemResourceItemsResourceItem" parse
    where
      parse o = ItemResourceItemsResourceItem
                <$> o .:  "resource"
                <*> o .:? "resourceLocation"

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
  } deriving (Eq, Show)

instance FromJSON ItemResourceItemsResourceItemResource where
  parseJSON = withObject "ItemResourceItemsResourceItemResource" parse
    where
      parse o = ItemResourceItemsResourceItemResource
                <$> o .:? "orderExternalId"
                <*> o .:  "orderId"
                <*> o .:? "productExternalId"
                <*> o .:  "productId"
                <*> o .:  "quantity"
                <*> o .:  "sku"
                <*> o .:  "expected"
                <*> o .:  "pending"
                <*> o .:  "good"
                <*> o .:  "inReview"
                <*> o .:  "damaged"

newtype Expected = Expected
  { unExpected :: Integer
  } deriving (Eq, Show, FromJSON)

type Pending = StockItemResourcePending

type Good = StockItemResourceGood

type InReview = StockItemResourceInReview

type Damaged = StockItemResourceDamaged

data ItemResourceHolds = ItemResourceHolds
  { irhResource         :: Maybe ItemResourceHoldsResource
  , irhResourceLocation :: ResponseResourceLocation
  } deriving (Eq, Show)

instance FromJSON ItemResourceHolds where
  parseJSON = withObject "ItemResourceHolds" parse
    where
      parse o = ItemResourceHolds
                <$> o .:? "resource"
                <*> o .:  "resourceLocation"

data ItemResourceHoldsResource = ItemResourceHoldsResource
  { irhrItems    :: ItemResourceHoldsResourceItems
  , irhrNext     :: Maybe Next
  , irhrOffset   :: Offset
  , irhrPrevious :: Maybe Previous
  , irhrTotal    :: Total
  } deriving (Eq, Show)

instance FromJSON ItemResourceHoldsResource where
  parseJSON = withObject "ItemResourceHoldsResource" parse
    where
      parse o = ItemResourceHoldsResource
                <$> o .:  "items"
                <*> o .:? "next"
                <*> o .:  "offset"
                <*> o .:? "previous"
                <*> o .:  "total"

newtype ItemResourceHoldsResourceItems = ItemResourceHoldsResourceItems
  { irhriItems :: [ItemResourceHoldsResourceItem]
  } deriving (Eq, Show, FromJSON)

data ItemResourceHoldsResourceItem = ItemResourceHoldsResourceItem
  { irhriResource         :: ItemResourceHoldsResourceItemResource
  , irhriResourceLocation :: Maybe ResponseResourceLocation
  } deriving (Eq, Show)

instance FromJSON ItemResourceHoldsResourceItem where
  parseJSON = withObject "ItemResourceHoldsResourceItem" parse
    where
      parse o = ItemResourceHoldsResourceItem
                <$> o .:  "resource"
                <*> o .:? "resourceLocation"

data ItemResourceHoldsResourceItemResource = ItemResourceHoldsResourceItemResource
  { irhrirAppliedDate     :: AppliedDate
  , irhrirClearedDate     :: Maybe ClearedDate
  , irhrirDescription     :: Description
  , irhrirExternalOrderId :: Maybe ExternalOrderId
  , irhrirId              :: Id
  , irhrirOrderId         :: OrderId
  , irhrirType            :: Type
  } deriving (Eq, Show)

instance FromJSON ItemResourceHoldsResourceItemResource where
  parseJSON = withObject "ItemResourceHoldsResourceItemResource" parse
    where
      parse o = ItemResourceHoldsResourceItemResource
                <$> o .:  "appliedDate"
                <*> o .:? "clearedDate"
                <*> o .:  "description"
                <*> o .:? "externalOrderId"
                <*> o .:  "id"
                <*> o .:  "orderId"
                <*> o .:  "type"

type ExternalOrderId = ExternalId

newtype Description = Description
  { unDescription :: Text
  } deriving (Eq, Show, FromJSON)

type ClearedDate = ExpectedShipDate

type AppliedDate = ExpectedShipDate

newtype ItemStatus = ItemStatus
  { unItemStatus :: Text
  } deriving (Eq, Show)

newtype CommerceName = CommerceName
  { unCommerceName :: Text
  } deriving (Eq, Show, FromJSON)

type TransactionId = ExternalId

data CreateReceiving = CreateReceiving
  { createReceivingExternalId             :: Maybe ExternalId
  , createReceivingOrderNo                :: Maybe OrderNo
  , createReceivingExpectedDate           :: Maybe ExpectedDateText
  , createReceivingOptions                :: ReceivingOptions
  , createReceivingArrangement            :: ReceivingArrangement
  , createReceivingShipments              :: ReceivingShipments
  , createReceivingLabels                 :: Maybe ReceivingLabels
  , createReceivingTrackings              :: Maybe ReceivingTrackings
  , createReceivingItems                  :: ReceivingItems
  , createReceivingShipFrom               :: ReceivingShipFrom
  , createReceivingInstructionsRecipients :: Maybe ReceivingInstructionsRecipients
  } deriving (Eq, Show)

instance ToJSON CreateReceiving where
  toJSON CreateReceiving {..} = omitNulls ["externalId"             .= createReceivingExternalId
                                          ,"orderNo"                .= createReceivingOrderNo
                                          ,"expectedDate"           .= createReceivingExpectedDate
                                          ,"options"                .= createReceivingOptions
                                          ,"arrangement"            .= createReceivingArrangement
                                          ,"shipments"              .= createReceivingShipments
                                          ,"labels"                 .= createReceivingLabels
                                          ,"trackings"              .= createReceivingTrackings
                                          ,"items"                  .= createReceivingItems
                                          ,"shipFrom"               .= createReceivingShipFrom
                                          ,"instructionsRecipients" .= createReceivingInstructionsRecipients]

-- | The expected format is YYYY-MM-DDThh:mm:ssTZD (ISO8601)
-- "2014-05-27T00:00:00-07:00"
newtype ExpectedDateText = ExpectedDateText
  { unExpectedDate :: Text
  } deriving (Eq, Show, ToJSON, FromJSON)

newtype OrderNo = OrderNo
  { unOrderNo :: Text
  } deriving (Eq, Show, ToJSON, FromJSON)

data ReceivingOptions = ReceivingOptions
  { ropWarehouseId         :: Maybe WarehouseId
  , ropWarehouseExternalId :: Maybe WarehouseExternalId
  , ropWarehouseRegion     :: Maybe WarehouseRegion
  } deriving (Eq, Show)

type WarehouseId = Id

type WarehouseExternalId = ExternalId

instance ToJSON ReceivingOptions where
  toJSON ReceivingOptions {..} = omitNulls ["warehouseId"         .= ropWarehouseId
                                           ,"warehouseExternalId" .= ropWarehouseExternalId
                                           ,"warehouseRegion"     .= ropWarehouseRegion]
                                           
data ReceivingArrangement = ReceivingArrangement
  { rapType    :: ArrangementType
  , rapContact :: Maybe Contact
  , rapPhone   :: Maybe Phone
  } deriving (Eq, Show)

instance ToJSON ReceivingArrangement where
  toJSON ReceivingArrangement {..} = omitNulls ["type"    .= rapType
                                               ,"contact" .= rapContact
                                               ,"phone"   .= rapPhone]

newtype Contact = Contact
  { unContact :: Text
  } deriving (Eq, Show, ToJSON, FromJSON)

newtype Phone = Phone
  { unPhone :: Text
  } deriving (Eq, Show, ToJSON, FromJSON)

newtype Type = Type
  { unType :: Text
  } deriving (Eq, Show, ToJSON, FromJSON)

data ArrangementType = ArrangementTypeNone
  | ArrangementTypeOverseas
  | ArrangementTypeLabel
  | ArrangementTypePickup
  deriving (Eq, Show)

instance FromJSON ArrangementType where
  parseJSON = withText "arrangementType" parse
    where
      parse "none"     = pure ArrangementTypeNone
      parse "overseas" = pure ArrangementTypeOverseas
      parse "label"    = pure ArrangementTypeLabel
      parse "pickup"   = pure ArrangementTypePickup
      parse o          = fail ("Unexpected arrangementType value: " <> show o)

instance ToJSON ArrangementType where
  toJSON ArrangementTypeNone     = String "none"
  toJSON ArrangementTypeOverseas = String "overseas"
  toJSON ArrangementTypeLabel    = String "label"
  toJSON ArrangementTypePickup   = String "pickup"

newtype ReceivingShipments = ReceivingShipments
  { rShipments :: [ReceivingShipment]
  } deriving (Eq, Show, ToJSON)

data ReceivingShipment = ReceivingShipment
  { rsiLength :: Maybe Length
  , rsiWidth  :: Maybe Width
  , rsiHeight :: Maybe Height
  , rsiWeight :: Maybe Weight
  , rsiType   :: Type
  } deriving (Eq, Show)

instance ToJSON ReceivingShipment where
  toJSON ReceivingShipment {..} = omitNulls ["length" .= rsiLength
                                            ,"width"  .= rsiWidth
                                            ,"height" .= rsiHeight
                                            ,"weight" .= rsiWeight
                                            ,"type"   .= rsiType]

newtype ReceivingLabels = ReceivingLabels
  { rLabels :: [ReceivingLabel]
  } deriving (Eq, Show, ToJSON)

data ReceivingLabel = ReceivingLabel
  { rlLabelId         :: Maybe LabelId
  , rlOrderId         :: Maybe OrderId
  , rlOrderExternalId :: Maybe OrderExternalId
  } deriving (Eq, Show)

instance ToJSON ReceivingLabel where
  toJSON ReceivingLabel {..} = omitNulls ["labelId"         .= rlLabelId
                                         ,"orderId"         .= rlOrderId
                                         ,"orderExternalid" .= rlOrderExternalId]

newtype ReceivingTrackings = ReceivingTrackings
  { rTrackings :: [ReceivingTracking]
  } deriving (Eq, Show, ToJSON)

data ReceivingTracking = ReceivingTracking
  { rtTracking :: Tracking
  , rtCarrier  :: Maybe CarrierName
  , rtContact  :: Maybe Contact
  , rtPhone    :: Maybe Phone
  } deriving (Eq, Show)

instance ToJSON ReceivingTracking where
  toJSON ReceivingTracking {..} = omitNulls ["tracking" .= rtTracking
                                            ,"carrier"  .= rtCarrier
                                            ,"contact"  .= rtContact
                                            ,"phone"    .= rtPhone]

newtype Tracking = Tracking
  { unTracking :: Text
  } deriving (Eq, Show, ToJSON, FromJSON)

newtype ReceivingItems = ReceivingItems
  { rItems :: [ReceivingItem]
  } deriving (Eq, Show, ToJSON)

data ReceivingItem = ReceivingItem
  { rSku      :: SKU
  , rQuantity :: Quantity
  } deriving (Eq, Show)

instance ToJSON ReceivingItem where
  toJSON ReceivingItem {..} = object ["sku"      .= rSku
                                     ,"quantity" .= rQuantity]

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
  } deriving (Eq, Show)

instance ToJSON ReceivingShipFrom where
  toJSON ReceivingShipFrom {..} = omitNulls ["email"      .= rsfpEmail
                                            ,"name"       .= rsfpName
                                            ,"address1"   .= rsfpAddress1
                                            ,"address2"   .= rsfpAddress2
                                            ,"city"       .= rsfpCity
                                            ,"state"      .= rsfpState
                                            ,"postalCode" .= rsfpPostalCode
                                            ,"country"    .= rsfpCountry
                                            ,"phone"      .= rsfpPhone]

newtype Email = Email
  { unEmail :: Text
  } deriving (Eq, Show, ToJSON, FromJSON)

newtype Name = Name
  { unName :: Text
  } deriving (Eq, Show, ToJSON, FromJSON)

newtype State = State
  { unState :: Text
  } deriving (Eq, Show, ToJSON, FromJSON)

newtype ReceivingInstructionsRecipients = ReceivingInstructionsRecipients
  { rirsInstructionsRecipients :: [ReceivingInstructionsRecipient]
  } deriving (Eq, Show, ToJSON)

newtype Note = Note
  { unNote :: Text
  } deriving (Eq, Show, ToJSON, FromJSON)

data ReceivingInstructionsRecipient = ReceivingInstructionsRecipient
  { rirEmail :: Email
  , rirName  :: Maybe Name
  , rirNote  :: Maybe Note
  } deriving (Eq, Show)

instance ToJSON ReceivingInstructionsRecipient where
  toJSON ReceivingInstructionsRecipient {..} = omitNulls ["email" .= rirEmail
                                                         ,"name"  .= rirName
                                                         ,"note"  .= rirNote]

data ReceivingResponse = ReceivingResponse
  { receivingResponseStatus           :: ResponseStatus
  , receivingResponseMessage          :: ResponseMessage
  , receivingResponseWarnings         :: Maybe ResponseWarnings
  , receivingResponseErrors           :: Maybe ResponseErrors
  , receivingResponseResourceLocation :: Maybe ResponseResourceLocation
  , receivingResponseResource         :: ReceivingResource
  } deriving (Eq, Show)

type ReceivingResource = ReceivingsItemResource

instance FromJSON ReceivingResponse where
  parseJSON = withObject "GetReceivingResponse" parse
    where
      parse o = ReceivingResponse
                <$> o .:  "status"
                <*> o .:  "message"
                <*> o .:? "warnings"
                <*> o .:? "errors"
                <*> o .:? "resourceLocation"
                <*> o .:  "resource"

newtype ReceivingId = ReceivingId
  { unReceivingId :: Text
  } deriving (Eq, Show, FromJSON)

getReceivingId :: ReceivingId -> Text
getReceivingId (ReceivingId x) = x
