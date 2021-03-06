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
  , HighAccuracyEstimates(..)
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
  , IgnoreUnknownSkus(..)
  , Country(..)
  , Currency(..)
  , GroupBy(..)
  , WarehouseArea(..)
  , GenericResponse(..)
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
  , ProductId(..)
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
  , Body(..)
  , Query(..)
  , TupleBS8
  , (-&-)
  , CreateReceivingRequest
  , GetReceivingsRequest
  , UpdatedAfter(..)
  , ReceivingStatusParams(..)
  , ReceivingStatusParam(..)
  , statusParamToBS8
  , OrderNoParam(..)
  , OrderIdParam(..)
  , ExternalIdParam(..)
  , TransactionIdParam(..)
  , ExpandReceivingsParam(..)
  , ExpandReceivings(..)
  , expandReceivingsToBS8
  , CommerceNameParam(..)
  , CreateReceivingResponse
  , GetReceivingsResponse
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
  , ExpectedDate(..)
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
  , OrderNo(..)
  , ReceivingOptions(..)
  , WarehouseId(..)
  , WarehouseExternalId(..)
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
  , ReceivingId(..)
  , getReceivingId
  , ModifyReceivingRequest
  , ModifyReceiving
  , SimpleResponse(..)
  , CancelReceivingRequest
  , CancelReceivingResponse
  , CancelReceivingLabelsRequest
  , CancelReceivingLabelsResponse
  , GetReceivingHoldsRequest
  , IncludeClearedParam(..)
  , GetReceivingInstructionsRecipientsRequest
  , GetReceivingItemsRequest
  , GetReceivingShipmentsRequest
  , GetReceivingTrackingsRequest
  , GetReceivingLabelsRequest
  , GetProductsRequest
  , GetProductsResponse(..)
  , GetProductsResponseResource(..)
  , GetProductsResponseResourceItems(..)
  , GetProductsResponseResourceItem(..)
  , ProductsWrapper(..)
  , BaseProductResponseResource(..)
  , TechnicalData(..)
  , TechnicalDataResource(..)
  , NumberOfBatteries(..)
  , BatteryType(..)
  , NumberOfCells(..)
  , BatteryWeightResponse(..)
  , BatteryWeight(..)
  , CapacityResponse(..)
  , Capacity(..)
  , CapacityUnit(..)
  , StorageConfiguration(..)
  , CountryOfOrigin(..)
  , HsCode(..)
  , CreationDate
  , Flags(..)
  , FlagsResource(..)
  , IsAdult(..)
  , HasInnerPack(..)
  , HasEditRestrictions(..)
  , IsPerishable(..)
  , IsDangerous(..)
  , IsLiquid(..)
  , IsArchivable(..)
  , IsFragile(..)
  , HasMasterCase(..)
  , HasPallet(..)
  , IsDeletable(..)
  , IsMedia(..)
  , Category(..)
  , Status(..)
  , AlternateNamesResponse(..)
  , AlternateNamesResponseResource(..)
  , AlternateNamesResponseResourceItems(..)
  , AlternateNamesResponseResourceItem(..)
  , AlternateNamesResponseResourceItemResource(..)
  , Pallet(..)
  , PalletResource
  , InnerPack(..)
  , InnerPackResource
  , EnqueuedDimensions(..)
  , EnqueuedDimensionsResource(..)
  , ItemCount(..)
  , ArchivedDate
  , Classification(..)
  , BatteryConfiguration(..)
  , BaseProductResponseMasterCase(..)
  , BaseProductResponseMasterCaseResource(..)
  , Dimensions(..)
  , DimensionsResource(..)
  , DimensionsWeight(..)
  , WeightUnit
  , DimensionsHeight(..)
  , HeightUnit
  , DimensionsWidth(..)
  , WidthUnit
  , DimensionsLength(..)
  , LengthUnit
  , ValuesResponse(..)
  , ValuesResource(..)
  , Values(..)
  , CostValueCurrency(..)
  , WholesaleValueCurrency(..)
  , RetailValueCurrency(..)
  , CostValueResponse(..)
  , CostValue(..)
  , IndividualItemsPerCase(..)
  , MasterCaseFlags(..)
  , MasterCaseFlagsResource(..)
  , IsPackagedReadyToShip(..)
  , MarketingInsertResponseResource(..)
  , MarketingInsertMasterCaseResponse(..)
  , MarketingInsertMasterCaseResponseResource(..)
  , InclusionRuleType(..)
  , InclusionRules(..)
  , InclusionRulesResource(..)
  , InsertAfterDate(..)
  , InsertBeforeDate(..)
  , InsertWhenWorthValueResponse(..)
  , InsertWhenWorthValueCurrency
  , InsertWhenQuantity(..)
  , InclusionRulesResourceFlags(..)
  , MarketingInsertFlagsResponse(..)
  , MarketingInsertFlagsResponseResource(..)
  , VirtualKitResponseResource(..)
  , VirtualKitResponseContent(..)
  , VirtualKitContentResource(..)
  , VirtualKitContentResourceItems(..)
  , VirtualKitContentResourceItem(..)
  , VirtualKitContentResourceItemResource(..)
  , VirtualKitFlags(..)
  , KitResponseResource(..)
  , KitResponseMasterCase
  , KitResponseTechnicalData(..)
  , KitResponseTechnicalDataResource(..)
  , KitResponseTechnicalDataResourceBattery(..)
  , KitTechnicalDataResourceBatteryResource
  , KitResponseContent
  , ExpandProductsParam(..)
  , ExpandProducts(..)
  , expandProductsToBS8
  , ClassificationParam(..)
  , classificationToBS
  , DescriptionParam(..)
  , IncludeArchived(..)
  , includeArchivedToBS
  , ProductStatusParams(..)
  , ProductStatusParam(..)
  , productStatusParamToBS
  , FlowParams(..)
  , FlowParam(..)
  , flowParamToBS
  , IdsParam(..)
  , SkusParam(..)
  , CreateProductsRequest
  , CreateProductsResponse
  , CreateProductsWrapper(..)
  , BaseProduct(..)
  , BaseProductPallet(..)
  , BaseProductMasterCase(..)
  , BaseProductInnerPack(..)
  , BaseProductAlternateNames(..)
  , BaseProductAlternateName(..)
  , BaseProductFlags(..)
  , BaseProductPalletFlags(..)
  , BaseProductMasterCaseFlags(..)
  , BaseProductInnerPackFlags(..)
  , BaseProductTechnicalData(..)
  , BaseProductTechnicalDataBattery(..)
  , BaseProductDimensions(..)
  , BaseProductLength(..)
  , BaseProductWidth(..)
  , BaseProductHeight(..)
  , BaseProductWeight(..)
  , RetailCurrency(..)
  , WholesaleCurrency(..)
  , RetailValue(..)
  , RetailValueResponse(..)
  , WholesaleValue(..)
  , WholesaleValueResponse(..)
  , MarketingInsert(..)
  , MarketingInsertAlternateNames(..)
  , MarketingInsertAlternateName(..)
  , MarketingInsertDimensions(..)
  , MarketingInsertLength(..)
  , MarketingInsertWidth(..)
  , MarketingInsertHeight(..)
  , MarketingInsertWeight(..)
  , MarketingInsertInclusionRules(..)
  , ShouldNotFold(..)
  , MarketingInsertFlags(..)
  , InsertWhenWorthValue(..)
  , InsertWhenWorthCurrency(..)
  , MarketingInsertMasterCase(..)
  , MarketingInsertMasterCaseDimensions(..)
  , MarketingInsertMasterCaseDimensionsHeight(..)
  , MarketingInsertMasterCaseDimensionsLength(..)
  , MarketingInsertMasterCaseDimensionsWidth(..)
  , MarketingInsertMasterCaseDimensionsWeight(..)
  , ProductError(..)
  , VirtualKit(..)
  , VirtualKitContent(..)
  , VirtualKitContentObject(..)
  , Kit(..)
  , KitPallet(..)
  , KitPalletFlags(..)
  , KitMasterCase(..)
  , KitMasterCaseFlags(..)
  , KitInnerPack(..)
  , KitInnerPackFlags(..)
  , KitFlags(..)
  , HasBattery(..)
  , KitTechnicalData(..)
  , KitTechnicalDataBattery(..)
  , KitDimensions(..)
  , KitLength(..)
  , KitWidth(..)
  , KitHeight(..)
  , KitWeight(..)
  , KitContent(..)
  , KitContentObject(..)
  , KitAlternateNames(..)
  , KitAlternateName(..)
  , KitValues(..)
  , utcToShipwire
  , RetireProductsRequest
  , RetireProductsResponse(..)
  , ProductsToRetire(..)
  , Message(..)
  , MoreInfo(..)
  , MoreInfoItems(..)
  , MoreInfoItem(..)
  , Configuration(..)
  , Success(..)
  , ModifyProductsRequest
  , ModifyProductsResponse(..)
  , GetProductRequest
  , GetProductResponseResource(..)
  , ModifyProductRequest
  , CreateOrderRequest
  , CreateOrder(..)
  , OrderItem(..)
  , CommercialInvoiceValueCurrency(..)
  , CommercialInvoiceValue(..)
  , OrderItems(..)
  , PackingList(..)
  , PackingListMessage(..)
  , PackingListOther(..)
  , MessageBody(..)
  , MessageHeader(..)
  , MessageDocument(..)
  , MessageLocation(..)
  , CommercialInvoice(..)
  , AdditionalValue(..)
  , AdditionalValueCurrency(..)
  , InsuranceValue(..)
  , InsuranceValueCurrency(..)
  , ShippingValue(..)
  , ShippingValueCurrency(..)
  , OrderShipTo(..)
  , OrderShipFrom(..)
  , Company(..)
  , ShipFromCompany(..)
  , CreateOrderOptions(..)
  , Server(..)
  , HoldReason(..)
  , Hold(..)
  , DiscountCode(..)
  , Affiliate(..)
  , Referrer(..)
  , ForceAddress(..)
  , ForceDuplicate(..)
  , SameDay(..)
  , ProcessAfterDate(..)
  , GetOrdersRequest
  , GetOrdersResponseResource(..)
  , GetOrdersResponseResourceItems(..)
  , GetOrdersResponseResourceItemResource(..)
  , SplitOrdersResponse(..)
  , SplitOrdersResponseResource(..)
  , SplitOrdersResponseResourceItems(..)
  , ShipwireAnywhereResponse(..)
  , ShipwireAnywhereResponseResource(..)
  , PricingEstimateResponse(..)
  , PricingResponse(..)
  , PricingEstimateResource
  , RoutingResponseResource(..)
  , DestinationLatitudeResponse(..)
  , DestinationLongitudeResponse(..)
  , OriginLatitudeResponse
  , OriginLongitudeResponse
  , ShippingLabelResponse(..)
  , ShippingLabelResponseResource(..)
  , PackingListResponse(..)
  , PackingListResponseResource(..)
  , PackingListResponseResourceOther(..)
  , PackingListResponseResourceOtherResource
  , PackingListResponseResourceMessage(..)
  , CommercialInvoiceResponse(..)
  , CommercialInvoiceResponseResource(..)
  , DocumentLocation(..)
  , OrderShipToResponse(..)
  , OrderShipToResponseResource
  , OrderShipFromResponse(..)
  , OrderShipFromResponseResource(..)
  , GetOrdersOptions(..)
  , GetOrdersOptionsResource(..)
  , GetOrdersReturns(..)
  , GetOrdersReturnsResource(..)
  , GetOrdersReturnsResourceItems(..)
  , GetOrdersTrackings(..)
  , GetOrdersTrackingsResource(..)
  , GetOrdersTrackingsResourceItems(..)
  , GetOrdersTrackingsResourceItem(..)
  , GetOrdersTrackingsResourceItemResource(..)
  , FirstScanRegion
  , FirstScanPostalCode
  , FirstScanCountry
  , DeliveryCity
  , DeliveryRegion
  , DeliveryPostalCode
  , DeliveryCountry
  , FirstScanDate(..)
  , LabelCreatedDate(..)
  , GetOrdersItems(..)
  , GetOrdersItemsResource(..)
  , GetOrdersItemsResourceItems(..)
  , GetOrdersItemsResourceItem(..)
  , GetOrdersItemsResourceItemResource(..)
  , Shipped(..)
  , Shipping(..)
  , Reserved(..)
  , Backordered(..)
  , Ordered(..)
  , SerialNumbersResponse(..)
  , SerialNumbersResponseResource(..)
  , SerialNumbersResponseResourceItems(..)
  , SerialNumbersResponseResourceItem(..)
  , SerialNumbersResponseResourceItemResource(..)
  , SerialNumber(..)
  , GetOrdersHolds(..)
  , GetOrdersHoldsResource(..)
  , GetOrdersHoldsResourceItems(..)
  , GetOrdersHoldsResourceItem(..)
  , GetOrdersHoldsResourceItemResource(..)
  , NeedsReview(..)
  , EventsResponse
  , GetOrdersResponseResourceItem(..)
  , PricingResponseResource(..)
  , TotalValue(..)
  , HandlingValue(..)
  , PackagingValue(..)
  , RoutingResponse(..)
  , ExpandOrders(..)
  , ExpandOrdersParam(..)
  , expandOrdersToBS8
  , OrderStatusParam(..)
  , OrderStatus(..)
  , ReferrerParam(..)
  , CancelOrderRequest
  , CancelOrderResponse
  , IdWrapper(..)
  , GetOrderTrackingsRequest
  , GetOrderTrackingsResponseResource(..)
  , GetOrderTrackingsResponseResourceItems(..)
  , GetOrderTrackingsResponseResourceItem(..)
  , GetOrderTrackingsResponseResourceItemResource(..)
  , ValidateAddressRequest
  , ValidateAddressResponse(..)
  , ValidateAddressResponseResource
  , AddressToValidate
  , ValidateAddressWarnings(..)
  , WarningObject(..)
  , GetOrderRequest
  , GetOrderResponseResource
  , ExtendedAttributesResponse(..)
  , ExtendedAttributesResponseResource(..)
  , ExtendedAttributesResponseResourceItems(..)
  , ExtendedAttributesResponseResourceItem(..)
  , ExtendedAttributesResponseResourceItemResource(..)
  , ExtendedAttributeValue(..)
  , FreightSummaryResponse(..)
  , FreightSummaryResponseResource(..)
  , MeasurementType(..)
  , FreightSummaryTotalWeight(..)
  , ErrorObject(..)
  , ValidateAddressErrors(..)
  , InnerErrorObject(..)
  , InnerErrorObjectRules(..)
  , InnerWarningObject(..)
  , InnerWarningObjectRules(..)
  ) where

import           Control.Applicative ((<|>))
import           Data.Aeson
import           Data.Aeson.Types (Parser)
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy.Char8 as BSL
import           Data.Fixed
import qualified Data.HashMap.Lazy as HM
import           Data.Monoid ((<>))
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import           Data.Time.Clock
import           Data.Time.LocalTime
import qualified Data.Vector as V
import           Network.HTTP.Client
import qualified Network.HTTP.Types.Method as NHTM
import           System.Environment (getEnv)

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
  , params   :: Params TupleBS8 BSL.ByteString -- ^ Request params of ShipwireRequest
  }

mkShipwireRequest :: Method
                  -> Text
                  -> Params TupleBS8 BSL.ByteString
                  -> ShipwireRequest a b c
mkShipwireRequest m e p = ShipwireRequest m e p

type family ShipwireReturn a :: *

---------------------------------------------------------------
-- Rate Endpoint https://www.shipwire.com/w/developers/rate/ --
---------------------------------------------------------------

data RateRequest
type instance ShipwireReturn RateRequest = GenericResponse RateResource

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
  { rateOptionCurrency              :: Currency
  , rateOptionGroupBy               :: GroupBy
  , rateOptionWarehouseId           :: Maybe WarehouseId
  , rateOptionWarehouseExternalId   :: Maybe WarehouseExternalId
  , rateOptionWarehouseRegion       :: Maybe WarehouseRegion
  , rateOptionIgnoreUnknownSkus     :: Maybe IgnoreUnknownSkus
  , rateOptionCanSplit              :: CanSplit
  , rateOptionWarehouseArea         :: WarehouseArea
  , rateOptionChannelName           :: Maybe ChannelName
  , rateOptionExpectedShipDate      :: Maybe ExpectedShipDate
  , rateOptionHighAccuracyEstimates :: Maybe HighAccuracyEstimates
  } deriving (Eq, Show)

data HighAccuracyEstimates =
    HighAccuracyEstimates
  | NoHighAccuracyEstimates
  deriving (Eq, Show)

instance ToJSON HighAccuracyEstimates where
  toJSON HighAccuracyEstimates   = Number 1
  toJSON NoHighAccuracyEstimates = Number 0

data IgnoreUnknownSkus = IgnoreUnknownSkus
  | DontIgnoreUnknownSkus
  deriving (Eq, Show)

instance ToJSON IgnoreUnknownSkus where
  toJSON IgnoreUnknownSkus     = Number 1
  toJSON DontIgnoreUnknownSkus = Number 0

instance ToJSON RateOptions where
  toJSON RateOptions {..} = omitNulls ["currency"              .= rateOptionCurrency
                                      ,"groupBy"               .= rateOptionGroupBy
                                      ,"warehouseId"           .= rateOptionWarehouseId
                                      ,"warehouseExternalId"   .= rateOptionWarehouseExternalId
                                      ,"warehouseRegion"       .= rateOptionWarehouseRegion
                                      ,"ignoreUnknownSkus"     .= rateOptionIgnoreUnknownSkus
                                      ,"canSplit"              .= rateOptionCanSplit
                                      ,"warehouseArea"         .= rateOptionWarehouseArea
                                      ,"channelName"           .= rateOptionChannelName
                                      ,"expectedShipDate"      .= rateOptionExpectedShipDate
                                      ,"highAccuracyEstimates" .= rateOptionHighAccuracyEstimates]

data CanSplit
  = CanSplit
  | CanNotSplit
  deriving (Eq, Show)

instance ToJSON CanSplit where
  toJSON CanSplit    = Number 1
  toJSON CanNotSplit = Number 0

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

instance FromJSON IsCommercial where
  parseJSON (Number 1) = pure Commercial
  parseJSON (Number 0) = pure NotCommercial
  parseJSON o          = fail $ "Unexpected IsCommercial: " <> show o

data IsPoBox
  = PoBox
  | NotPoBox
  deriving (Eq, Show)

instance ToJSON IsPoBox where
  toJSON PoBox    = Number 1
  toJSON NotPoBox = Number 0

instance FromJSON IsPoBox where
  parseJSON (Number 1) = pure PoBox
  parseJSON (Number 0) = pure NotPoBox
  parseJSON o          = fail $ "Unexpected IsPoBox: " <> show o

type Items = [ItemInfo]

newtype ItemInfo =
  ItemInfo (SKU, Quantity)
  deriving (Eq, Show)

instance ToJSON ItemInfo where
  toJSON (ItemInfo (sku, q)) = object ["sku"      .= sku
                                      ,"quantity" .= q]

newtype Quantity = Quantity
  { unQuantity :: Integer
  } deriving (Eq, Show, ToJSON, FromJSON)

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
  } deriving (Eq, Show, ToJSON, FromJSON)

newtype Country = Country
  { unCountry :: Text
  } deriving (Eq, Show, ToJSON, FromJSON)

data Currency =
  USD
  deriving (Eq, Show)

instance ToJSON Currency where
  toJSON USD = String "USD"

instance FromJSON Currency where
  parseJSON = withText "Currency" parse
    where
      parse "USD" = pure USD
      parse o     = fail $ "Unexpected Currency: " <> show o

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
  parseJSON = withText "GroupBy" parse
    where
      parse "all"       = pure GroupByAll
      parse "warehouse" = pure GroupByWarehouse
      parse o           = fail $ "Unexpected GroupBy: " <> show o

newtype WarehouseArea = WarehouseArea
  { unWarehouseArea :: Text
  } deriving (Eq, Show, ToJSON, FromJSON)

data GenericResponse a = GenericResponse
  { genericResponseStatus           :: ResponseStatus
  , genericResponseMessage          :: ResponseMessage
  , genericResponseWarnings         :: Maybe ResponseWarnings
  , genericResponseErrors           :: Maybe ResponseErrors
  , genericResponseResourceLocation :: Maybe ResponseResourceLocation
  , genericResponseResource         :: Maybe a
  } deriving (Eq, Show)

instance FromJSON a => FromJSON (GenericResponse a) where
  parseJSON = withObject "GenericResponse" parse
    where
      parse o = GenericResponse
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
  parseJSON = withText "WarningType" parse
    where
      parse "warning" = pure WarningWarning
      parse "error"   = pure WarningError
      parse o         = fail $ "Unexpected WarningType: " <> show o

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

data ErrorCode = ErrorCodeText Text
  | ErrorCodeInteger Integer
  deriving (Eq, Show)

instance FromJSON ErrorCode where
  parseJSON o = (ErrorCodeText <$> parseJSON o) <|> (ErrorCodeInteger <$> parseJSON o)

newtype ErrorMessage = ErrorMessage
  { unErrorMessage :: Text
  } deriving (Eq, Show, FromJSON)

data ErrorType
  = ErrorWarning
  | ErrorError
  deriving (Eq, Show)

instance FromJSON ErrorType where
  parseJSON = withText "ErrorType" parse
    where
      parse "warning" = pure ErrorWarning
      parse "error"   = pure ErrorError
      parse o         = fail $ "Unexpected ErrorType: " <> show o

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

instance ToJSON ServiceLevelCode where
  toJSON DomesticGround        = String "GD"
  toJSON DomesticTwoDay        = String "2D"
  toJSON DomesticOneDay        = String "1D"
  toJSON InternationalEconomy  = String "E-INTL"
  toJSON InternationalStandard = String "INTL"
  toJSON InternationalPlus     = String "PL-INTL"
  toJSON InternationalPremium  = String "PM-INTL"

instance FromJSON ServiceLevelCode where
  parseJSON = withText "ServiceLevelCode" parse
    where
      parse "GD"      = pure DomesticGround
      parse "2D"      = pure DomesticTwoDay
      parse "1D"      = pure DomesticOneDay
      parse "E-INTL"  = pure InternationalEconomy
      parse "INTL"    = pure InternationalStandard
      parse "PL-INTL" = pure InternationalPlus
      parse "PM-INTL" = pure InternationalPremium
      parse o         = fail $ "Unexpected ServiceLevelCode: " <> show o

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

instance ToJSON ExpectedShipDate where
  toJSON (ExpectedShipDate x) = object ["expectedShipDate" .= utcToShipwire x]

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
  } deriving (Eq, Show, FromJSON, ToJSON)

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
  } deriving (Eq, Show, ToJSON, FromJSON)

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

-----------------------------------------------------------------
-- Stock Endpoint https://www.shipwire.com/w/developers/stock/ --
-----------------------------------------------------------------

data StockRequest
type instance ShipwireReturn StockRequest = GenericResponse StockResource

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
  } deriving (Eq, Show, ToJSON, FromJSON)

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
  { sirProductId           :: Maybe ProductId
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

newtype ProductId = ProductId
  { unProductId :: Integer
  } deriving (Eq, Show, ToJSON, FromJSON)

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
  parseJSON o          = fail $ "Unexpected isBundle: " <> show o

data IsAlias
  = Alias
  | NotAlias
  deriving (Eq, Show)

instance FromJSON IsAlias where
  parseJSON (Number 1) = pure Alias
  parseJSON (Number 0) = pure NotAlias
  parseJSON o          = fail $ "Unexpected isAlias: " <> show o

-- | Either production or sandbox API host
type Host = Text

data ShipwireHost =
    ShipwireProduction
  | ShipwireSandbox
  deriving (Eq, Show)

hostUri :: ShipwireHost -> Text
hostUri ShipwireProduction = "https://api.shipwire.com/api/v3"
hostUri ShipwireSandbox    = "https://api.beta.shipwire.com/api/v3"

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

newtype Query = Query {
  unQuery :: TupleBS8
  } deriving (Eq, Show)

newtype Body = Body {
  unBody :: BSL.ByteString
  } deriving (Eq, Show)

-- | Parameters for each request which include both the query and the body of a
-- request
data Params b c = Params
  { paramsBody :: Maybe Body
  , paramsQuery :: [Query]
  } deriving Show

joinQueryParams :: Params b c -> Params b c -> Params b c
joinQueryParams (Params _ xs) (Params b ys) = Params b (xs ++ ys)

-- | Type alias for query parameters
type TupleBS8 = (BS8.ByteString, BS8.ByteString)

-- | Convert a parameter to a key/value
class ToShipwireParam param where
  toShipwireParam :: param -> Params TupleBS8 c -> Params TupleBS8 c

instance ToShipwireParam SKU where
  toShipwireParam (SKU i) =
    joinQueryParams $ Params Nothing [Query ("sku", TE.encodeUtf8 i)]

instance ToShipwireParam ParentId where
  toShipwireParam (ParentId i) =
    joinQueryParams $ Params Nothing [Query ("parentId", TE.encodeUtf8 i)]

instance ToShipwireParam ProductIdParam where
  toShipwireParam (ProductIdParam xs) =
    joinQueryParams $ Params Nothing [Query ("productId", TE.encodeUtf8 (T.intercalate "," xs))]

instance ToShipwireParam ProductExternalIdParam where
  toShipwireParam (ProductExternalIdParam xs) =
    joinQueryParams $ Params Nothing [Query ("productExternalId", TE.encodeUtf8 (T.intercalate "," xs))]

instance ToShipwireParam WarehouseIdParam where
  toShipwireParam (WarehouseIdParam xs) =
    joinQueryParams $ Params Nothing [Query ("warehouseId", TE.encodeUtf8 (T.intercalate "," xs))]

instance ToShipwireParam WarehouseExternalIdParam where
  toShipwireParam (WarehouseExternalIdParam xs) =
    joinQueryParams $ Params Nothing [Query ("warehouseExternalId", TE.encodeUtf8 (T.intercalate "," xs))]

instance ToShipwireParam WarehouseRegionParam where
  toShipwireParam (WarehouseRegionParam xs) =
    joinQueryParams $ Params Nothing [Query ("warehouseRegion", TE.encodeUtf8 (T.intercalate "," xs))]

instance ToShipwireParam WarehouseAreaParam where
  toShipwireParam (WarehouseAreaParam xs) =
    joinQueryParams $ Params Nothing [Query ("warehouseArea", TE.encodeUtf8 (T.intercalate "," xs))]

instance ToShipwireParam ChannelName where
  toShipwireParam (ChannelName n) =
    joinQueryParams $ Params Nothing [Query ("channelName", TE.encodeUtf8 n)]

instance ToShipwireParam IncludeEmpty where
  toShipwireParam (IncludeEmpty b) =
    joinQueryParams $ Params Nothing [Query ("includeEmpty", TE.encodeUtf8 $ (T.pack . show) b)]

instance ToShipwireParam VendorIdParam where
  toShipwireParam (VendorIdParam vi) =
    joinQueryParams $ Params Nothing [Query ("vendorId", TE.encodeUtf8 $ T.intercalate "," $ map (T.pack . show) vi)]

instance ToShipwireParam VendorExternalIdParam where
  toShipwireParam (VendorExternalIdParam vei) =
    joinQueryParams $ Params Nothing [Query ("vendorExternalId", TE.encodeUtf8 $ T.intercalate "," $ map (T.pack . show) vei)]

instance ToShipwireParam DisableAutoBreakLots where
  toShipwireParam (DisableAutoBreakLots d) =
    joinQueryParams $ Params Nothing [Query ("disableAutoBreakLots", TE.encodeUtf8 d)]

instance ToShipwireParam Mode where
  toShipwireParam m =
    joinQueryParams $ Params Nothing [Query ("mode", modeToBS8 m)]

instance ToShipwireParam IncludeEmptyShipwireAnywhere where
  toShipwireParam (IncludeEmptyShipwireAnywhere i) =
    joinQueryParams $ Params Nothing [Query ("includeEmptyShipwireAnywhere", TE.encodeUtf8 i)]

instance ToShipwireParam Offset where
  toShipwireParam (Offset o) =
    joinQueryParams $ Params Nothing [Query ("offset", TE.encodeUtf8 $ (T.pack . show) o)]

instance ToShipwireParam Total where
  toShipwireParam (Total t) =
    joinQueryParams $ Params Nothing [Query ("total", TE.encodeUtf8 $ (T.pack . show) t)]

instance ToShipwireParam Previous where
  toShipwireParam (Previous p) =
    joinQueryParams $ Params Nothing [Query ("previous", TE.encodeUtf8 p)]

instance ToShipwireParam Next where
  toShipwireParam (Next n) =
    joinQueryParams $ Params Nothing [Query ("next", TE.encodeUtf8 n)]

instance ToShipwireParam Limit where
  toShipwireParam (Limit l) =
    joinQueryParams $ Params Nothing [Query ("limit", TE.encodeUtf8 $ (T.pack . show) l)]

class (ToShipwireParam param) => ShipwireHasParam request param where

-- | Add an optional query parameter
(-&-)
  :: ShipwireHasParam r param
  => ShipwireRequest r b c -> param -> ShipwireRequest r b c
stripeRequest -&- param =
  stripeRequest
  { params = toShipwireParam param (params stripeRequest)
  }

---------------------------------------------------------------------------
-- Receiving Endpoint -- https://www.shipwire.com/w/developers/receiving --
---------------------------------------------------------------------------

-- | GET /api/v3/receivings
data GetReceivingsRequest
type instance ShipwireReturn GetReceivingsRequest = GenericResponse ReceivingsResource

type CreateReceivingResponse = GenericResponse ReceivingsResource

instance ShipwireHasParam GetReceivingsRequest ExpandReceivingsParam
instance ShipwireHasParam GetReceivingsRequest CommerceNameParam
instance ShipwireHasParam GetReceivingsRequest TransactionIdParam
instance ShipwireHasParam GetReceivingsRequest ExternalIdParam
instance ShipwireHasParam GetReceivingsRequest OrderIdParam
instance ShipwireHasParam GetReceivingsRequest OrderNoParam
instance ShipwireHasParam GetReceivingsRequest ReceivingStatusParams
instance ShipwireHasParam GetReceivingsRequest UpdatedAfter
instance ShipwireHasParam GetReceivingsRequest WarehouseIdParam
instance ShipwireHasParam GetReceivingsRequest WarehouseExternalIdParam

-- | POST /api/v3/receivings
data CreateReceivingRequest
type instance ShipwireReturn CreateReceivingRequest = GenericResponse ReceivingsResource

type GetReceivingsResponse = GenericResponse ReceivingsResource

instance ShipwireHasParam CreateReceivingRequest ExpandReceivingsParam

-- | GET /api/v3/receivings/{id}

data GetReceivingRequest
type instance ShipwireReturn GetReceivingRequest = GenericResponse ReceivingResource

instance ShipwireHasParam GetReceivingRequest ExpandReceivingsParam

-- | PUT /api/v3/receivings/{id}

data ModifyReceivingRequest
type instance ShipwireReturn ModifyReceivingRequest = GenericResponse ReceivingsResource

instance ShipwireHasParam ModifyReceivingRequest ExpandReceivingsParam

-- | POST /api/v3/receivings/{id}/cancel

data CancelReceivingRequest
type instance ShipwireReturn CancelReceivingRequest = CancelReceivingResponse

-- | POST /api/v3/receivings/{id}/labels/cancel

data CancelReceivingLabelsRequest
type instance ShipwireReturn CancelReceivingLabelsRequest = CancelReceivingLabelsResponse

-- | GET /api/v3/receivings/{id}/holds

data GetReceivingHoldsRequest
type instance ShipwireReturn GetReceivingHoldsRequest = GenericResponse ItemResourceHoldsResource

instance ShipwireHasParam GetReceivingHoldsRequest IncludeClearedParam

-- | GET /api/v3/receivings/{id}/instructionsRecipients

data GetReceivingInstructionsRecipientsRequest
type instance ShipwireReturn GetReceivingInstructionsRecipientsRequest = GenericResponse ItemResourceInstructionsRecipientsResource

-- | GET /api/v3/receivings/{id}/items

data GetReceivingItemsRequest
type instance ShipwireReturn GetReceivingItemsRequest = GenericResponse ItemResourceItemsResource

-- | GET /api/v3/receivings/{id}/shipments

data GetReceivingShipmentsRequest
type instance ShipwireReturn GetReceivingShipmentsRequest = GenericResponse ItemResourceShipmentsResource

-- | GET /api/v3/receivings/{id}/trackings

data GetReceivingTrackingsRequest
type instance ShipwireReturn GetReceivingTrackingsRequest = GenericResponse ItemResourceTrackingsResource

-- | GET /api/v3/receivings/{id}/labels

data GetReceivingLabelsRequest
type instance ShipwireReturn GetReceivingLabelsRequest = GenericResponse ItemResourceLabelsResource

-- | ISO 8601 format, ex: "2014-05-30T13:08:29-07:00"
newtype UpdatedAfter = UpdatedAfter
  { updatedAfter :: UTCTime
  } deriving (Eq, Show)

instance ToShipwireParam UpdatedAfter where
  toShipwireParam (UpdatedAfter x) =
    joinQueryParams $ Params Nothing [Query ("updatedAfter", TE.encodeUtf8 $ utcToShipwire x)]

newtype ReceivingStatusParams = ReceivingStatusParams
  { statusParam :: [ReceivingStatusParam]
  } deriving (Eq, Show)

data ReceivingStatusParam = StatusProcessed
  | StatusCanceled
  | StatusCompleted
  | StatusDelivered
  | StatusReturned
  | StatusSubmitted
  | StatusHeld
  | StatusTracked
  deriving (Eq, Show)

statusParamToBS8 :: ReceivingStatusParam -> BS8.ByteString
statusParamToBS8 StatusProcessed = "processed"
statusParamToBS8 StatusCanceled  = "canceled"
statusParamToBS8 StatusCompleted = "completed"
statusParamToBS8 StatusDelivered = "delivered"
statusParamToBS8 StatusReturned  = "returned"
statusParamToBS8 StatusSubmitted = "submitted"
statusParamToBS8 StatusHeld      = "held"
statusParamToBS8 StatusTracked   = "tracked"

instance ToShipwireParam ReceivingStatusParams where
  toShipwireParam (ReceivingStatusParams xs) =
    joinQueryParams $ Params Nothing [Query ("status", (BS8.intercalate "," (map statusParamToBS8 xs)))]

newtype OrderNoParam = OrderNoParam
  { orderNoParam :: [Text]
  } deriving (Eq, Show)

instance ToShipwireParam OrderNoParam where
  toShipwireParam (OrderNoParam xs) =
    joinQueryParams $ Params Nothing [Query ("orderNo", TE.encodeUtf8 (T.intercalate "," xs))]

newtype OrderIdParam = OrderIdParam
  { orderIdParam :: [Text]
  } deriving (Eq, Show)

instance ToShipwireParam OrderIdParam where
  toShipwireParam (OrderIdParam xs) =
    joinQueryParams $ Params Nothing [Query ("orderId", TE.encodeUtf8 (T.intercalate "," xs))]

newtype ExternalIdParam = ExternalIdParam
  { externalIdParam :: [Text]
  } deriving (Eq, Show)

instance ToShipwireParam ExternalIdParam where
  toShipwireParam (ExternalIdParam xs) =
    joinQueryParams $ Params Nothing [Query ("externalId", TE.encodeUtf8 (T.intercalate "," xs))]

newtype TransactionIdParam = TransactionIdParam
  { transactionIdParam :: [Text]
  } deriving (Eq, Show)

instance ToShipwireParam TransactionIdParam where
  toShipwireParam (TransactionIdParam xs) =
    joinQueryParams $ Params Nothing [Query ("transactionId", TE.encodeUtf8 (T.intercalate "," xs))]

newtype ExpandReceivingsParam = ExpandReceivingsParam
  { expandReceivingsParam :: [ExpandReceivings]
  } deriving (Eq, Show)

data ExpandReceivings = ExpandHolds
  | ExpandInstructionsRecipients
  | ExpandItems
  | ExpandShipments
  | ExpandLabels
  | ExpandTrackings
  | ExpandAll
  deriving (Eq, Show)

expandReceivingsToBS8 :: ExpandReceivings -> BS8.ByteString
expandReceivingsToBS8 ExpandHolds                  = "holds"
expandReceivingsToBS8 ExpandInstructionsRecipients = "instructionsRecipients"
expandReceivingsToBS8 ExpandItems                  = "items"
expandReceivingsToBS8 ExpandShipments              = "shipments"
expandReceivingsToBS8 ExpandLabels                 = "labels"
expandReceivingsToBS8 ExpandTrackings              = "trackings"
expandReceivingsToBS8 ExpandAll                    = "all"

instance ToShipwireParam ExpandReceivingsParam where
  toShipwireParam (ExpandReceivingsParam xs) =
    joinQueryParams $ Params Nothing [Query ("expand", BS8.intercalate "," (map expandReceivingsToBS8 xs))]

newtype CommerceNameParam = CommerceNameParam
  { commerceNameParam :: [Text]
  } deriving (Eq, Show)

instance ToShipwireParam CommerceNameParam where
  toShipwireParam (CommerceNameParam ns) =
    joinQueryParams $ Params Nothing [Query ("commerceName", TE.encodeUtf8 (T.intercalate "," ns))]

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
  , rirExpectedDate           :: Maybe ExpectedDate
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
  , irirrNext     :: Maybe ResponseNext
  , irirrOffset   :: ResponseOffset
  , irirrPrevious :: Maybe ResponsePrevious
  , irirrTotal    :: ResponseTotal
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

newtype ExpectedDate = ExpectedDate
  { unExpectedDate :: UTCTime
  } deriving (Eq, Show, FromJSON)

instance ToJSON ExpectedDate where
  toJSON (ExpectedDate x) = object ["expectedDate" .= utcToShipwire x]

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
  , irerProcessedDate        :: Maybe ProcessedDate
  , irerCompletedDate        :: Maybe CompletedDate
  , irerExpectedDate         :: Maybe ExpectedDate
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
                <*> o .:? "processedDate"
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
  { irorWarehouseExternalId :: Maybe WarehouseExternalId
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
  , irlrNext     :: Maybe ResponseNext
  , irlrOffset   :: ResponseOffset
  , irlrPrevious :: Maybe ResponsePrevious
  , irlrTotal    :: ResponseTotal
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
  , irsrNext     :: Maybe ResponseNext
  , irsrOffset   :: ResponseOffset
  , irsrPrevious :: Maybe ResponsePrevious
  , irsrTotal    :: ResponseTotal
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
  , irtrNext     :: Maybe ResponseNext
  , irtrOffset   :: ResponseOffset
  , irtrPrevious :: Maybe ResponsePrevious
  , irtrTotal    :: ResponseTotal
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
  , irirNext     :: Maybe ResponseNext
  , irirOffset   :: ResponseOffset
  , irirPrevious :: Maybe ResponsePrevious
  , irirTotal    :: ResponseTotal
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
  , irhrNext     :: Maybe ResponseNext
  , irhrOffset   :: ResponseOffset
  , irhrPrevious :: Maybe ResponsePrevious
  , irhrTotal    :: ResponseTotal
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
  } deriving (Eq, Show, ToJSON, FromJSON)

type ClearedDate = ExpectedShipDate

type AppliedDate = ExpectedShipDate

newtype ItemStatus = ItemStatus
  { unItemStatus :: Text
  } deriving (Eq, Show)

newtype CommerceName = CommerceName
  { unCommerceName :: Text
  } deriving (Eq, Show, FromJSON, ToJSON)

type TransactionId = ExternalId

data CreateReceiving = CreateReceiving
  { createReceivingExternalId             :: Maybe ExternalId
  , createReceivingOrderNo                :: Maybe OrderNo
  , createReceivingExpectedDate           :: Maybe ExpectedDate
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

newtype OrderNo = OrderNo
  { unOrderNo :: Text
  } deriving (Eq, Show, ToJSON, FromJSON)

data ReceivingOptions = ReceivingOptions
  { ropWarehouseId         :: Maybe WarehouseId
  , ropWarehouseExternalId :: Maybe WarehouseExternalId
  , ropWarehouseRegion     :: Maybe WarehouseRegion
  } deriving (Eq, Show)

newtype WarehouseId = WarehouseId
  { unWarehouseId :: Integer
  } deriving (Eq, Show, ToJSON, FromJSON)

newtype WarehouseExternalId = WarehouseExternalId
  { unWarehouseExternalId :: Text
  } deriving (Eq, Show, ToJSON, FromJSON)

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
  parseJSON = withText "ArrangementType" parse
    where
      parse "none"     = pure ArrangementTypeNone
      parse "overseas" = pure ArrangementTypeOverseas
      parse "label"    = pure ArrangementTypeLabel
      parse "pickup"   = pure ArrangementTypePickup
      parse o          = fail $ "Unexpected ArrangementType: " <> show o

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

type ReceivingResource = ReceivingsItemResource

newtype ReceivingId = ReceivingId
  { unReceivingId :: Text
  } deriving (Eq, Show, FromJSON)

getReceivingId :: ReceivingId -> Text
getReceivingId (ReceivingId x) = x

type ModifyReceiving = CreateReceiving

data SimpleResponse = SimpleResponse
  { status           :: ResponseStatus
  , resourceLocation :: Maybe ResponseResourceLocation
  , message          :: ResponseMessage
  , warnings         :: Maybe ResponseWarnings
  , errors           :: Maybe ResponseErrors
  } deriving (Eq, Show)

instance FromJSON SimpleResponse where
  parseJSON = withObject "CancelReceivingResponse" parse
    where
      parse o = SimpleResponse
                <$> o .:  "status"
                <*> o .:? "resourceLocation"
                <*> o .:  "message"
                <*> o .:? "warnings"
                <*> o .:? "errors"

type CancelReceivingResponse = SimpleResponse

type CancelReceivingLabelsResponse = CancelReceivingResponse

data IncludeClearedParam
  = IncludeCleared
  | DontIncludeCleared
  deriving (Eq, Show)

instance ToShipwireParam IncludeClearedParam where
  toShipwireParam IncludeCleared =
    joinQueryParams $ Params Nothing [Query ("includeCleared", TE.encodeUtf8 $ (T.pack . show) (1 :: Int))]
  toShipwireParam DontIncludeCleared =
    joinQueryParams $ Params Nothing [Query ("includeCleared", TE.encodeUtf8 $ (T.pack . show) (0 :: Int))]

-----------------------------------------------------------------------
-- Product Endpoint -- https://www.shipwire.com/w/developers/product --
-----------------------------------------------------------------------

-- | GET /api/v3/products
data GetProductsRequest
type instance ShipwireReturn GetProductsRequest = GetProductsResponse

instance ShipwireHasParam GetProductsRequest ExpandProductsParam
instance ShipwireHasParam GetProductsRequest DescriptionParam
instance ShipwireHasParam GetProductsRequest SKU
instance ShipwireHasParam GetProductsRequest StorageConfiguration
instance ShipwireHasParam GetProductsRequest ClassificationParam
instance ShipwireHasParam GetProductsRequest IncludeArchived
instance ShipwireHasParam GetProductsRequest ProductStatusParams
instance ShipwireHasParam GetProductsRequest FlowParams
instance ShipwireHasParam GetProductsRequest IdsParam
instance ShipwireHasParam GetProductsRequest SkusParam

-- | POST /api/v3/products
data CreateProductsRequest
type instance ShipwireReturn CreateProductsRequest = CreateProductsResponse

type CreateProductsResponse = GetProductsResponse

-- | PUT /api/v3/products
data ModifyProductsRequest
type instance ShipwireReturn ModifyProductsRequest = GenericResponse GetProductsResponseResource

-- | PUT /api/v3/products/{id}
data ModifyProductRequest
type instance ShipwireReturn ModifyProductRequest = GenericResponse GetProductsResponseResource

-- | GET /api/v3/products/{id}
data GetProductRequest
type instance ShipwireReturn GetProductRequest = GenericResponse GetProductResponseResource

-- | POST /api/v3/products/retire
data RetireProductsRequest
type instance ShipwireReturn RetireProductsRequest = RetireProductsResponse

newtype GetProductResponseResource = GetProductResponseResource
  { gprrProductWrapper :: ProductsWrapper
  } deriving (Eq, Show, FromJSON)

data ModifyProductsResponse = ModifyProductsResponse
  { mprStatus           :: ResponseStatus
  , mprResourceLocation :: ResponseResourceLocation
  , mprMessage          :: ResponseMessage
  , mprResource         :: GetProductsResponseResource
  , mprWarnings         :: Maybe ResponseWarnings
  , mprErrors           :: Maybe ProductError
  } deriving (Eq, Show)

instance FromJSON ModifyProductsResponse where
  parseJSON = withObject "ModifyProductsResponse" parse
    where
      parse o = ModifyProductsResponse
                <$> o .:  "status"
                <*> o .:  "resourceLocation"
                <*> o .:  "message"
                <*> o .:  "resource"
                <*> o .:? "warnings"
                <*> o .:? "errors"

newtype ProductsToRetire = ProductsToRetire
  { rpIds :: [Id]
  } deriving (Eq, Show)

instance ToJSON ProductsToRetire where
  toJSON ProductsToRetire {..} = object ["ids" .= rpIds]

data RetireProductsResponse = RetireProductsResponse
  { rprMessage          :: Message
  , rprMoreInfo         :: Maybe MoreInfo
  , rprResourceLocation :: Maybe ResponseResourceLocation
  , rprStatus           :: ResponseStatus
  } deriving (Eq, Show)

instance FromJSON RetireProductsResponse where
  parseJSON = withObject "RetireProductsResponse" parse
    where
      parse o = RetireProductsResponse
                <$> o .:  "message"
                <*> o .:? "moreInfo"
                <*> o .:? "resourceLocation"
                <*> o .:  "status"

newtype Message = Message
  { unMessage :: Text
  } deriving (Eq, Show, FromJSON)

newtype MoreInfo = MoreInfo
  { miItems :: [MoreInfoItems]
  } deriving (Eq, Show, FromJSON)

newtype MoreInfoItems = MoreInfoItems
  { miiItems :: [MoreInfoItem]
  } deriving (Eq, Show, FromJSON)

data MoreInfoItem = MoreInfoItem
  { miiId            :: Id
  , miiExternalId    :: Maybe ExternalId
  , miiSku           :: SKU
  , miiStatus        :: Status
  , miiConfiguration :: Configuration
  , miiSuccess       :: Success
  } deriving (Eq, Show)

instance FromJSON MoreInfoItem where
  parseJSON = withObject "MoreInfoItem" parse
    where
      parse o = MoreInfoItem
                <$> o .: "id"
                <*> o .: "externalId"
                <*> o .: "sku"
                <*> o .: "status"
                <*> o .: "configuration"
                <*> o .: "success"

newtype Configuration = Configuration
  { unConfiguration :: Text
  } deriving (Eq, Show, FromJSON)

newtype Success = Success
  { unSuccess :: Bool
  } deriving (Eq, Show, FromJSON)

-- | You can create multiple products of different classifications at the same time
-- by passing them inside a JSON array. To distinguish between different ToJSON instances
-- we use this wrapper datatype.
data CreateProductsWrapper = CpwBaseProduct BaseProduct
  | CpwMarketingInsert MarketingInsert
  | CpwVirtualKit VirtualKit
  | CpwKit Kit
  deriving (Eq, Show)

instance ToJSON CreateProductsWrapper where
  toJSON (CpwBaseProduct x)     = toJSON x
  toJSON (CpwMarketingInsert x) = toJSON x
  toJSON (CpwVirtualKit x)      = toJSON x
  toJSON (CpwKit x)             = toJSON x

data VirtualKit = VirtualKit
  -- Have to set the Id when modifying a previously created VirtualKit
  { vkId                :: Maybe Id
  , vkSku               :: SKU
  , vkClassification    :: Classification
  , vkDescription       :: Description
  , vkVirtualKitContent :: VirtualKitContent
  } deriving (Eq, Show)

instance ToJSON VirtualKit where
  toJSON VirtualKit {..} = object ["id"                .= vkId
                                  ,"sku"               .= vkSku
                                  ,"classification"    .= vkClassification
                                  ,"description"       .= vkDescription
                                  ,"virtualKitContent" .= vkVirtualKitContent]

newtype VirtualKitContent = VirtualKitContent
  { unVirtualKitContent :: [VirtualKitContentObject]
  } deriving (Eq, Show, ToJSON)

data VirtualKitContentObject = VirtualKitContentObject
  { vkcoProductId  :: Maybe ProductId
  , vkcoExternalId :: Maybe ExternalId
  , vkcoQuantity   :: Quantity
  } deriving (Eq, Show)

instance ToJSON VirtualKitContentObject where
  toJSON VirtualKitContentObject {..} = omitNulls ["productId"  .= vkcoProductId
                                                  ,"externalId" .= vkcoExternalId
                                                  ,"quantity"   .= vkcoQuantity]

data Kit = Kit
  -- Have to set the Id when modifying a previously created Kit
  { kId                   :: Maybe Id
  , kSku                  :: SKU
  , kExternalId           :: Maybe ExternalId
  , kClassification       :: Classification
  , kDescription          :: Description
  , kBatteryConfiguration :: BatteryConfiguration
  , kHsCode               :: Maybe HsCode
  , kCountryOfOrigin      :: Maybe CountryOfOrigin
  , kValues               :: KitValues
  , kAlternateNames       :: Maybe KitAlternateNames
  , kContent              :: KitContent
  , kDimensions           :: KitDimensions
  , kTechnicalData        :: Maybe KitTechnicalData
  , kFlags                :: Maybe KitFlags
  , kInnerPack            :: Maybe KitInnerPack
  , kMasterCase           :: Maybe KitMasterCase
  , kPallet               :: Maybe KitPallet
  } deriving (Eq, Show)

instance ToJSON Kit where
  toJSON Kit {..} = omitNulls ["id"                   .= kId
                              ,"sku"                  .= kSku
                              ,"externalId"           .= kExternalId
                              ,"classification"       .= kClassification
                              ,"description"          .= kDescription
                              ,"batteryConfiguration" .= kBatteryConfiguration
                              ,"hsCode"               .= kHsCode
                              ,"countryOfOrigin"      .= kCountryOfOrigin
                              ,"values"               .= kValues
                              ,"alternateNames"       .= kAlternateNames
                              ,"kitContent"           .= kContent
                              ,"dimensions"           .= kDimensions
                              ,"technicalData"        .= kTechnicalData
                              ,"flags"                .= kFlags
                              ,"innerPack"            .= kInnerPack
                              ,"masterCase"           .= kMasterCase
                              ,"pallet"               .= kPallet]

data KitPallet = KitPallet
  { kpIndividualItemsPerCase :: IndividualItemsPerCase
  , kpSku                    :: SKU
  , kpDescription            :: Description
  , kpValues                 :: KitValues
  , kpDimensions             :: KitDimensions
  , kpFlags                  :: KitPalletFlags
  } deriving (Eq, Show)

instance ToJSON KitPallet where
  toJSON KitPallet {..} = object ["individualItemsPerCase" .= kpIndividualItemsPerCase
                                 ,"sku"                    .= kpSku
                                 ,"description"            .= kpDescription
                                 ,"values"                 .= kpValues
                                 ,"dimensions"             .= kpDimensions
                                 ,"flags"                  .= kpFlags]

newtype KitPalletFlags = KitPalletFlags
  { kpfIsPackagedReadyToShip :: IsPackagedReadyToShip
  } deriving (Eq, Show, ToJSON)

data KitMasterCase = KitMasterCase
  { kmcIndividualItemsPerCase :: IndividualItemsPerCase
  , kmcSku                    :: SKU
  , kmcDescription            :: Description
  , kmcValues                 :: KitValues
  , kmcDimensions             :: KitDimensions
  , kmcFlags                  :: KitMasterCaseFlags
  } deriving (Eq, Show)

instance ToJSON KitMasterCase where
  toJSON KitMasterCase {..} = object ["individualItemsPerCase" .= kmcIndividualItemsPerCase
                                     ,"sku"                    .= kmcSku
                                     ,"description"            .= kmcDescription
                                     ,"values"                 .= kmcValues
                                     ,"dimensions"             .= kmcDimensions
                                     ,"flags"                  .= kmcFlags]

newtype KitMasterCaseFlags = KitMasterCaseFlags
  { kmcfIsPackagedReadyToShip :: IsPackagedReadyToShip
  } deriving (Eq, Show, ToJSON)

data KitInnerPack = KitInnerPack
  { kipIndividualItemsPerCase :: IndividualItemsPerCase
  , kipSku                    :: SKU
  , kipDescription            :: Description
  , kipValues                 :: KitValues
  , kipDimensions             :: KitDimensions
  , kipFlags                  :: KitInnerPackFlags
  } deriving (Eq, Show)

instance ToJSON KitInnerPack where
  toJSON KitInnerPack {..} = object ["individualItemsPerCase" .= kipIndividualItemsPerCase
                                    ,"sku"                    .= kipSku
                                    ,"description"            .= kipDescription
                                    ,"values"                 .= kipValues
                                    ,"dimensions"             .= kipDimensions
                                    ,"flags"                  .= kipFlags]

newtype KitInnerPackFlags = KitInnerPackFlags
  { kipfIsPackagedReadyToShip :: IsPackagedReadyToShip
  } deriving (Eq, Show, ToJSON)

data KitFlags = KitFlags
  { kfIsPackagedReadyToShip :: IsPackagedReadyToShip
  , kfIsFragile             :: IsFragile
  , kfIsDangerous           :: IsDangerous
  , kfIsPerishable          :: IsPerishable
  , kfIsMedia               :: IsMedia
  , kfIsAdult               :: IsAdult
  , kfIsLiquid              :: IsLiquid
  , kfHasBattery            :: HasBattery
  , kfHasInnerPack          :: HasInnerPack
  , kfHasMasterCase         :: HasMasterCase
  , kfHasPallet             :: HasPallet
  } deriving (Eq, Show)

instance ToJSON KitFlags where
  toJSON KitFlags {..} = object ["isPackagedReadyToShip" .= kfIsPackagedReadyToShip
                                ,"isFragile"             .= kfIsFragile
                                ,"isDangerous"           .= kfIsDangerous
                                ,"isPerishable"          .= kfIsPerishable
                                ,"isMedia"               .= kfIsMedia
                                ,"isAdult"               .= kfIsAdult
                                ,"isLiquid"              .= kfIsLiquid
                                ,"hasBattery"            .= kfHasBattery
                                ,"hasInnerPack"          .= kfHasInnerPack
                                ,"hasMasterCase"         .= kfHasMasterCase
                                ,"hasPallet"             .= kfHasPallet]

data HasBattery = HasBattery
  | NoBattery
  deriving (Eq, Show)

instance ToJSON HasBattery where
  toJSON HasBattery = Number 1
  toJSON NoBattery  = Number 0

newtype KitTechnicalData = KitTechnicalData
  { ktdTechnicalDataBattery :: KitTechnicalDataBattery
  } deriving (Eq, Show, ToJSON)

data KitTechnicalDataBattery = KitTechnicalDataBattery
  { ktdbType              :: BatteryType
  , ktdbBatteryWeight     :: Maybe BatteryWeight
  , ktdbNumberOfBatteries :: Maybe NumberOfBatteries
  , ktdbCapacity          :: Maybe Capacity
  , ktdbNumberOfCells     :: Maybe NumberOfCells
  , ktdbCapacityUnit      :: Maybe CapacityUnit
  } deriving (Eq, Show)

instance ToJSON KitTechnicalDataBattery where
  toJSON KitTechnicalDataBattery {..} = omitNulls ["type"              .= ktdbType
                                                  ,"batteryWeight"     .= ktdbBatteryWeight
                                                  ,"numberOfBatteries" .= ktdbNumberOfBatteries
                                                  ,"capacity"          .= ktdbCapacity
                                                  ,"numberOfCells"     .= ktdbNumberOfCells
                                                  ,"capacityUnit"      .= ktdbCapacityUnit]

data KitDimensions = KitDimensions
  { kdLength :: KitLength
  , kdWidth  :: KitWidth
  , kdHeight :: KitHeight
  , kdWeight :: KitWeight
  } deriving (Eq, Show)

instance ToJSON KitDimensions where
  toJSON KitDimensions {..} = object ["length" .= kdLength
                                     ,"width"  .= kdWidth
                                     ,"height" .= kdHeight
                                     ,"weight" .= kdWeight]

newtype KitLength = KitLength
  { unKitLength :: Double
  } deriving (Eq, Show, ToJSON)

newtype KitWidth = KitWidth
  { unKitWidth :: Double
  } deriving (Eq, Show, ToJSON)

newtype KitHeight = KitHeight
  { unKitHeight :: Double
  } deriving (Eq, Show, ToJSON)

newtype KitWeight = KitWeight
  { unKitWeight :: Double
  } deriving (Eq, Show, ToJSON)

newtype KitContent = KitContent
  { unKitContent :: [KitContentObject]
  } deriving (Eq, Show, ToJSON)

data KitContentObject = KitContentObject
  { kcProductId  :: Maybe ProductId
  , kcExternalId :: Maybe ExternalId
  , kcQuantity   :: Quantity
  } deriving (Eq, Show)

instance ToJSON KitContentObject where
  toJSON KitContentObject {..} = omitNulls ["productId"  .= kcProductId
                                           ,"externalId" .= kcExternalId
                                           ,"quantity"   .= kcQuantity]

newtype KitAlternateNames = KitAlternateNames
  { kanAlternateNames :: [KitAlternateName]
  } deriving (Eq, Show, ToJSON)

newtype KitAlternateName = KitAlternateName
  { kanAlternateName :: Name
  } deriving (Eq, Show)

instance ToJSON KitAlternateName where
  toJSON KitAlternateName {..} = object ["name" .= kanAlternateName]

data KitValues = KitValues
  { kvCostValue         :: CostValue
  , kvWholesaleValue    :: WholesaleValue
  , kvRetailValue       :: RetailValue
  , kvCostCurrency      :: Maybe CostCurrency
  , kvWholesaleCurrency :: Maybe WholesaleCurrency
  , kvRetailCurrency    :: Maybe RetailCurrency
  } deriving (Eq, Show)

instance ToJSON KitValues where
  toJSON KitValues {..} = omitNulls ["costValue"         .= kvCostValue
                                    ,"wholesaleValue"    .= kvWholesaleValue
                                    ,"retailValue"       .= kvRetailValue
                                    ,"costCurrency"      .= kvCostCurrency
                                    ,"wholesaleCurrency" .= kvWholesaleCurrency
                                    ,"retailCurrency"    .= kvRetailCurrency]

data MarketingInsert = MarketingInsert
  -- Have to set the Id when modifying a previously created MarketingInsert
  { miId                :: Maybe Id
  , miSku               :: SKU
  , miExternalId        :: Maybe ExternalId
  , miClassification    :: Classification
  , miDescription       :: Description
  , miInclusionRuleType :: InclusionRuleType
  , miAlternateNames    :: Maybe MarketingInsertAlternateNames
  , miDimensions        :: MarketingInsertDimensions
  , miFlags             :: Maybe MarketingInsertFlags
  , miInclusionRules    :: Maybe MarketingInsertInclusionRules
  , miMasterCase        :: MarketingInsertMasterCase
  } deriving (Eq, Show)

-- | There is a separate error class for /products endpoint apparently.
-- It returns back JSON with field names as object names and
-- details hidden inside that object. It is not specified anywhere what those might be.
-- So for now I treat them as an Object.
-- There must be a better way of dealing with it.
-- E.g.:
--   "errors": {
--    "myMarketingInsert": {
--      "code": "productSubmitFailed",
--      "externalId": null,
--      "id": null,
--      "sku": "myMarketingInsert",
--      "message": {
--        "sku": {
--          "stringLengthTooLong": "SKU must be between 1 and 16 characters"
--        },
--        "masterCase_sku": {
--          "stringLengthTooLong": "Master case SKU must be between 1 and 16 characters"
--        },
--      },
--      "type": {
--        "sku": {
--          "stringLengthTooLong": "error"
--        },
--        "masterCase_sku": {
--          "stringLengthTooLong": "error"
--        },
--      }
--    }
--  }
newtype ProductError = ProductError
  { unProductError :: Object
  } deriving (Eq, Show, FromJSON)

instance ToJSON MarketingInsert where
  toJSON MarketingInsert {..} = omitNulls ["id"                .= miId
                                          ,"sku"               .= miSku
                                          ,"classification"    .= miClassification
                                          ,"externalId"        .= miExternalId
                                          ,"description"       .= miDescription
                                          ,"inclusionRuleType" .= miInclusionRuleType
                                          ,"alternateNames"    .= miAlternateNames
                                          ,"dimensions"        .= miDimensions
                                          ,"flags"             .= miFlags
                                          ,"inclusionRules"    .= miInclusionRules
                                          ,"masterCase"        .= miMasterCase]

newtype MarketingInsertAlternateNames = MarketingInsertAlternateNames
  { unMarketingInsertAlternateNames :: [MarketingInsertAlternateName]
  } deriving (Eq, Show, ToJSON)

newtype MarketingInsertAlternateName = MarketingInsertAlternateName
  { mianName :: Name
  } deriving (Eq, Show)

instance ToJSON MarketingInsertAlternateName where
  toJSON MarketingInsertAlternateName {..} = object ["name" .= mianName]

data MarketingInsertDimensions = MarketingInsertDimensions
  { midLength :: MarketingInsertLength
  , midWidth  :: MarketingInsertWidth
  , midHeight :: MarketingInsertHeight
  , midWeight :: MarketingInsertWeight
  } deriving (Eq, Show)

instance ToJSON MarketingInsertDimensions where
  toJSON MarketingInsertDimensions {..} = object ["length" .= midLength
                                                 ,"width"  .= midWidth
                                                 ,"height" .= midHeight
                                                 ,"weight" .= midWeight]

newtype MarketingInsertLength = MarketingInsertLength
  { unMarketingInsertLength :: Double
  } deriving (Eq, Show, ToJSON)

newtype MarketingInsertWidth = MarketingInsertWidth
  { unMarketingInsertWidth :: Double
  } deriving (Eq, Show, ToJSON)

newtype MarketingInsertHeight = MarketingInsertHeight
  { unMarketingInsertHeight :: Double
  } deriving (Eq, Show, ToJSON)

newtype MarketingInsertWeight = MarketingInsertWeight
  { unMarketingInsertWeight :: Double
  } deriving (Eq, Show, ToJSON)

newtype MarketingInsertFlags = MarketingInsertFlags
  { mifShouldNotFold :: ShouldNotFold
  } deriving (Eq, Show, ToJSON)

data ShouldNotFold = ShouldNotFold
  | ShouldFold
  deriving (Eq, Show)

data MarketingInsertInclusionRules = MarketingInsertInclusionRules
  { miirInsertAfterDate         :: Maybe InsertAfterDate
  , miirInsertBeforeDate        :: Maybe InsertBeforeDate
  , miirInsertWhenWorthValue    :: Maybe InsertWhenWorthValue
  , miirInsertWhenQuantity      :: Maybe InsertWhenQuantity
  , miirInsertWhenWorthCurrency :: Maybe InsertWhenWorthCurrency
  } deriving (Eq, Show)

instance ToJSON MarketingInsertInclusionRules where
  toJSON MarketingInsertInclusionRules {..} = omitNulls ["insertAfterDate"         .= miirInsertAfterDate
                                                        ,"insertBeforeDate"        .= miirInsertBeforeDate
                                                        ,"insertWhenWorthValue"    .= miirInsertWhenWorthValue
                                                        ,"insertWhenQuantity"      .= miirInsertWhenQuantity
                                                        ,"insertWhenWorthCurrency" .= miirInsertWhenWorthCurrency]

instance ToJSON ShouldNotFold where
  toJSON ShouldNotFold = Number 1
  toJSON ShouldFold    = Number 0

newtype InsertWhenWorthValue = InsertWhenWorthValue
  { unInsertWhenWorthValue :: Centi
  } deriving (Eq, Show, ToJSON)

newtype InsertWhenWorthCurrency = InsertWhenWorthCurrency
  { unInsertWhenWorthCurrency :: Text
  } deriving (Eq, Show, ToJSON)

data MarketingInsertMasterCase = MarketingInsertMasterCase
  { mimcIndividualItemsPerCase :: IndividualItemsPerCase
  , mimcSku                    :: SKU
  , mimcExternalId             :: Maybe ExternalId
  , mimcDescription            :: Description
  , mimcDimensions             :: MarketingInsertMasterCaseDimensions
  } deriving (Eq, Show)

instance ToJSON MarketingInsertMasterCase where
  toJSON MarketingInsertMasterCase {..} = omitNulls ["individualItemsPerCase" .= mimcIndividualItemsPerCase
                                                    ,"sku"                    .= mimcSku
                                                    ,"externalId"             .= mimcExternalId
                                                    ,"description"            .= mimcDescription
                                                    ,"dimensions"             .= mimcDimensions]

data MarketingInsertMasterCaseDimensions = MarketingInsertMasterCaseDimensions
  { mimcdLength :: MarketingInsertMasterCaseDimensionsLength
  , mimcdWidth  :: MarketingInsertMasterCaseDimensionsWidth
  , mimcdHeight :: MarketingInsertMasterCaseDimensionsHeight
  , mimcdWeight :: MarketingInsertMasterCaseDimensionsWeight
  } deriving (Eq, Show)

instance ToJSON MarketingInsertMasterCaseDimensions where
  toJSON MarketingInsertMasterCaseDimensions {..} = object ["length" .= mimcdLength
                                                           ,"width"  .= mimcdWidth
                                                           ,"height" .= mimcdHeight
                                                           ,"weight" .= mimcdWeight]

newtype MarketingInsertMasterCaseDimensionsLength = MarketingInsertMasterCaseDimensionsLength
  { unMarketingInsertMasterCaseDimensionsLength :: Double
  } deriving (Eq, Show, ToJSON)

newtype MarketingInsertMasterCaseDimensionsWidth = MarketingInsertMasterCaseDimensionsWidth
  { unMarketingInsertMasterCaseDimensionsWidth :: Double
  } deriving (Eq, Show, ToJSON)

newtype MarketingInsertMasterCaseDimensionsHeight = MarketingInsertMasterCaseDimensionsHeight
  { unMarketingInsertMasterCaseDimensionsHeight :: Double
  } deriving (Eq, Show, ToJSON)

newtype MarketingInsertMasterCaseDimensionsWeight = MarketingInsertMasterCaseDimensionsWeight
  { unMarketingInsertMasterCaseDimensionsWeight :: Double
  } deriving (Eq, Show, ToJSON)

data BaseProduct = BaseProduct
  -- Have to set the Id when modifying a previously created BaseProduct
  { bpId                   :: Maybe Id
  , bpSku                  :: SKU
  , bpExternalId           :: Maybe ExternalId
  , bpClassification       :: Classification
  , bpDescription          :: Description
  , bpHsCode               :: Maybe HsCode
  , bpCountryOfOrigin      :: Maybe CountryOfOrigin
  , bpCategory             :: Category
  , bpBatteryConfiguration :: BatteryConfiguration
  , bpValues               :: Values
  , bpAlternateNames       :: Maybe BaseProductAlternateNames
  , bpDimensions           :: BaseProductDimensions
  , bpTechnicalData        :: Maybe BaseProductTechnicalData
  , bpFlags                :: Maybe BaseProductFlags
  , bpInnerPack            :: Maybe BaseProductInnerPack
  , bpMasterCase           :: Maybe BaseProductMasterCase
  , bpPallet               :: Maybe BaseProductPallet
  } deriving (Eq, Show)

instance ToJSON BaseProduct where
  toJSON BaseProduct {..} = omitNulls ["id"                   .= bpId
                                      ,"sku"                  .= bpSku
                                      ,"externalId"           .= bpExternalId
                                      ,"classification"       .= bpClassification
                                      ,"description"          .= bpDescription
                                      ,"hsCode"               .= bpHsCode
                                      ,"countryOfOrigin"      .= bpCountryOfOrigin
                                      ,"category"             .= bpCategory
                                      ,"batteryConfiguration" .= bpBatteryConfiguration
                                      ,"values"               .= bpValues
                                      ,"alternateNames"       .= bpAlternateNames
                                      ,"dimensions"           .= bpDimensions
                                      ,"technicalData"        .= bpTechnicalData
                                      ,"flags"                .= bpFlags
                                      ,"innerPack"            .= bpInnerPack
                                      ,"masterCase"           .= bpMasterCase
                                      ,"pallet"               .= bpPallet]

data BaseProductPallet = BaseProductPallet
  { bppIndividualItemsPerCase :: IndividualItemsPerCase
  , bppExternalid             :: Maybe ExternalId
  , bppSku                    :: SKU
  , bppDescription            :: Description
  , bppValues                 :: Values
  , bppDimensions             :: BaseProductDimensions
  , bppFlags                  :: BaseProductPalletFlags
  } deriving (Eq, Show)

instance ToJSON BaseProductPallet where
  toJSON BaseProductPallet {..} = omitNulls ["individualItemsPerCase" .= bppIndividualItemsPerCase
                                            ,"externalId"             .= bppExternalid
                                            ,"sku"                    .= bppSku
                                            ,"description"            .= bppDescription
                                            ,"values"                 .= bppValues
                                            ,"dimensions"             .= bppDimensions
                                            ,"flags"                  .= bppFlags]

newtype BaseProductPalletFlags = BaseProductPalletFlags
  { bppfIsPackagedReadyToShip :: IsPackagedReadyToShip
  } deriving (Eq, Show, ToJSON)

data BaseProductMasterCase = BaseProductMasterCase
  { bpmcIndividualItemsPerCase :: IndividualItemsPerCase
  , bpmcExternalId             :: Maybe ExternalId
  , bpmcSku                    :: SKU
  , bpmcDescription            :: Description
  , bpmcValues                 :: Values
  , bpmcDimensions             :: BaseProductDimensions
  , bpmcFlags                  :: BaseProductMasterCaseFlags
  } deriving (Eq, Show)

instance ToJSON BaseProductMasterCase where
  toJSON BaseProductMasterCase {..} = object ["individualItemsPerCase" .= bpmcIndividualItemsPerCase
                                             ,"externalId"             .= bpmcExternalId
                                             ,"sku"                    .= bpmcSku
                                             ,"description"            .= bpmcDescription
                                             ,"values"                 .= bpmcValues
                                             ,"dimensions"             .= bpmcDimensions
                                             ,"flags"                  .= bpmcFlags]

newtype BaseProductMasterCaseFlags = BaseProductMasterCaseFlags
  { bpmcfIsPackagedReadyToShip :: IsPackagedReadyToShip
  } deriving (Eq, Show, ToJSON)

data BaseProductInnerPack = BaseProductInnerPack
  { bpipIndividualItemsPerCase :: IndividualItemsPerCase
  , bpipExternalId             :: Maybe ExternalId
  , bpipSku                    :: SKU
  , bpipDescription            :: Description
  , bpipValues                 :: Values
  , bpipDimensions             :: BaseProductDimensions
  , bpipFlags                  :: BaseProductInnerPackFlags
  } deriving (Eq, Show)

instance ToJSON BaseProductInnerPack where
  toJSON BaseProductInnerPack {..} = omitNulls ["individualItemsPerCase" .= bpipIndividualItemsPerCase
                                               ,"externalId"             .= bpipExternalId
                                               ,"sku"                    .= bpipSku
                                               ,"description"            .= bpipDescription
                                               ,"values"                 .= bpipValues
                                               ,"dimensions"             .= bpipDimensions
                                               ,"flags"                  .= bpipFlags]

newtype BaseProductInnerPackFlags = BaseProductInnerPackFlags
  { bpipfIsPackagedReadyToShip :: IsPackagedReadyToShip
  } deriving (Eq, Show, ToJSON)

newtype BaseProductAlternateNames = BaseProductAlternateNames
  { bpanAlternateNames :: [BaseProductAlternateName]
  } deriving (Eq, Show, ToJSON)

newtype BaseProductAlternateName = BaseProductAlternateName
  { bpanName :: Name
  } deriving (Eq, Show)

instance ToJSON BaseProductAlternateName where
  toJSON BaseProductAlternateName {..} = object ["name" .= bpanName]

data BaseProductFlags = BaseProductFlags
  { bpfIsPackagedReadyToShip :: IsPackagedReadyToShip
  , bpfIsFragile             :: IsFragile
  , bpfIsDangerous           :: IsDangerous
  , bpfIsPerishable          :: IsPerishable
  , bpfIsMedia               :: IsMedia
  , bpfIsAdult               :: IsAdult
  , bpfIsLiquid              :: IsLiquid
  , bpfHasInnerPack          :: HasInnerPack
  , bpfHasMasterCase         :: HasMasterCase
  , bpfHasPallet             :: HasPallet
  } deriving (Eq, Show)

instance ToJSON BaseProductFlags where
  toJSON BaseProductFlags {..} = object ["isPackagedReadyToShip" .= bpfIsPackagedReadyToShip
                                        ,"isFragile"             .= bpfIsFragile
                                        ,"isDangerous"           .= bpfIsDangerous
                                        ,"isPerishable"          .= bpfIsPerishable
                                        ,"isMeida"               .= bpfIsMedia
                                        ,"isAdult"               .= bpfIsAdult
                                        ,"isLiquid"              .= bpfIsLiquid
                                        ,"hasInnerPack"          .= bpfHasInnerPack
                                        ,"hasMasterCase"         .= bpfHasMasterCase
                                        ,"hasPallet"             .= bpfHasPallet]

newtype BaseProductTechnicalData = BaseProductTechnicalData
  { bptdTechnicalDataBattery :: BaseProductTechnicalDataBattery
  } deriving (Eq, Show, ToJSON)

data BaseProductTechnicalDataBattery = BaseProductTechnicalDataBattery
  { bptdbType              :: Maybe BatteryType
  , bptdbBatteryWeight     :: Maybe BatteryWeight
  , bptdbNumberOfBatteries :: Maybe NumberOfBatteries
  , bptdbCapacity          :: Maybe Capacity
  , bptdbNumberOfCells     :: Maybe NumberOfCells
  , bptdbCapacityUnit      :: Maybe CapacityUnit
  } deriving (Eq, Show)

instance ToJSON BaseProductTechnicalDataBattery where
  toJSON BaseProductTechnicalDataBattery {..} = omitNulls ["type"              .= bptdbType
                                                          ,"batteryWeight"     .= bptdbBatteryWeight
                                                          ,"numberOfBatteries" .= bptdbNumberOfBatteries
                                                          ,"capacity"          .= bptdbCapacity
                                                          ,"numberOfCells"     .= bptdbNumberOfCells
                                                          ,"capacityUnit"      .= bptdbCapacityUnit]

data BaseProductDimensions = BaseProductDimensions
  { bpdLength :: BaseProductLength
  , bpdWidth  :: BaseProductWidth
  , bpdHeight :: BaseProductHeight
  , bpdWeight :: BaseProductWeight
  } deriving (Eq, Show)

instance ToJSON BaseProductDimensions where
  toJSON BaseProductDimensions {..} = object ["length" .= bpdLength
                                             ,"width"  .= bpdWidth
                                             ,"height" .= bpdHeight
                                             ,"weight" .= bpdWeight]

newtype BaseProductLength = BaseProductLength
  { unBaseProductLength :: Double
  } deriving (Eq, Show, ToJSON)

newtype BaseProductWidth = BaseProductWidth
  { unBaseProductWidth :: Double
  } deriving (Eq, Show, ToJSON)

newtype BaseProductHeight = BaseProductHeight
  { unBaseProductHeight :: Double
  } deriving (Eq, Show, ToJSON)

newtype BaseProductWeight = BaseProductWeight
  { unBaseProductWeight :: Double
  } deriving (Eq, Show, ToJSON)

newtype SkusParam = SkusParam
  { unSkusParam :: [BS8.ByteString]
  } deriving (Eq, Show)

instance ToShipwireParam SkusParam where
  toShipwireParam (SkusParam xs) =
    joinQueryParams $ Params Nothing [Query ("skus", BS8.intercalate "," xs)]

newtype IdsParam = IdsParam
  { unIdsParam :: [BS8.ByteString]
  } deriving (Eq, Show)

instance ToShipwireParam IdsParam where
  toShipwireParam (IdsParam xs) =
    joinQueryParams $ Params Nothing [Query ("ids", BS8.intercalate "," xs)]

newtype FlowParams = FlowParams
  { unFlowParams :: [FlowParam]
  } deriving (Eq, Show)

data FlowParam = ManageStep
  | KitStep
  | VirtualKitStep
  | OrderStep
  | ReceivingStep
  | QuoteStep
  deriving (Eq, Show)

flowParamToBS :: FlowParam -> BS8.ByteString
flowParamToBS ManageStep     = "manage"
flowParamToBS KitStep        = "kit"
flowParamToBS VirtualKitStep = "virtualKit"
flowParamToBS OrderStep      = "order"
flowParamToBS ReceivingStep  = "receiving"
flowParamToBS QuoteStep      = "quote"

instance ToShipwireParam FlowParams where
  toShipwireParam (FlowParams xs) =
    joinQueryParams $ Params Nothing [Query ("flow", BS8.intercalate "," (map flowParamToBS xs))]

newtype ProductStatusParams = ProductStatusParams
  { unProductStatusParams :: [ProductStatusParam]
  } deriving (Eq, Show)

data ProductStatusParam = NotInUse
  | InStock
  | OutOfStock
  deriving (Eq, Show)

productStatusParamToBS :: ProductStatusParam -> BS8.ByteString
productStatusParamToBS NotInUse   = "notinuse"
productStatusParamToBS InStock    = "instock"
productStatusParamToBS OutOfStock = "outofstock"

instance ToShipwireParam ProductStatusParams where
  toShipwireParam (ProductStatusParams xs) =
    joinQueryParams $ Params Nothing [Query ("status", BS8.intercalate "," (map productStatusParamToBS xs))]

data IncludeArchived = AnyArchived
  | IncludeArchived
  | ExcludeArchived
  deriving (Eq, Show)

includeArchivedToBS :: IncludeArchived -> BS8.ByteString
includeArchivedToBS AnyArchived     = "anyArchived"
includeArchivedToBS IncludeArchived = "includeArchived"
includeArchivedToBS ExcludeArchived = "excludeArchived"

instance ToShipwireParam IncludeArchived where
  toShipwireParam x =
    joinQueryParams $ Params Nothing [Query ("includeArchived", includeArchivedToBS x)]

storageConfigurationToBS :: StorageConfiguration -> BS8.ByteString
storageConfigurationToBS IndividualItemConfiguration = "INDIVIDUAL_ITEM"
storageConfigurationToBS InnerPackConfiguration      = "INNER_PACK"
storageConfigurationToBS MasterCaseConfiguration     = "MASTER_CASE"
storageConfigurationToBS PalletConfiguration         = "PALLET"
storageConfigurationToBS KitConfiguration            = "KIT"

instance ToShipwireParam StorageConfiguration where
  toShipwireParam x =
    joinQueryParams $ Params Nothing [Query ("storageConfiguration", storageConfigurationToBS x)]

newtype DescriptionParam = DescriptionParam
  { unDescriptionParam :: Text
  } deriving (Eq, Show)

replaceSpaces :: Text -> Text
replaceSpaces = T.intercalate "+" . T.words

instance ToShipwireParam DescriptionParam where
  toShipwireParam (DescriptionParam x) =
    joinQueryParams $ Params Nothing [Query ("description", TE.encodeUtf8 $ replaceSpaces x)]

data ClassificationParam = ClassificationParamBaseProduct
  | ClassificationParamKit
  | ClassificationParamVirtualKit
  | ClassificationParamMarketingInsert
  deriving (Eq, Show)

classificationToBS :: ClassificationParam -> BS8.ByteString
classificationToBS ClassificationParamBaseProduct     = "baseProduct"
classificationToBS ClassificationParamKit             = "kit"
classificationToBS ClassificationParamVirtualKit      = "virtualKit"
classificationToBS ClassificationParamMarketingInsert = "marketingInsert"

instance ToShipwireParam ClassificationParam where
  toShipwireParam x =
    joinQueryParams $ Params Nothing [Query ("classification", classificationToBS x)]

newtype ExpandProductsParam = ExpandProductsParam
  { expandProductsParam :: [ExpandProducts]
  } deriving (Eq, Show)

data ExpandProducts = ProductsExpandAll
  | ExpandAlternateNames
  | ExpandMasterCase
  | ExpandEnqueuedDimensions
  | ExpandFlags
  | ExpandDimensions
  | ExpandTechnicalData
  | ExpandInnerPack
  | ExpandPallet
  | ExpandValues
  | ExpandKitContent
  | ExpandInclusionRules
  | ExpandVirtualKitContent
  deriving (Eq, Show)

expandProductsToBS8 :: ExpandProducts -> BS8.ByteString
expandProductsToBS8 ProductsExpandAll        = "all"
expandProductsToBS8 ExpandAlternateNames     = "alternateNames"
expandProductsToBS8 ExpandMasterCase         = "masterCase"
expandProductsToBS8 ExpandEnqueuedDimensions = "enqueuedDimensions"
expandProductsToBS8 ExpandFlags              = "flags"
expandProductsToBS8 ExpandDimensions         = "dimensions"
expandProductsToBS8 ExpandTechnicalData      = "technicalData"
expandProductsToBS8 ExpandInnerPack          = "innerPack"
expandProductsToBS8 ExpandPallet             = "pallet"
expandProductsToBS8 ExpandValues             = "values"
expandProductsToBS8 ExpandKitContent         = "kitContent"
expandProductsToBS8 ExpandInclusionRules     = "inclusionRules"
expandProductsToBS8 ExpandVirtualKitContent  = "virtualKitContent"

instance ToShipwireParam ExpandProductsParam where
  toShipwireParam (ExpandProductsParam xs) =
    joinQueryParams $ Params Nothing [Query ("expand", BS8.intercalate "," (map expandProductsToBS8 xs))]

data GetProductsResponse = GetProductsResponse
  { gprStatus           :: ResponseStatus
  , gprResourceLocation :: ResponseResourceLocation
  , gprMessage          :: ResponseMessage
  , gprResource         :: GetProductsResponseResource
  , gprWarnings         :: Maybe ResponseWarnings
  , gprErrors           :: Maybe ProductError
  } deriving (Eq, Show)

instance FromJSON GetProductsResponse where
  parseJSON = withObject "GetProductsResponse" parse
    where
      parse o = GetProductsResponse
                <$> o .:  "status"
                <*> o .:  "resourceLocation"
                <*> o .:  "message"
                <*> o .:  "resource"
                <*> o .:? "warnings"
                <*> o .:? "errors"

data GetProductsResponseResource = GetProductsResponseResource
  { gprrPrevious :: Maybe ResponsePrevious
  , gprrNext     :: Maybe ResponseNext
  , gprrTotal    :: ResponseTotal
  , gprrOffset   :: ResponseOffset
  , gprrItems    :: GetProductsResponseResourceItems
  } deriving (Eq, Show)

instance FromJSON GetProductsResponseResource where
  parseJSON = withObject "GetProductsResponseResource" parse
    where
      parse o = GetProductsResponseResource
                <$> o .:? "previous"
                <*> o .:? "next"
                <*> o .:  "total"
                <*> o .:  "offset"
                <*> o .:  "items"

newtype GetProductsResponseResourceItems = GetProductsResponseResourceItems
  { gprriItems :: [GetProductsResponseResourceItem]
  } deriving (Eq, Show, FromJSON)

data GetProductsResponseResourceItem = GetProductsResponseResourceItem
  { gprriResourceLocation :: ResponseResourceLocation
  , gprriResource         :: ProductsWrapper
  } deriving (Eq, Show)

instance FromJSON GetProductsResponseResourceItem where
  parseJSON = withObject "GetProductsResponseResourceItem" parse
    where
      parse o = GetProductsResponseResourceItem
                <$> o .: "resourceLocation"
                <*> o .: "resource"

-- | This a wrapper for different classifications of products.
-- Possible options are: baseProduct, marketingInsert, virtualKit, kit.
data ProductsWrapper = PwBaseProduct BaseProductResponseResource
  | PwMarketingInsert MarketingInsertResponseResource
  | PwVirtualKit VirtualKitResponseResource
  | PwKit KitResponseResource
  deriving (Eq, Show)

instance FromJSON ProductsWrapper where
  parseJSON (Object v) = pwValue
    where
      isBaseProduct     = HM.lookup "classification" v == Just "baseProduct"
      isMarketingInsert = HM.lookup "classification" v == Just "marketingInsert"
      isVirtualKit      = HM.lookup "classification" v == Just "virtualKit"
      isKit             = HM.lookup "classification" v == Just "kit"
      pwValue          = parseProductsWrapper isBaseProduct isMarketingInsert isVirtualKit isKit v
  parseJSON _ = mempty

parseProductsWrapper :: Bool -> Bool -> Bool -> Bool -> Object -> Parser ProductsWrapper
parseProductsWrapper isBaseProduct isMarketingInsert isVirtualKit isKit value
  | isBaseProduct     = PwBaseProduct     <$> parseBaseProduct value
  | isMarketingInsert = PwMarketingInsert <$> parseMarketingInsert value
  | isVirtualKit      = PwVirtualKit      <$> parseVirtualKit value
  | isKit             = PwKit             <$> parseKit value
  | otherwise         = PwBaseProduct     <$> parseBaseProduct value

data KitResponseResource = KitResponseResource
  { krId                   :: Id
  , krExternalId           :: Maybe ExternalId
  , krSku                  :: SKU
  , krDescription          :: Description
  , krHsCode               :: Maybe HsCode
  , krCountryOfOrigin      :: Maybe CountryOfOrigin
  , krCreationDate         :: CreationDate
  , krArchivedDate         :: Maybe ArchivedDate
  , krStatus               :: Status
  , krStorageConfiguration :: StorageConfiguration
  , krBatteryConfiguration :: Maybe BatteryConfiguration
  , krClassification       :: Classification
  , krCategory             :: Maybe Category
  , krItemCount            :: ItemCount
  , krDimensions           :: Dimensions
  , krValues               :: ValuesResource
  , krAlternateNames       :: AlternateNamesResponse
  , krKitContent           :: Maybe KitResponseContent
  , krTechnicalData        :: Maybe KitResponseTechnicalData
  , krFlags                :: Flags
  , krEnqueuedDimensions   :: EnqueuedDimensions
  , krInnerPack            :: Maybe InnerPack
  , krMasterCase           :: Maybe KitResponseMasterCase
  , krPallet               :: Maybe Pallet
  } deriving (Eq, Show)

instance FromJSON KitResponseResource where
  parseJSON (Object o) = parseKit o
  parseJSON _          = mempty

parseKit :: Object -> Parser KitResponseResource
parseKit o = KitResponseResource
             <$> o .:  "id"
             <*> o .:? "externalId"
             <*> o .:  "sku"
             <*> o .:  "description"
             <*> o .:? "hsCode"
             <*> o .:? "countryOfOrigin"
             <*> o .:  "creationDate"
             <*> o .:? "archivedDate"
             <*> o .:  "status"
             <*> o .:  "storageConfiguration"
             <*> o .:? "batteryConfiguration"
             <*> o .:  "classification"
             <*> o .:? "category"
             <*> o .:  "itemCount"
             <*> o .:  "dimensions"
             <*> o .:  "values"
             <*> o .:  "alternateNames"
             <*> o .:? "kitContent"
             <*> o .:? "technicalData"
             <*> o .:  "flags"
             <*> o .:  "enqueuedDimensions"
             <*> o .:? "innerPack"
             <*> o .:? "masterCase"
             <*> o .:? "pallet"

type KitResponseMasterCase = BaseProductResponseMasterCase

data KitResponseTechnicalData = KitResponseTechnicalData
  { ktdResourceLocation :: Maybe ResponseResourceLocation
  , ktdResource         :: Maybe KitResponseTechnicalDataResource
  } deriving (Eq, Show)

instance FromJSON KitResponseTechnicalData where
  parseJSON = withObject "KitResponseTechnicalData" parse
    where
      parse o = KitResponseTechnicalData
                <$> o .:? "resourceLocation"
                <*> o .:? "resource"

newtype KitResponseTechnicalDataResource = KitResponseTechnicalDataResource
  { ktdrBattery :: KitResponseTechnicalDataResourceBattery
  } deriving (Eq, Show, FromJSON)

data KitResponseTechnicalDataResourceBattery = KitResponseTechnicalDataResourceBattery
  { ktdrbResourceLocation :: Maybe ResponseResourceLocation
  , ktdrbResource         :: KitTechnicalDataResourceBatteryResource
  } deriving (Eq, Show)

instance FromJSON KitResponseTechnicalDataResourceBattery where
  parseJSON = withObject "KitResponseTechnicalDataResourceBattery" parse
    where
      parse o = KitResponseTechnicalDataResourceBattery
                <$> o .:? "resourceLocation"
                <*> o .:  "resource"

type KitTechnicalDataResourceBatteryResource = TechnicalDataResource

type KitResponseContent = VirtualKitResponseContent

data VirtualKitResponseResource = VirtualKitResponseResource
  { vkrId                :: Id
  , vkrExternalId        :: Maybe ExternalId
  , vkrClassification    :: Classification
  , vkrSku               :: SKU
  , vkrCreationDate      :: CreationDate
  , vkrDescription       :: Description
  , vkrStatus            :: Status
  , vkrVirtualKitContent :: VirtualKitResponseContent
  , vkrFlags             :: VirtualKitFlags
  } deriving (Eq, Show)

instance FromJSON VirtualKitResponseResource where
  parseJSON (Object o) = parseVirtualKit o
  parseJSON _          = mempty

parseVirtualKit :: Object -> Parser VirtualKitResponseResource
parseVirtualKit o = VirtualKitResponseResource
                    <$> o .:  "id"
                    <*> o .:? "externalId"
                    <*> o .:  "classification"
                    <*> o .:  "sku"
                    <*> o .:  "creationDate"
                    <*> o .:  "description"
                    <*> o .:  "status"
                    <*> o .:  "virtualKitContent"
                    <*> o .:  "flags"

data VirtualKitResponseContent = VirtualKitResponseContent
  { vkcResourceLocation :: ResponseResourceLocation
  , vkcResource         :: Maybe VirtualKitContentResource
  } deriving (Eq, Show)

instance FromJSON VirtualKitResponseContent where
  parseJSON = withObject "VirtualKitContentResponse" parse
    where
      parse o = VirtualKitResponseContent
                <$> o .:  "resourceLocation"
                <*> o .:? "resource"

data VirtualKitContentResource = VirtualKitContentResource
  { vkcrOffset   :: ResponseOffset
  , vkcrTotal    :: ResponseTotal
  , vkcrPrevious :: Maybe ResponsePrevious
  , vkcrNext     :: Maybe ResponseNext
  , vkcrItems    :: VirtualKitContentResourceItems
  } deriving (Eq, Show)

instance FromJSON VirtualKitContentResource where
  parseJSON = withObject "VirtualKitContentResource" parse
    where
      parse o = VirtualKitContentResource
                <$> o .:  "offset"
                <*> o .:  "total"
                <*> o .:? "previous"
                <*> o .:? "next"
                <*> o .:  "items"

newtype VirtualKitContentResourceItems = VirtualKitContentResourceItems
  { vkcriItems :: [VirtualKitContentResourceItem]
  } deriving (Eq, Show, FromJSON)

data VirtualKitContentResourceItem = VirtualKitContentResourceItem
  { vkcriResourceLocation :: Maybe ResponseResourceLocation
  , vkcriResource         :: VirtualKitContentResourceItemResource
  } deriving (Eq, Show)

instance FromJSON VirtualKitContentResourceItem where
  parseJSON = withObject "VirtualKitContentResourceItem" parse
    where
      parse o = VirtualKitContentResourceItem
                <$> o .:? "resourceLocation"
                <*> o .:  "resource"

data VirtualKitContentResourceItemResource = VirtualKitContentResourceItemResource
  { vkcrirProductId  :: ProductId
  , vkcrirExternalId :: ExternalId
  , vkcrirQuantity   :: Quantity
  } deriving (Eq, Show)

instance FromJSON VirtualKitContentResourceItemResource where
  parseJSON = withObject "VirtualKitContentResourceItemResource" parse
    where
      parse o = VirtualKitContentResourceItemResource
                <$> o .: "productId"
                <*> o .: "externalId"
                <*> o .: "quantity"

newtype VirtualKitFlags = VirtualKitFlags
  { vkfResourceLocation :: Maybe ResponseResourceLocation
  } deriving (Eq, Show)

instance FromJSON VirtualKitFlags where
  parseJSON = withObject "VirtualKitFlags" parse
    where
      parse o = VirtualKitFlags
                <$> o .:? "resourceLocation"

data MarketingInsertResponseResource = MarketingInsertResponseResource
  { mirId                   :: Id
  , mirExternalId           :: Maybe ExternalId
  , mirSku                  :: SKU
  , mirDescription          :: Description
  , mirInclusionRuleType    :: Maybe InclusionRuleType
  , mirCreationDate         :: CreationDate
  , mirArchivedDate         :: Maybe ArchivedDate
  , mirStatus               :: Status
  , mirStorageConfiguration :: StorageConfiguration
  , mirClassificaion        :: Classification
  , mirItemCount            :: ItemCount
  , mirDimensions           :: Dimensions
  , mirAlternateNames       :: AlternateNamesResponse
  , mirFlags                :: Maybe MarketingInsertFlagsResponse
  , mirInclusionRules       :: Maybe InclusionRules
  , mirMasterCase           :: Maybe MarketingInsertMasterCaseResponse
  } deriving (Eq, Show)

instance FromJSON MarketingInsertResponseResource where
  parseJSON (Object o) = parseMarketingInsert o
  parseJSON _          = mempty

parseMarketingInsert :: Object -> Parser MarketingInsertResponseResource
parseMarketingInsert o = MarketingInsertResponseResource
                         <$> o .:  "id"
                         <*> o .:? "externalId"
                         <*> o .:  "sku"
                         <*> o .:  "description"
                         <*> o .:? "inclusionRuleType"
                         <*> o .:  "creationDate"
                         <*> o .:? "archivedDate"
                         <*> o .:  "status"
                         <*> o .:  "storageConfiguration"
                         <*> o .:  "classification"
                         <*> o .:  "itemCount"
                         <*> o .:  "dimensions"
                         <*> o .:  "alternateNames"
                         <*> o .:? "flags"
                         <*> o .:? "inclusionRules"
                         <*> o .:? "masterCase"

data MarketingInsertMasterCaseResponse = MarketingInsertMasterCaseResponse
  { mimcResourceLocation :: Maybe ResponseResourceLocation
  , mimcResource         :: Maybe MarketingInsertMasterCaseResponseResource
  } deriving (Eq, Show)

instance FromJSON MarketingInsertMasterCaseResponse where
  parseJSON = withObject "MarketingInsertMasterCaseResponse" parse
    where
      parse o = MarketingInsertMasterCaseResponse
                <$> o .:? "resourceLocation"
                <*> o .:? "resource"

data MarketingInsertMasterCaseResponseResource = MarketingInsertMasterCaseResponseResource
  { mimcrProductId              :: ProductId
  , mimcrExternalId             :: Maybe ExternalId
  , mimcrIndividualItemsPerCase :: IndividualItemsPerCase
  , mimcrSku                    :: SKU
  , mimcrDescription            :: Description
  , mimcrDimensions             :: Dimensions
  } deriving (Eq, Show)

instance FromJSON MarketingInsertMasterCaseResponseResource where
  parseJSON = withObject "MarketingInsertMasterCaseResponseResource" parse
    where
      parse o = MarketingInsertMasterCaseResponseResource
                <$> o .:  "productId"
                <*> o .:? "externalId"
                <*> o .:  "individualItemsPerCase"
                <*> o .:  "sku"
                <*> o .:  "description"
                <*> o .:  "dimensions"

newtype InclusionRuleType = InclusionRuleType
  { unInclusionRuleType :: Text
  } deriving (Eq, Show, ToJSON, FromJSON)

data InclusionRules = InclusionRules
  { irResourceLocation :: Maybe ResponseResourceLocation
  , irResource         :: Maybe InclusionRulesResource
  } deriving (Eq, Show)

instance FromJSON InclusionRules where
  parseJSON = withObject "InclusionRules" parse
    where
      parse o = InclusionRules
                <$> o .:? "resourceLocation"
                <*> o .:? "resource"

data InclusionRulesResource = InclusionRulesResource
  { irrProductId                    :: ProductId
  , irrInsertAfterDate              :: Maybe InsertAfterDate
  , irrInsertBeforeDate             :: Maybe InsertBeforeDate
  , irrInsertWhenWorthValue         :: InsertWhenWorthValueResponse
  , irrInsertWhenWorthValueCurrency :: InsertWhenWorthValueCurrency
  , irrInsertWhenQuantity           :: InsertWhenQuantity
  , irrFlags                        :: Maybe InclusionRulesResourceFlags
  } deriving (Eq, Show)

instance FromJSON InclusionRulesResource where
  parseJSON = withObject "InclusionRulesResource" parse
    where
      parse o = InclusionRulesResource
                <$> o .:  "productId"
                <*> o .:? "insertAfterDate"
                <*> o .:? "insertBeforeDate"
                <*> o .:  "insertWhenWorthValue"
                <*> o .:  "insertWhenWorthValueCurrency"
                <*> o .:  "insertWhenQuantity"
                <*> o .:? "flags"

utcToShipwire :: UTCTime -> Text
utcToShipwire ut =
  tshow day <> "T" <> clockTime <> "-00:00"
  where tshow :: Show a => a -> Text
        tshow = T.pack . show
        day = utctDay ut
        time = utctDayTime ut
        tod = snd $ utcToLocalTimeOfDay utc (timeToTimeOfDay time)
        atLeastTwo :: Text -> Int -> Text
        atLeastTwo t i
          | i < 10 = t <> tshow i
          | otherwise = tshow i
        clockTime = atLeastTwo "0" (todHour tod)
                 <> ":"
                 <> atLeastTwo "0" (todMin tod)
                 <> ":"
                 <> atLeastTwo "0" (floor $ todSec tod)

newtype InsertAfterDate = InsertAfterDate
  { unInsertAfterDate :: UTCTime
  } deriving (Eq, Show, FromJSON)

instance ToJSON InsertAfterDate where
  toJSON (InsertAfterDate x) = object ["insertAfterDate" .= utcToShipwire x]

newtype InsertBeforeDate = InsertBeforeDate
  { unInsertBeforeDate :: UTCTime
  } deriving (Eq, Show, FromJSON)

instance ToJSON InsertBeforeDate where
  toJSON (InsertBeforeDate x) = object ["insertBeforeDate" .= utcToShipwire x]

newtype InsertWhenWorthValueResponse = InsertWhenWorthValueResponse
  { unInsertWhenWorthValueResponse :: Text
  } deriving (Eq, Show, FromJSON)

type InsertWhenWorthValueCurrency = CostCurrency

newtype InsertWhenQuantity = InsertWhenQuantity
  { unInsertWhenQuantity :: Integer
  } deriving (Eq, Show, ToJSON, FromJSON)

newtype InclusionRulesResourceFlags = InclusionRulesResourceFlags
  { irrfResourceLocation :: Maybe ResponseResourceLocation
  } deriving (Eq, Show, FromJSON)

data MarketingInsertFlagsResponse = MarketingInsertFlagsResponse
  { mifResourceLocation :: Maybe ResponseResourceLocation
  , mifResource         :: Maybe MarketingInsertFlagsResponseResource
  } deriving (Eq, Show)

instance FromJSON MarketingInsertFlagsResponse where
  parseJSON = withObject "MarketingInsertFlagsResponse" parse
    where
      parse o = MarketingInsertFlagsResponse
                <$> o .:? "resourceLocation"
                <*> o .:? "resource"

data MarketingInsertFlagsResponseResource = MarketingInsertFlagsResponseResource
  { mifrIsPackagedReadyToShip :: IsPackagedReadyToShip
  , mifrHasMasterCase         :: Maybe HasMasterCase
  , mifrIsArchivable          :: IsArchivable
  , mifrIsDeletable           :: IsDeletable
  , mifrHasEditRestrictions   :: HasEditRestrictions
  } deriving (Eq, Show)

instance FromJSON MarketingInsertFlagsResponseResource where
  parseJSON = withObject "MarketingInsertFlagsResponseResource" parse
    where
      parse o = MarketingInsertFlagsResponseResource
                <$> o .:  "isPackagedReadyToShip"
                <*> o .:? "hasMasterCase"
                <*> o .:  "isArchivable"
                <*> o .:  "isDeletable"
                <*> o .:  "hasEditRestrictions"

data BaseProductResponseResource = BaseProductResponseResource
  { bprClassification       :: Classification
  , bprBatteryConfiguration :: Maybe BatteryConfiguration
  , bprMasterCase           :: Maybe BaseProductResponseMasterCase
  , bprItemCount            :: ItemCount
  , bprId                   :: Id
  , bprSku                  :: SKU
  , bprArchivedDate         :: Maybe ArchivedDate
  , bprEnqueuedDimensions   :: EnqueuedDimensions
  , bprDimensions           :: Dimensions
  , bprInnerPack            :: Maybe InnerPack
  , bprPallet               :: Maybe Pallet
  , bprExternalId           :: Maybe ExternalId
  , bprAlternateNames       :: AlternateNamesResponse
  , bprValues               :: ValuesResponse
  , bprStatus               :: Status
  , bprCategory             :: Maybe Category
  , bprVendorId             :: Maybe VendorId
  , bprVendorExternalid     :: Maybe VendorExternalId
  , bprDescription          :: Description
  , bprFlags                :: Flags
  , bprCreationDate         :: CreationDate
  , bprHsCode               :: Maybe HsCode
  , bprCountryOfOrigin      :: Maybe CountryOfOrigin
  , bprStorageConfiguration :: StorageConfiguration
  , bprTechnicalData        :: Maybe TechnicalData
  } deriving (Eq, Show)

type VendorId = Id

type VendorExternalId = ExternalId

instance FromJSON BaseProductResponseResource where
  parseJSON (Object o) = parseBaseProduct o
  parseJSON _          = mempty

parseBaseProduct :: Object -> Parser BaseProductResponseResource
parseBaseProduct o = BaseProductResponseResource
                     <$> o .:  "classification"
                     <*> o .:? "batteryConfiguration"
                     <*> o .:? "masterCase"
                     <*> o .:  "itemCount"
                     <*> o .:  "id"
                     <*> o .:  "sku"
                     <*> o .:? "archivedDate"
                     <*> o .:  "enqueuedDimensions"
                     <*> o .:  "dimensions"
                     <*> o .:? "innerPack"
                     <*> o .:? "pallet"
                     <*> o .:? "externalId"
                     <*> o .:  "alternateNames"
                     <*> o .:  "values"
                     <*> o .:  "status"
                     <*> o .:? "category"
                     <*> o .:? "vendorId"
                     <*> o .:? "vendorExternalId"
                     <*> o .:  "description"
                     <*> o .:  "flags"
                     <*> o .:  "creationDate"
                     <*> o .:? "hsCode"
                     <*> o .:? "countryOfOrigin"
                     <*> o .:  "storageConfiguration"
                     <*> o .:? "technicalData"

data TechnicalData = TechnicalData
  { tdResourceLocation :: Maybe ResponseResourceLocation
  , tdResource         :: Maybe TechnicalDataResource
  } deriving (Eq, Show)

instance FromJSON TechnicalData where
  parseJSON = withObject "TechnicalData" parse
    where
      parse o = TechnicalData
                <$> o .:? "resourceLocation"
                <*> o .:? "resource"

data TechnicalDataResource = TechnicalDataResource
  { tdrCapacityUnit      :: Maybe CapacityUnit
  , tdrCapacity          :: Maybe CapacityResponse
  , tdrBatteryWeight     :: Maybe BatteryWeightResponse
  , tdrNumberOfCells     :: Maybe NumberOfCells
  , tdrType              :: Maybe BatteryType
  , tdrNumberOfBatteries :: Maybe NumberOfBatteries
  , tdrProductId         :: Maybe ProductId
  } deriving (Eq, Show)

instance FromJSON TechnicalDataResource where
  parseJSON = withObject "TechnicalDataResource" parse
    where
      parse o = TechnicalDataResource
                <$> o .:? "capacityUnit"
                <*> o .:? "capacity"
                <*> o .:? "batteryWeight"
                <*> o .:? "numberOfCells"
                <*> o .:? "type"
                <*> o .:? "numberOfBatteries"
                <*> o .:? "productId"

newtype NumberOfBatteries = NumberOfBatteries
  { unNumberOfBatteries :: Integer
  } deriving (Eq, Show, ToJSON, FromJSON)

newtype BatteryType = BatteryType
  { unBatteryType :: Text
  } deriving (Eq, Show, ToJSON, FromJSON)

newtype NumberOfCells = NumberOfCells
  { unNumberOfCells :: Integer
  } deriving (Eq, Show, ToJSON, FromJSON)

newtype BatteryWeightResponse = BatteryWeightResponse
  { unBatteryWeightR :: Text
  } deriving (Eq, Show, FromJSON)

newtype BatteryWeight = BatteryWeight
  { unBatteryWeight :: Integer
  } deriving (Eq, Show, ToJSON)

newtype CapacityResponse = CapacityResponse
  { unCapacityR :: Text
  } deriving (Eq, Show, FromJSON)

newtype Capacity = Capacity
  { unCapacity :: Integer
  } deriving (Eq, Show, ToJSON)

-- It's not specified in the docs what type this should be.
newtype CapacityUnit = CapacityUnit
  { unCapacityUnit :: Text
  } deriving (Eq, Show, ToJSON, FromJSON)

data StorageConfiguration = IndividualItemConfiguration
  | InnerPackConfiguration
  | MasterCaseConfiguration
  | PalletConfiguration
  | KitConfiguration
  deriving (Eq, Show)

instance FromJSON StorageConfiguration where
  parseJSON = withText "StorageConfiguration" parse
    where
      parse "INDIVIDUAL_ITEM" = pure IndividualItemConfiguration
      parse "INNER_PACK"      = pure InnerPackConfiguration
      parse "MASTER_CASE"     = pure MasterCaseConfiguration
      parse "PALLET"          = pure PalletConfiguration
      parse "KIT"             = pure KitConfiguration
      parse o                 = fail $ "Unexpected StorageConfiguration: " <> show o

newtype HsCode = HsCode
  { unHsCode :: Text
  } deriving (Eq, Show, ToJSON, FromJSON)

type CreationDate = ExpectedDate

data Flags = Flags
  { fResourceLocation :: Maybe ResponseResourceLocation
  , fResource         :: Maybe FlagsResource
  } deriving (Eq, Show)

instance FromJSON Flags where
  parseJSON = withObject "Flags" parse
    where
      parse o = Flags
                <$> o .:? "resourceLocation"
                <*> o .:? "resource"

data FlagsResource = FlagsResource
  { frIsMedia               :: IsMedia
  , frIsDeletable           :: IsDeletable
  , frHasPallet             :: Maybe HasPallet
  , frIsPackagedReadyToShip :: IsPackagedReadyToShip
  , frHasMasterCase         :: Maybe HasMasterCase
  , frIsFragile             :: IsFragile
  , frIsArchivable          :: IsArchivable
  , frIsLiquid              :: IsLiquid
  , frIsDangerous           :: Maybe IsDangerous
  , frIsPerishable          :: IsPerishable
  , frHasEditRestrictions   :: HasEditRestrictions
  , frHasInnerPack          :: Maybe HasInnerPack
  , frIsAdult               :: IsAdult
  } deriving (Eq, Show)

instance FromJSON FlagsResource where
  parseJSON = withObject "FlagsResource" parse
    where
      parse o = FlagsResource
                <$> o .:  "isMedia"
                <*> o .:  "isDeletable"
                <*> o .:? "hasPallet"
                <*> o .:  "isPackagedReadyToShip"
                <*> o .:? "hasMasterCase"
                <*> o .:  "isFragile"
                <*> o .:  "isArchivable"
                <*> o .:  "isLiquid"
                <*> o .:? "isDangerous"
                <*> o .:  "isPerishable"
                <*> o .:  "hasEditRestrictions"
                <*> o .:? "hasInnerPack"
                <*> o .:  "isAdult"

data IsAdult = Adult
  | NotAdult
  deriving (Eq, Show)

instance FromJSON IsAdult where
  parseJSON = withScientific "IsAdult" parse
    where
      parse 0 = pure NotAdult
      parse 1 = pure Adult
      parse o = fail $ "Unexpected IsAdult: " <> show o

instance ToJSON IsAdult where
  toJSON NotAdult = Number 0
  toJSON Adult    = Number 1

data HasInnerPack = HasInnerPack
  | NoInnerPack
  deriving (Eq, Show)

instance FromJSON HasInnerPack where
  parseJSON = withScientific "HasInnerPack" parse
    where
      parse 0 = pure NoInnerPack
      parse 1 = pure HasInnerPack
      parse o = fail $ "Unexpected HasInnerPack: " <> show o

instance ToJSON HasInnerPack where
  toJSON NoInnerPack  = Number 0
  toJSON HasInnerPack = Number 1

data HasEditRestrictions = EditRestrictions
  | NoEditRestrictions
  deriving (Eq, Show)

instance FromJSON HasEditRestrictions where
  parseJSON = withScientific "HasEditRestrictions" parse
    where
      parse 0 = pure NoEditRestrictions
      parse 1 = pure EditRestrictions
      parse o = fail $ "Unexpected HasEditRestrictions: " <> show o

data IsPerishable = Perishable
  | NotPerishable
  deriving (Eq, Show)

instance FromJSON IsPerishable where
  parseJSON = withScientific "IsPerishable" parse
    where
      parse 0 = pure NotPerishable
      parse 1 = pure Perishable
      parse o = fail $ "Unexpected IsPerishable: " <> show o

instance ToJSON IsPerishable where
  toJSON NotPerishable = Number 0
  toJSON Perishable    = Number 1

data IsDangerous = Dangerous
  | NotDangerous
  deriving (Eq, Show)

instance FromJSON IsDangerous where
  parseJSON = withScientific "IsDangerous" parse
    where
      parse 0 = pure NotDangerous
      parse 1 = pure Dangerous
      parse o = fail $ "Unexpected IsDangerous: " <> show o

instance ToJSON IsDangerous where
  toJSON NotDangerous = Number 0
  toJSON Dangerous    = Number 1

data IsLiquid = Liquid
  | NotLiquid
  deriving (Eq, Show)

instance FromJSON IsLiquid where
  parseJSON = withScientific "IsLiquid" parse
    where
      parse 0 = pure NotLiquid
      parse 1 = pure Liquid
      parse o = fail $ "Unexpected IsLiquid: " <> show o

instance ToJSON IsLiquid where
  toJSON NotLiquid = Number 0
  toJSON Liquid    = Number 1

data IsArchivable = Archivable
  | NotArchivable
  deriving (Eq, Show)

instance FromJSON IsArchivable where
  parseJSON = withScientific "IsArchivable" parse
    where
      parse 0 = pure NotArchivable
      parse 1 = pure Archivable
      parse o = fail $ "Unexpected IsArchivable: " <> show o

data IsFragile = Fragile
  | NotFragile
  deriving (Eq, Show)

instance FromJSON IsFragile where
  parseJSON = withScientific "IsFragile" parse
    where
      parse 0 = pure NotFragile
      parse 1 = pure Fragile
      parse o = fail $ "Unexpected IsFragile: " <> show o

instance ToJSON IsFragile where
  toJSON NotFragile = Number 0
  toJSON Fragile    = Number 1

data HasMasterCase = HasMasterCase
  | NoMasterCase
  deriving (Eq, Show)

instance FromJSON HasMasterCase where
  parseJSON = withScientific "HasMasterCase" parse
    where
      parse 0 = pure NoMasterCase
      parse 1 = pure HasMasterCase
      parse o = fail $ "Unexpected HasMasterCase: " <> show o

instance ToJSON HasMasterCase where
  toJSON NoMasterCase  = Number 0
  toJSON HasMasterCase = Number 1

data HasPallet = HasPallet
  | NoPallet
  deriving (Eq, Show)

instance FromJSON HasPallet where
  parseJSON = withScientific "HasPallet" parse
    where
      parse 0 = pure NoPallet
      parse 1 = pure HasPallet
      parse o = fail $ "Unexpected HasPallet: " <> show o

instance ToJSON HasPallet where
  toJSON NoPallet  = Number 0
  toJSON HasPallet = Number 1

data IsDeletable = Deletable
  | NotDeletable
  deriving (Eq, Show)

instance FromJSON IsDeletable where
  parseJSON = withScientific "IsDeletable" parse
    where
      parse 0 = pure NotDeletable
      parse 1 = pure Deletable
      parse o = fail $ "Unexpected IsDeletable: " <> show o

data IsMedia = Media
  | NotMedia
  deriving (Eq, Show)

instance FromJSON IsMedia where
  parseJSON = withScientific "IsMedia" parse
    where
      parse 0 = pure NotMedia
      parse 1 = pure Media
      parse o = fail $ "Unexpected IsMedia: " <> show o

instance ToJSON IsMedia where
  toJSON NotMedia = Number 0
  toJSON Media    = Number 1

newtype Category = Category
  { unCategory :: Text
  } deriving (Eq, Show, ToJSON, FromJSON)

newtype Status = Status
  { unStatus :: Text
  } deriving (Eq, Show, FromJSON)

data AlternateNamesResponse = AlternateNamesResponse
  { anResourceLocation :: Maybe ResponseResourceLocation
  , anResource         :: Maybe AlternateNamesResponseResource
  } deriving (Eq, Show)

instance FromJSON AlternateNamesResponse where
  parseJSON = withObject "AlternateNames" parse
    where
      parse o = AlternateNamesResponse
                <$> o .:? "resourceLocation"
                <*> o .:? "resource"

data AlternateNamesResponseResource = AlternateNamesResponseResource
  { anrPrevious :: Maybe ResponsePrevious
  , anrNext     :: Maybe ResponseNext
  , anrTotal    :: ResponseTotal
  , anrItems    :: AlternateNamesResponseResourceItems
  , anrOffset   :: ResponseOffset
  } deriving (Eq, Show)

instance FromJSON AlternateNamesResponseResource where
  parseJSON = withObject "AlternateNamesResource" parse
    where
      parse o = AlternateNamesResponseResource
                <$> o .:? "previous"
                <*> o .:? "next"
                <*> o .:  "total"
                <*> o .:  "items"
                <*> o .:  "offset"

newtype AlternateNamesResponseResourceItems = AlternateNamesResponseResourceItems
  { anriItems :: [AlternateNamesResponseResourceItem]
  } deriving (Eq, Show, FromJSON)

data AlternateNamesResponseResourceItem = AlternateNamesResponseResourceItem
  { anriResourceLocation :: Maybe ResponseResourceLocation
  , anriResource         :: AlternateNamesResponseResourceItemResource
  } deriving (Eq, Show)

instance FromJSON AlternateNamesResponseResourceItem where
  parseJSON = withObject "AlternateNamesResourceItem" parse
    where
      parse o = AlternateNamesResponseResourceItem
                <$> o .:? "resourceLocation"
                <*> o .:  "resource"

data AlternateNamesResponseResourceItemResource = AlternateNamesResponseResourceItemResource
  { anrirExternalid :: Maybe ExternalId
  , anrirName       :: Name
  , anrirProductId  :: ProductId
  } deriving (Eq, Show)

instance FromJSON AlternateNamesResponseResourceItemResource where
  parseJSON = withObject "AlternateNamesResourceItemResource" parse
    where
      parse o = AlternateNamesResponseResourceItemResource
                <$> o .:? "externalId"
                <*> o .:  "name"
                <*> o .:  "productId"

data Pallet = Pallet
  { pResourceLocation :: Maybe ResponseResourceLocation
  , pResource         :: Maybe PalletResource
  } deriving (Eq, Show)

instance FromJSON Pallet where
  parseJSON = withObject "Pallet" parse
    where
      parse o = Pallet
                <$> o .:? "resourceLocation"
                <*> o .:? "resource"

type PalletResource = BaseProductResponseMasterCaseResource

data InnerPack = InnerPack
  { ipResourceLocation :: Maybe ResponseResourceLocation
  , ipResource         :: Maybe InnerPackResource
  } deriving (Eq, Show)

instance FromJSON InnerPack where
  parseJSON = withObject "InnerPack" parse
    where
      parse o = InnerPack
                <$> o .:? "resourceLocation"
                <*> o .:? "resource"

type InnerPackResource = BaseProductResponseMasterCaseResource

data EnqueuedDimensions = EnqueuedDimensions
  { edResourceLocation :: Maybe ResponseResourceLocation
  , edResource         :: Maybe EnqueuedDimensionsResource
  } deriving (Eq, Show)

instance FromJSON EnqueuedDimensions where
  parseJSON = withObject "EnqueuedDimensions" parse
    where
      parse o = EnqueuedDimensions
                <$> o .:? "resourceLocation"
                <*> o .:? "resource"

data EnqueuedDimensionsResource = EnqueuedDimensionsResource
  { edrPrevious :: Maybe ResponsePrevious
  , edrNext     :: Maybe ResponseNext
  , edrTotal    :: ResponseTotal
  -- No idea what edrItems are supposed to be.
  -- There is nothing in the API docs and no model schema either.
  , edrItems    :: Maybe Array
  , edrOffset   :: ResponseOffset
  } deriving (Eq, Show)

instance FromJSON EnqueuedDimensionsResource where
  parseJSON = withObject "EnqueuedDimensionsResource" parse
    where
      parse o = EnqueuedDimensionsResource
                <$> o .:? "previous"
                <*> o .:? "next"
                <*> o .:  "total"
                <*> o .:  "items"
                <*> o .:  "offset"

newtype ItemCount = ItemCount
  { unItemCount :: Integer
  } deriving (Eq, Show, FromJSON)

type ArchivedDate = ExpectedDate

data Classification = BaseProductClassification
  | MarketingInsertClassification
  | VirtualKitClassification
  | KitClassification
  deriving (Eq, Show)

instance ToJSON Classification where
  toJSON BaseProductClassification     = String "baseProduct"
  toJSON MarketingInsertClassification = String "marketingInsert"
  toJSON VirtualKitClassification      = String "virtualKit"
  toJSON KitClassification             = String "kit"

instance FromJSON Classification where
  parseJSON = withText "Classification" parse
    where
      parse "baseProduct"     = pure BaseProductClassification
      parse "marketingInsert" = pure MarketingInsertClassification
      parse "virtualKit"      = pure VirtualKitClassification
      parse "kit"             = pure KitClassification
      parse o                 = fail $ "Unexpected Classification: " <> show o

data BatteryConfiguration = NoBatteryConfiguration
  | IsBatteryConfiguration
  | HasLooseBatteryConfiguration
  deriving (Eq, Show)

instance ToJSON BatteryConfiguration where
  toJSON NoBatteryConfiguration       = String "NOBATTERY"
  toJSON IsBatteryConfiguration       = String "ISBATTERY"
  toJSON HasLooseBatteryConfiguration = String "HASLOOSEBATTERY"

instance FromJSON BatteryConfiguration where
  parseJSON = withText "BatteryConfiguration" parse
    where
      parse "NOBATTERY"       = pure NoBatteryConfiguration
      parse "ISBATTERY"       = pure IsBatteryConfiguration
      parse "HASLOOSEBATTERY" = pure HasLooseBatteryConfiguration
      parse o                 = fail $ "Unexpected BatteryConfiguration: " <> show o

data BaseProductResponseMasterCase = BaseProductResponseMasterCase
  { mcResourceLocation :: Maybe ResponseResourceLocation
  , mcResource         :: Maybe BaseProductResponseMasterCaseResource
  } deriving (Eq, Show)

instance FromJSON BaseProductResponseMasterCase where
  parseJSON = withObject "MasterCase" parse
    where
      parse o = BaseProductResponseMasterCase
                <$> o .:? "resourceLocation"
                <*> o .:? "resource"

data BaseProductResponseMasterCaseResource = BaseProductResponseMasterCaseResource
  { mcrSku                    :: Maybe SKU
  , mcrDimensions             :: Maybe Dimensions
  , mcrValues                 :: Maybe ValuesResource
  , mcrExternalId             :: Maybe ExternalId
  , mcrIndividualItemsPerCase :: Maybe IndividualItemsPerCase
  , mcrFlags                  :: Maybe MasterCaseFlags
  , mcrProductId              :: Maybe ProductId
  , mcrDescription            :: Maybe Description
  } deriving (Eq, Show)

instance FromJSON BaseProductResponseMasterCaseResource where
  parseJSON = withObject "MasterCaseResource" parse
    where
      parse o = BaseProductResponseMasterCaseResource
                <$> o .:? "sku"
                <*> o .:? "dimensions"
                <*> o .:? "values"
                <*> o .:? "externalId"
                <*> o .:? "individualItemsPerCase"
                <*> o .:? "flags"
                <*> o .:? "productId"
                <*> o .:? "description"

data Dimensions = Dimensions
  { dResourceLocation :: Maybe ResponseResourceLocation
  , dResource         :: Maybe DimensionsResource
  } deriving (Eq, Show)

instance FromJSON Dimensions where
  parseJSON = withObject "Dimensions" parse
    where
      parse o = Dimensions
                <$> o .:? "resourceLocation"
                <*> o .:? "resource"

data DimensionsResource = DimensionsResource
  { drWeight     :: DimensionsWeight
  , drWeightUnit :: WeightUnit
  , drHeight     :: DimensionsHeight
  , drHeightUnit :: HeightUnit
  , drWidth      :: DimensionsWidth
  , drWidthUnit  :: WidthUnit
  , drLength     :: DimensionsLength
  , drLengthUnit :: LengthUnit
  } deriving (Eq, Show)

instance FromJSON DimensionsResource where
  parseJSON = withObject "DimensionsResource" parse
    where
      parse o = DimensionsResource
                <$> o .: "weight"
                <*> o .: "weightUnit"
                <*> o .: "height"
                <*> o .: "heightUnit"
                <*> o .: "width"
                <*> o .: "widthUnit"
                <*> o .: "length"
                <*> o .: "lengthUnit"

newtype DimensionsWeight = DimensionsWeight
  { unDimensionsWeight :: Text
  } deriving (Eq, Show, FromJSON)

type WeightUnit = PieceWeightUnits

newtype DimensionsHeight = DimensionsHeight
  { unDimensionsHeight :: Text
  } deriving (Eq, Show, FromJSON)

type HeightUnit = PieceHeightUnits

newtype DimensionsWidth = DimensionsWidth
  { unDimensionsWidth :: Text
  } deriving (Eq, Show, FromJSON)

type WidthUnit = PieceWidthUnits

newtype DimensionsLength = DimensionsLength
  { unDimensionsLength :: Text
  } deriving (Eq, Show, FromJSON)

type LengthUnit = PieceLengthUnits

data ValuesResponse = ValuesResponse
  { vrResourceLocation :: Maybe ResponseResourceLocation
  , vrResource         :: Maybe ValuesResource
  } deriving (Eq, Show)

instance FromJSON ValuesResponse where
  parseJSON = withObject "ValuesResponse" parse
    where
      parse o = ValuesResponse
                <$> o .:? "resourceLocation"
                <*> o .:? "resource"

data ValuesResource = ValuesResource
  { vrCostValueCurrency      :: Maybe CostValueCurrency
  , vrWholesaleValue         :: Maybe WholesaleValueResponse
  , vrCostValue              :: Maybe CostValueResponse
  , vrWholesaleValueCurrency :: Maybe WholesaleValueCurrency
  , vrRetailValue            :: Maybe RetailValueResponse
  , vrRetailValueCurrency    :: Maybe RetailValueCurrency
  } deriving (Eq, Show)

data Values = Values
  { vCostValue         :: CostValue
  , vWholesaleValue    :: Maybe WholesaleValue
  , vRetailValue       :: RetailValue
  , vCostCurrency      :: Maybe CostCurrency
  , vWholesaleCurrency :: Maybe WholesaleCurrency
  , vRetailCurrency    :: Maybe RetailCurrency
  } deriving (Eq, Show)

instance ToJSON Values where
  toJSON Values {..} = omitNulls ["costValue"         .= vCostValue
                                 ,"wholesaleValue"    .= vWholesaleValue
                                 ,"retailValue"       .= vRetailValue
                                 ,"costCurrency"      .= vCostCurrency
                                 ,"wholesaleCurrency" .= vWholesaleCurrency
                                 ,"retailCurrency"    .= vRetailCurrency]

instance FromJSON ValuesResource where
  parseJSON = withObject "ValuesResource" parse
    where
      parse o = ValuesResource
                <$> o .:? "costValueCurrency"
                <*> o .:? "wholesaleValue"
                <*> o .:? "costValue"
                <*> o .:? "wholesaleValueCurrency"
                <*> o .:? "retailValue"
                <*> o .:? "retailValueCurrency"

newtype WholesaleValueCurrency = WholesaleValueCurrency
  { unWholesaleValueCurrency :: Text
  } deriving (Eq, Show, ToJSON, FromJSON)

newtype WholesaleCurrency = WholesaleCurrency
  { unWholesaleCurrency :: Text
  } deriving (Eq, Show, ToJSON)

newtype RetailValueCurrency = RetailValueCurrency
  { unRetailValueCurrency :: Text
  } deriving (Eq, Show, ToJSON, FromJSON)

newtype RetailCurrency = RetailCurrency
  { unRetailCurrency :: Text
  } deriving (Eq, Show, ToJSON)

newtype CostValueCurrency = CostValueCurrency
  { unCostValueCurrency :: Text
  } deriving (Eq, Show, ToJSON, FromJSON)

newtype CostValueResponse = CostValueResponse
  { unCostValueR :: Text
  } deriving (Eq, Show, FromJSON)

newtype CostValue = CostValue
  { unCostValue :: Centi
  } deriving (Eq, Show, ToJSON)

newtype WholesaleValue = WholesaleValue
  { unWholesaleValue :: Centi
  } deriving (Eq, Show, ToJSON)

newtype WholesaleValueResponse = WholesaleValueResponse
  { unWholesaleValeuResponse :: Text
  } deriving (Eq, Show, FromJSON)

newtype RetailValue = RetailValue
  { unRetailValue :: Centi
  } deriving (Eq, Show, ToJSON)

newtype RetailValueResponse = RetailValueResponse
  { unRetailValueResponsee :: Text
  } deriving (Eq, Show, FromJSON)

newtype IndividualItemsPerCase = IndividualItemsPerCase
  { unIndividualItemsPerCase :: Integer
  } deriving (Eq, Show, ToJSON, FromJSON)

data MasterCaseFlags = MasterCaseFlags
  { mcfResourceLocation :: Maybe ResponseResourceLocation
  , mcfResource         :: MasterCaseFlagsResource
  } deriving (Eq, Show)

instance FromJSON MasterCaseFlags where
  parseJSON = withObject "Flags" parse
    where
      parse o = MasterCaseFlags
                <$> o .:? "resourceLocation"
                <*> o .:  "resource"

newtype MasterCaseFlagsResource = MasterCaseFlagsResource
  { unFlagsResource :: IsPackagedReadyToShip
  } deriving (Eq, Show, ToJSON, FromJSON)

data IsPackagedReadyToShip = PackagedReadyToShip
  | NotPackagedReadyToShip
  deriving (Eq, Show)

instance FromJSON IsPackagedReadyToShip where
  parseJSON = withScientific "IsPackagedReadyToShip" parse
    where
      parse 0 = pure NotPackagedReadyToShip
      parse 1 = pure PackagedReadyToShip
      parse o = fail $ "Unexpected IsPackagedReadyToShip: " <> show o

instance ToJSON IsPackagedReadyToShip where
  toJSON NotPackagedReadyToShip = Number 0
  toJSON PackagedReadyToShip    = Number 1

newtype CountryOfOrigin = CountryOfOrigin
  { unCountryOfOrigin :: Text
  } deriving (Eq, Show, ToJSON, FromJSON)

----------------------------------------------------------------
-- Order Endpoint https://www.shipwire.com/w/developers/order --
----------------------------------------------------------------

-- | GET /api/v3/orders
data GetOrdersRequest
type instance ShipwireReturn GetOrdersRequest = GenericResponse GetOrdersResponseResource

instance ShipwireHasParam GetOrdersRequest ExpandOrdersParam
instance ShipwireHasParam GetOrdersRequest CommerceNameParam
instance ShipwireHasParam GetOrdersRequest TransactionIdParam
instance ShipwireHasParam GetOrdersRequest OrderIdParam
instance ShipwireHasParam GetOrdersRequest OrderNoParam
instance ShipwireHasParam GetOrdersRequest ReferrerParam
instance ShipwireHasParam GetOrdersRequest ExternalIdParam
instance ShipwireHasParam GetOrdersRequest OrderStatusParam
instance ShipwireHasParam GetOrdersRequest UpdatedAfter
instance ShipwireHasParam GetOrdersRequest WarehouseIdParam
instance ShipwireHasParam GetOrdersRequest WarehouseExternalIdParam

-- | GET /api/v3/orders/{id} or /api/v3/orders/E{externalId}
data GetOrderRequest
type instance ShipwireReturn GetOrderRequest = GenericResponse GetOrderResponseResource

instance ShipwireHasParam GetOrderRequest ExpandOrdersParam

-- | POST /api/v3/orders
data CreateOrderRequest
type instance ShipwireReturn CreateOrderRequest = GenericResponse GetOrdersResponseResource

instance ShipwireHasParam CreateOrderRequest ExpandOrdersParam

-- | POST /api/v3/orders/{id}/cancel or /api/v3/orders/E{externalId}/cancel
data CancelOrderRequest
type instance ShipwireReturn CancelOrderRequest = CancelOrderResponse

type CancelOrderResponse = SimpleResponse

-- | GET /api/v3/orders/{id}/trackings or /api/v3/orders/E{externalId}/trackings
data GetOrderTrackingsRequest
type instance ShipwireReturn GetOrderTrackingsRequest = GenericResponse GetOrderTrackingsResponseResource

type GetOrderResponseResource = GetOrdersResponseResourceItemResource

data GetOrderTrackingsResponseResource = GetOrderTrackingsResponseResource
  { gotrrItems    :: GetOrderTrackingsResponseResourceItems
  , gotrrNext     :: Maybe ResponseNext
  , gotrrOffset   :: ResponseOffset
  , gotrrPrevious :: Maybe ResponsePrevious
  , gotrrTotal    :: ResponseTotal
  } deriving (Eq, Show)

instance FromJSON GetOrderTrackingsResponseResource where
  parseJSON = withObject "GetOrderTrackingsResponseResource" parse
    where
      parse o = GetOrderTrackingsResponseResource
                <$> o .:  "items"
                <*> o .:? "next"
                <*> o .:  "offset"
                <*> o .:? "previous"
                <*> o .:  "total"

newtype GetOrderTrackingsResponseResourceItems = GetOrderTrackingsResponseResourceItems
  { gotrriItems :: [GetOrderTrackingsResponseResourceItem]
  } deriving (Eq, Show, FromJSON)

data GetOrderTrackingsResponseResourceItem = GetOrderTrackingsResponseResourceItem
  { gotrriResource         :: GetOrderTrackingsResponseResourceItemResource
  , gotrriResourceLocation :: ResponseResourceLocation
  } deriving (Eq, Show)

instance FromJSON GetOrderTrackingsResponseResourceItem where
  parseJSON = withObject "GetOrderTrackingsResponseResourceItem" parse
    where
      parse o = GetOrderTrackingsResponseResourceItem
                <$> o .: "resource"
                <*> o .: "resourceLocation"

data GetOrderTrackingsResponseResourceItemResource = GetOrderTrackingsResponseResourceItemResource
  { gotrrirId                  :: Id
  , gotrrirOrderId             :: OrderId
  , gottrirOrderExternalId     :: OrderExternalId
  , gotrrirTracking            :: Tracking
  , gotrrirCarrier             :: CarrierName
  , gotrrirUrl                 :: URL
  , gotrrirSummary             :: Summary
  , gotrrirSummaryDate         :: SummaryDate
  , gotrrirLabelCreatedDate    :: LabelCreatedDate
  , gotrrirTrackedDate         :: TrackedDate
  , gotrrirFirstScanDate       :: FirstScanDate
  , gotrrirFirstScanRegion     :: FirstScanRegion
  , gotrrirFirstScanPostalCode :: FirstScanPostalCode
  , gotrrirFirstScanCountry    :: FirstScanCountry
  , gotrrirDeliveredDate       :: DeliveredDate
  , gotrrirDeliveryCity        :: DeliveryCity
  , gotrrirDeliveryRegion      :: DeliveryRegion
  , gotrrirDeliveryPostalCode  :: DeliveryPostalCode
  , gotrrirDeliveryCountry     :: DeliveryCountry
  } deriving (Eq, Show)

instance FromJSON GetOrderTrackingsResponseResourceItemResource where
  parseJSON = withObject "GetOrderTrackingsResponseResourceItemResource" parse
    where
      parse o = GetOrderTrackingsResponseResourceItemResource
                <$> o .: "id"
                <*> o .: "orderId"
                <*> o .: "orderExternalId"
                <*> o .: "tracking"
                <*> o .: "carrier"
                <*> o .: "url"
                <*> o .: "summary"
                <*> o .: "summaryDate"
                <*> o .: "labelCreatedDate"
                <*> o .: "trackedDate"
                <*> o .: "firstScanDate"
                <*> o .: "firstScanRegion"
                <*> o .: "firstScanPostalCode"
                <*> o .: "firstScanCountry"
                <*> o .: "deliveredDate"
                <*> o .: "deliveryCity"
                <*> o .: "deliveryRegion"
                <*> o .: "deliveryPostalCode"
                <*> o .: "deliveryCountry"

data IdWrapper = WrappedId Id
  | WrappedExternalId ExternalId
  deriving (Eq, Show)

instance ToJSON IdWrapper where
  toJSON (WrappedId x) = toJSON x
  toJSON (WrappedExternalId x) = toJSON x

newtype OrderStatusParam = OrderStatusParam
  { unStatusParam :: [OrderStatus]
  } deriving (Eq, Show)

orderStatusParamToBS8 :: OrderStatus -> BS8.ByteString
orderStatusParamToBS8 OrderProcessed = "processed"
orderStatusParamToBS8 OrderCanceled  = "canceled"
orderStatusParamToBS8 OrderCompleted = "completed"
orderStatusParamToBS8 OrderDelivered = "delivered"
orderStatusParamToBS8 OrderReturned  = "returned"
orderStatusParamToBS8 OrderSubmitted = "submitted"
orderStatusParamToBS8 OrderHeld      = "held"
orderStatusParamToBS8 OrderTracked   = "tracked"

instance ToShipwireParam OrderStatusParam where
  toShipwireParam (OrderStatusParam xs) =
    joinQueryParams $ Params Nothing [Query ("status", BS8.intercalate "," (map orderStatusParamToBS8 xs))]

newtype ReferrerParam = ReferrerParam
  { unReferrerParam :: [Text]
  } deriving (Eq, Show)

instance ToShipwireParam ReferrerParam where
  toShipwireParam (ReferrerParam xs) =
    joinQueryParams $ Params Nothing [Query ("referrer", TE.encodeUtf8 (T.intercalate "," xs))]

data ExpandOrders = OrdersExpandAll
  | OrdersExpandHolds
  | OrdersExpandItems
  | OrdersExpandReturns
  | OrdersExpandTrackings
  | OrdersExpandSplitOrders
  deriving (Eq, Show)

newtype ExpandOrdersParam = ExpandOrdersParam
  { unExpandOrdersParam :: [ExpandOrders]
  } deriving (Eq, Show)

expandOrdersToBS8 :: ExpandOrders -> BS8.ByteString
expandOrdersToBS8 OrdersExpandAll         = "all"
expandOrdersToBS8 OrdersExpandHolds       = "holds"
expandOrdersToBS8 OrdersExpandItems       = "items"
expandOrdersToBS8 OrdersExpandReturns     = "returns"
expandOrdersToBS8 OrdersExpandTrackings   = "trackings"
expandOrdersToBS8 OrdersExpandSplitOrders = "splitOrders"

instance ToShipwireParam ExpandOrdersParam where
  toShipwireParam (ExpandOrdersParam xs) =
    joinQueryParams $ Params Nothing [Query ("expand", BS8.intercalate "," (map expandOrdersToBS8 xs))]

data GetOrdersResponseResource = GetOrdersResponseResource
  { gorrItems    :: GetOrdersResponseResourceItems
  , gorrNext     :: Maybe ResponseNext
  , gorrOffset   :: ResponseOffset
  , gorrPrevious :: Maybe ResponsePrevious
  , gorrTotal    :: ResponseTotal
  } deriving (Eq, Show)

instance FromJSON GetOrdersResponseResource where
  parseJSON = withObject "GetOrdersResponseResource" parse
    where
      parse o = GetOrdersResponseResource
                <$> o .:  "items"
                <*> o .:? "next"
                <*> o .:  "offset"
                <*> o .:? "previous"
                <*> o .:  "total"

newtype GetOrdersResponseResourceItems = GetOrdersResponseResourceItems
  { gorriItems :: [GetOrdersResponseResourceItem]
  } deriving (Eq, Show, FromJSON)

data GetOrdersResponseResourceItem = GetOrdersResponseResourceItem
  { gorriResource         :: GetOrdersResponseResourceItemResource
  , gorriResourceLocation :: ResponseResourceLocation
  } deriving (Eq, Show)

instance FromJSON GetOrdersResponseResourceItem where
  parseJSON = withObject "GetOrdersResponseResourceItem" parse
    where
      parse o = GetOrdersResponseResourceItem
                <$> o .: "resource"
                <*> o .: "resourceLocation"

data GetOrdersResponseResourceItemResource = GetOrdersResponseResourceItemResource
  { gorrirExternalId         :: Maybe ExternalId
  , gorrirOrderNo            :: Maybe OrderNo
  , gorrirCommerceName       :: Maybe CommerceName
  , gorrirProcessAfterDate   :: Maybe ProcessAfterDate
  , gorrirStatus             :: OrderStatus
  , gorrirLastUpdatedDate    :: LastUpdatedDate
  , gorrirId                 :: Id
  , gorrirTransactionId      :: TransactionId
  , gorrirNeedsReview        :: NeedsReview
  , gorrirHolds              :: GetOrdersHolds
  , gorrirItems              :: GetOrdersItems
  , gorrirTrackings          :: GetOrdersTrackings
  , gorrirReturns            :: GetOrdersReturns
  , gorrirOptions            :: GetOrdersOptions
  , gorrirShipFrom           :: OrderShipFromResponse
  , gorrirShipTo             :: OrderShipToResponse
  , gorrirCommercialInvoice  :: CommercialInvoiceResponse
  , gorrirPackingList        :: PackingListResponse
  , gorrirShippingLabel      :: ShippingLabelResponse
  , gorrirRouting            :: RoutingResponse
  , gorrirEvents             :: EventsResponse
  , gorrirPricing            :: PricingResponse
  , gorrirPricingEstimate    :: PricingEstimateResponse
  , gorrirShipwireAnywhere   :: ShipwireAnywhereResponse
  , gorrirSplitOrders        :: SplitOrdersResponse
  , gorrirFreightSummary     :: Maybe FreightSummaryResponse
  , gorrirExtendedAttributes :: Maybe ExtendedAttributesResponse
  } deriving (Eq, Show)

instance FromJSON GetOrdersResponseResourceItemResource where
  parseJSON = withObject "GetOrdersResponseResourceItemResource" parse
    where
      parse o = GetOrdersResponseResourceItemResource
                <$> o .:? "externalId"
                <*> o .:? "orderNo"
                <*> o .:? "commerceName"
                <*> o .:? "processAfterDate"
                <*> o .:  "status"
                <*> o .:  "lastUpdatedDate"
                <*> o .:  "id"
                <*> o .:  "transactionId"
                <*> o .:  "needsReview"
                <*> o .:  "holds"
                <*> o .:  "items"
                <*> o .:  "trackings"
                <*> o .:  "returns"
                <*> o .:  "options"
                <*> o .:  "shipFrom"
                <*> o .:  "shipTo"
                <*> o .:  "commercialInvoice"
                <*> o .:  "packingList"
                <*> o .:  "shippingLabel"
                <*> o .:  "routing"
                <*> o .:  "events"
                <*> o .:  "pricing"
                <*> o .:  "pricingEstimate"
                <*> o .:  "shipwireAnywhere"
                <*> o .:  "splitOrders"
                <*> o .:? "freightSummary"
                <*> o .:? "extendedAttributes"

data ExtendedAttributesResponse = ExtendedAttributesResponse
  { earResource         :: Maybe ExtendedAttributesResponseResource
  , earResourceLocation :: Maybe ResponseResourceLocation
  } deriving (Eq, Show)

instance FromJSON ExtendedAttributesResponse where
  parseJSON = withObject "ExtendedAttributesResponse" parse
    where
      parse o = ExtendedAttributesResponse
                <$> o .:? "resource"
                <*> o .:? "resourceLocation"

data ExtendedAttributesResponseResource = ExtendedAttributesResponseResource
  { earrOffset   :: Offset
  , earrTotal    :: Total
  , earrPrevious :: Maybe Previous
  , earrNext     :: Maybe Next
  , earrItems    :: ExtendedAttributesResponseResourceItems
  } deriving (Eq, Show)

instance FromJSON ExtendedAttributesResponseResource where
  parseJSON = withObject "ExtendedAttributesResponseResource" parse
    where
      parse o = ExtendedAttributesResponseResource
                <$> o .:  "offset"
                <*> o .:  "total"
                <*> o .:? "previous"
                <*> o .:? "next"
                <*> o .:  "items"

newtype ExtendedAttributesResponseResourceItems = ExtendedAttributesResponseResourceItems
  { earriItems :: [ExtendedAttributesResponseResourceItem]
  } deriving (Eq, Show, FromJSON)

data ExtendedAttributesResponseResourceItem = ExtendedAttributesResponseResourceItem
  { earriResource         :: ExtendedAttributesResponseResourceItemResource
  , earriResourceLocation :: Maybe ResponseResourceLocation
  } deriving (Eq, Show)

instance FromJSON ExtendedAttributesResponseResourceItem where
  parseJSON = withObject "ExtendedAttributesResponseResourceItem" parse
    where
      parse o = ExtendedAttributesResponseResourceItem
                <$> o .:  "resource"
                <*> o .:? "resourceLocation"

data ExtendedAttributesResponseResourceItemResource = ExtendedAttributesResponseResourceItemResource
  { earrirName  :: Name
  , earrirValue :: ExtendedAttributeValue
  , earrirType  :: Type
  } deriving (Eq, Show)

instance FromJSON ExtendedAttributesResponseResourceItemResource where
  parseJSON = withObject "ExtendedAttributesResponseResourceItemResource" parse
    where
      parse o = ExtendedAttributesResponseResourceItemResource
                <$> o .: "name"
                <*> o .: "value"
                <*> o .: "type"

newtype ExtendedAttributeValue = ExtendedAttributeValue
  { unExtendedAtttributeValue :: Double
  } deriving (Eq, Show, FromJSON)

data FreightSummaryResponse = FreightSummaryResponse
  { fsrResource         :: FreightSummaryResponseResource
  , fsrResourceLocation :: Maybe ResponseResourceLocation
  } deriving (Eq, Show)

instance FromJSON FreightSummaryResponse where
  parseJSON = withObject "FreightSummaryResponse" parse
    where
      parse o = FreightSummaryResponse
                <$> o .:  "resource"
                <*> o .:? "resourceLocation"

data FreightSummaryResponseResource = FreightSummaryResponseResource
  { fsrrTotalWeight     :: FreightSummaryTotalWeight
  , fsrrWeightUnit      :: Maybe WeightUnit
  , fsrrMeasurementType :: Maybe MeasurementType
  } deriving (Eq, Show)

instance FromJSON FreightSummaryResponseResource where
  parseJSON = withObject "FreightSummaryResponseResource" parse
    where
      parse o = FreightSummaryResponseResource
                <$> o .:  "totalWeight"
                <*> o .:? "weightUnit"
                <*> o .:? "measurementType"

newtype FreightSummaryTotalWeight = FreightSummaryTotalWeight
  { unFreightSummaryTotalWeight :: Text
  } deriving (Eq, Show, FromJSON)

newtype MeasurementType = MeasurementType
  { unMeasurementType :: Text
  } deriving (Eq, Show, FromJSON)

data OrderStatus = OrderProcessed
  | OrderCanceled
  | OrderCompleted
  | OrderDelivered
  | OrderReturned
  | OrderSubmitted
  | OrderHeld
  | OrderTracked
  deriving (Eq, Show)

instance FromJSON OrderStatus where
  parseJSON = withText "OrderStatus" parse
    where
      parse "processed" = pure OrderProcessed
      parse "cancelled" = pure OrderCanceled
      parse "completed" = pure OrderCompleted
      parse "delivered" = pure OrderDelivered
      parse "returned"  = pure OrderReturned
      parse "submitted" = pure OrderSubmitted
      parse "held"      = pure OrderHeld
      parse "tracked"   = pure OrderTracked
      parse o           = fail $ "Unexpected OrderStatus: " <> show o

data SplitOrdersResponse = SplitOrdersResponse
  { sorResource         :: Maybe SplitOrdersResponseResource
  , sorResourceLocation :: ResponseResourceLocation
  } deriving (Eq, Show)

instance FromJSON SplitOrdersResponse where
  parseJSON = withObject "SplitOrdersResponse" parse
    where
      parse o = SplitOrdersResponse
                <$> o .:? "resource"
                <*> o .:  "resourceLocation"

data SplitOrdersResponseResource = SplitOrdersResponseResource
  { sorrItems    :: SplitOrdersResponseResourceItems
  , sorrNext     :: Maybe ResponseNext
  , sorrOffset   :: ResponseOffset
  , sorrPrevious :: Maybe ResponsePrevious
  , sorrTotal    :: ResponseTotal
  } deriving (Eq, Show)

instance FromJSON SplitOrdersResponseResource where
  parseJSON = withObject "SplitOrdersResponseResource" parse
    where
      parse o = SplitOrdersResponseResource
                <$> o .:  "items"
                <*> o .:? "next"
                <*> o .:  "offset"
                <*> o .:? "previous"
                <*> o .:  "total"

newtype SplitOrdersResponseResourceItems = SplitOrdersResponseResourceItems
  { sorriItems :: Array
  } deriving (Eq, Show, FromJSON)

data ShipwireAnywhereResponse = ShipwireAnywhereResponse
  { sarResource         :: ShipwireAnywhereResponseResource
  , sarResourceLocation :: Maybe ResponseResourceLocation
  } deriving (Eq, Show)

instance FromJSON ShipwireAnywhereResponse where
  parseJSON = withObject "ShipwireAnywhereResponse" parse
    where
      parse o = ShipwireAnywhereResponse
                <$> o .:  "resource"
                <*> o .:  "resourceLocation"

newtype ShipwireAnywhereResponseResource = ShipwireAnywhereResponseResource
  { sarrStatus :: Maybe Status
  } deriving (Eq, Show)

instance FromJSON ShipwireAnywhereResponseResource where
  parseJSON = withObject "ShipwireAnywhereResponseResource" parse
    where
      parse o = ShipwireAnywhereResponseResource
                <$> o .:? "status"

data PricingEstimateResponse = PricingEstimateResponse
  { perResource         :: PricingEstimateResource
  , perResourceLocation :: Maybe ResponseResourceLocation
  } deriving (Eq, Show)

instance FromJSON PricingEstimateResponse where
  parseJSON = withObject "PricingEstimateResponse" parse
    where
      parse o = PricingEstimateResponse
                <$> o .:  "resource"
                <*> o .:? "resourceLocation"

type PricingEstimateResource = PricingResponseResource

data PricingResponse = PricingResponse
  { prResource         :: PricingResponseResource
  , prResourceLocation :: Maybe ResponseResourceLocation
  } deriving (Eq, Show)

instance FromJSON PricingResponse where
  parseJSON = withObject "PricingResponse" parse
    where
      parse o = PricingResponse
                <$> o .:  "resource"
                <*> o .:? "resourceLocation"

data PricingResponseResource = PricingResponseResource
  { prrShipping  :: ShippingValue
  , prrPackaging :: PackagingValue
  , prrInsurance :: InsuranceValue
  , prrHandling  :: HandlingValue
  , prrTotal     :: TotalValue
  } deriving (Eq, Show)

instance FromJSON PricingResponseResource where
  parseJSON = withObject "PricingResponseResource" parse
    where
      parse o = PricingResponseResource
                <$> o .: "shipping"
                <*> o .: "packaging"
                <*> o .: "insurance"
                <*> o .: "handling"
                <*> o .: "total"

newtype TotalValue = TotalValue
  { unTotalValue :: Centi
  } deriving (Eq, Show, FromJSON)

newtype HandlingValue = HandlingValue
  { unHandlingValue :: Centi
  } deriving (Eq, Show, FromJSON)

newtype PackagingValue = PackagingValue
  { unPackagingValue :: Centi
  } deriving (Eq, Show, FromJSON)

data RoutingResponse = RoutingResponse
  { rrResource         :: RoutingResponseResource
  , rrResourceLocation :: Maybe ResponseResourceLocation
  } deriving (Eq, Show)

instance FromJSON RoutingResponse where
  parseJSON = withObject "RoutingResponse" parse
    where
      parse o = RoutingResponse
                <$> o .:  "resource"
                <*> o .:? "resourceLocation"

data RoutingResponseResource = RoutingResponseResource
  { rrrWarehouseId          :: Maybe WarehouseId
  , rrrWarehouseExternalId  :: Maybe WarehouseExternalId
  , rrrWarehouseName        :: Maybe WarehouseName
  , rrrDestinationLatitude  :: Maybe DestinationLatitudeResponse
  , rrrDestinationLongitude :: Maybe DestinationLongitudeResponse
  , rrrOriginLatitude       :: OriginLatitudeResponse
  , rrrOriginLongitude      :: OriginLongitudeResponse
  } deriving (Eq, Show)

instance FromJSON RoutingResponseResource where
  parseJSON = withObject "RoutingResponseResource" parse
    where
      parse o = RoutingResponseResource
                <$> o .:? "warehouseId"
                <*> o .:? "warehouseExternalId"
                <*> o .:? "warehouseName"
                <*> o .:? "destinationLatitude"
                <*> o .:? "destinationLongitude"
                <*> o .:  "originLatitude"
                <*> o .:  "originLongitude"

newtype DestinationLatitudeResponse = DestinationLatitudeResponse
  { unDestinationLatitudeResponse :: Text
  } deriving (Eq, Show, FromJSON)

newtype DestinationLongitudeResponse = DestinationLongitudeResponse
  { unDestinationLongitudeResponse :: Text
  } deriving (Eq, Show, FromJSON)

type OriginLatitudeResponse       = Latitude

type OriginLongitudeResponse      = Longitude

data ShippingLabelResponse = ShippingLabelResponse
  { slrResource         :: Maybe ShippingLabelResponseResource
  , slrResourceLocation :: Maybe ResponseResourceLocation
  } deriving (Eq, Show)

instance FromJSON ShippingLabelResponse where
  parseJSON = withObject "ShippingLabelResponse" parse
    where
      parse o = ShippingLabelResponse
                <$> o .:? "resource"
                <*> o .:? "resourceLocation"

data ShippingLabelResponseResource = ShippingLabelResponseResource
  { slrrOrderId             :: OrderId
  , slrrExternalid          :: Maybe ExternalId
  , slrrWarehouseId         :: Maybe WarehouseId
  , slrrWarehouseExternalId :: Maybe WarehouseExternalId
  , slrrDocumentLocation    :: DocumentLocation
  } deriving (Eq, Show)

instance FromJSON ShippingLabelResponseResource where
  parseJSON = withObject "ShippingLabelResponseResource" parse
    where
      parse o = ShippingLabelResponseResource
                <$> o .:  "orderId"
                <*> o .:? "externalId"
                <*> o .:? "warehouseId"
                <*> o .:? "warehouseExternalId"
                <*> o .:  "documentLocation"

data PackingListResponse = PackingListResponse
  { plrResource         :: Maybe PackingListResponseResource
  , plrResourceLocation :: Maybe ResponseResourceLocation
  } deriving (Eq, Show)

instance FromJSON PackingListResponse where
  parseJSON = withObject "PackingListResponse" parse
    where
      parse o = PackingListResponse
                <$> o .:? "resource"
                <*> o .:? "resourceLocation"

data PackingListResponseResource = PackingListResponseResource
  { plrrMessage1         :: PackingListResponseResourceMessage
  , plrrMessage2         :: PackingListResponseResourceMessage
  , plrrMessage3         :: PackingListResponseResourceMessage
  , plrrOther            :: PackingListResponseResourceOther
  , plrrDocumentLocation :: DocumentLocation
  } deriving (Eq, Show)

instance FromJSON PackingListResponseResource where
  parseJSON = withObject "PackingListResponseResource" parse
    where
      parse o = PackingListResponseResource
                <$> o .: "message1"
                <*> o .: "message2"
                <*> o .: "message3"
                <*> o .: "other"
                <*> o .: "documentLocation"

data PackingListResponseResourceOther = PackingListResponseResourceOther
  { plrroResource         :: PackingListResponseResourceOtherResource
  , plrroResourceLocation :: Maybe ResponseResourceLocation
  } deriving (Eq, Show)

instance FromJSON PackingListResponseResourceOther where
  parseJSON = withObject "PackingListResponseResourceOther" parse
    where
      parse o = PackingListResponseResourceOther
                <$> o .:  "resource"
                <*> o .:? "resourceLocation"

type PackingListResponseResourceOtherResource = PackingListOther

data PackingListResponseResourceMessage = PackingListResponseResourceMessage
  { plrrmResource         :: PackingListResponseResourceMessageResource
  , plrrmResourceLocation :: Maybe ResponseResourceLocation
  } deriving (Eq, Show)

instance FromJSON PackingListResponseResourceMessage where
  parseJSON = withObject "PackingListResponseResourceMessage" parse
    where
      parse o = PackingListResponseResourceMessage
                <$> o .:  "resource"
                <*> o .:? "resourceLocation"

type PackingListResponseResourceMessageResource = PackingListMessage

data CommercialInvoiceResponse = CommercialInvoiceResponse
  { cirResource         :: Maybe CommercialInvoiceResponseResource
  , cirResourceLocation :: Maybe ResponseResourceLocation
  } deriving (Eq, Show)

instance FromJSON CommercialInvoiceResponse where
  parseJSON = withObject "CommercialInvoiceResponse" parse
    where
      parse o = CommercialInvoiceResponse
                <$> o .:? "resource"
                <*> o .:? "resourceLocation"

data CommercialInvoiceResponseResource = CommercialInvoiceResponseResource
  { cirrAdditionalValue  :: Maybe AdditionalValue
  , cirrInsuranceValue   :: Maybe InsuranceValue
  , cirrShippingValue    :: Maybe ShippingValue
  , cirrDocumentLocation :: DocumentLocation
  } deriving (Eq, Show)

instance FromJSON CommercialInvoiceResponseResource where
  parseJSON = withObject "CommercialInvoiceResponseResource" parse
    where
      parse o = CommercialInvoiceResponseResource
                <$> o .:? "additionalValue"
                <*> o .:? "insuranceValue"
                <*> o .:? "shippingValue"
                <*> o .:  "documentLocation"

newtype DocumentLocation = DocumentLocation
  { unDocumentLocation :: Text
  } deriving (Eq, Show, FromJSON)

data OrderShipToResponse = OrderShipToResponse
  { gostResource         :: OrderShipToResponseResource
  , gostResourceLocation :: Maybe ResponseResourceLocation
  } deriving (Eq, Show)

instance FromJSON OrderShipToResponse where
  parseJSON = withObject "OrderShipToResponse" parse
    where
      parse o = OrderShipToResponse
                <$> o .:  "resource"
                <*> o .:? "resourceLocation"

type OrderShipToResponseResource = OrderShipTo

data OrderShipFromResponse = OrderShipFromResponse
  { gosfResource         :: OrderShipFromResponseResource
  , gosfResourceLocation :: Maybe ResponseResourceLocation
  } deriving (Eq, Show)

instance FromJSON OrderShipFromResponse where
  parseJSON = withObject "OrderShipFromResponse" parse
    where
      parse o = OrderShipFromResponse
                <$> o .:  "resource"
                <*> o .:? "resourceLocation"

newtype OrderShipFromResponseResource = OrderShipFromResponseResource
  { osfrrCompany :: Maybe ShipFromCompany
  } deriving (Eq, Show)

instance FromJSON OrderShipFromResponseResource where
  parseJSON = withObject "OrderShipFromResponseResource" parse
    where
      parse o = OrderShipFromResponseResource
                <$> o .:? "company"

data GetOrdersOptions = GetOrdersOptions
  { gooResource         :: GetOrdersOptionsResource
  , gooResourceLocation :: Maybe ResponseResourceLocation
  } deriving (Eq, Show)

instance FromJSON GetOrdersOptions where
  parseJSON = withObject "GetOrdersOptions" parse
    where
      parse o = GetOrdersOptions
                <$> o .:  "resource"
                <*> o .:? "resourceLocation"

data GetOrdersOptionsResource = GetOrdersOptionsResource
  { goorWarehouseId         :: Maybe WarehouseId
  , goorWarehouseExternalId :: Maybe WarehouseExternalId
  , goorWarehouseRegion     :: Maybe WarehouseRegion
  , goorWarehouseArea       :: Maybe WarehouseArea
  , goorServiceLevelCode    :: Maybe ServiceLevelCode
  , goorCarrierCode         :: Maybe CarrierCode
  , goorSameDay             :: SameDay
  , goorForceDuplicate      :: ForceDuplicate
  , goorForceAddress        :: ForceAddress
  , goorChannelName         :: Maybe ChannelName
  , goorReferrer            :: Referrer
  } deriving (Eq, Show)

instance FromJSON GetOrdersOptionsResource where
  parseJSON = withObject "GetOrdersOptionsResource" parse
    where
      parse o = GetOrdersOptionsResource
                <$> o .:? "warehouseId"
                <*> o .:? "warehouseExternalId"
                <*> o .:? "warehouseRegion"
                <*> o .:? "warehouseArea"
                <*> o .:? "serviceLevelCode"
                <*> o .:? "carrierCode"
                <*> o .:  "sameDay"
                <*> o .:  "forceDuplicate"
                <*> o .:  "forceAddress"
                <*> o .:? "channelName"
                <*> o .:  "referrer"

data GetOrdersReturns = GetOrdersReturns
  { gordResource         :: Maybe GetOrdersReturnsResource
  , gordResourceLocation :: ResponseResourceLocation
  } deriving (Eq, Show)

instance FromJSON GetOrdersReturns where
  parseJSON = withObject "GetOrdersReturns" parse
    where
      parse o = GetOrdersReturns
                <$> o .:? "resource"
                <*> o .:  "resourceLocation"

data GetOrdersReturnsResource = GetOrdersReturnsResource
  { gordrItems    :: GetOrdersReturnsResourceItems
  , gordrNext     :: Maybe ResponseNext
  , gordrOffset   :: ResponseOffset
  , gordrPrevious :: Maybe ResponsePrevious
  , gordrTotal    :: ResponseTotal
  } deriving (Eq, Show)

instance FromJSON GetOrdersReturnsResource where
  parseJSON = withObject "GetOrdersReturnsResource" parse
    where
      parse o = GetOrdersReturnsResource
                <$> o .:  "items"
                <*> o .:? "next"
                <*> o .:  "offset"
                <*> o .:? "previous"
                <*> o .:  "total"

newtype GetOrdersReturnsResourceItems = GetOrdersReturnsResourceItems
  { gordri :: Array
  } deriving (Eq, Show, FromJSON)

data GetOrdersTrackings = GetOrdersTrackings
  { gotResource         :: Maybe GetOrdersTrackingsResource
  , gotResourceLocation :: ResponseResourceLocation
  } deriving (Eq, Show)

instance FromJSON GetOrdersTrackings where
  parseJSON = withObject "GetOrdersTrackings" parse
    where
      parse o = GetOrdersTrackings
                <$> o .:? "resource"
                <*> o .:  "resourceLocation"

data GetOrdersTrackingsResource = GetOrdersTrackingsResource
  { gotrItems    :: GetOrdersTrackingsResourceItems
  , gotrNext     :: Maybe ResponseNext
  , gotrOffset   :: ResponseOffset
  , gotrPrevious :: Maybe ResponsePrevious
  , gotrTotal    :: ResponseTotal
  } deriving (Eq, Show)

instance FromJSON GetOrdersTrackingsResource where
  parseJSON = withObject "GetOrdersTrackingsResource" parse
    where
      parse o = GetOrdersTrackingsResource
                <$> o .:  "items"
                <*> o .:? "next"
                <*> o .:  "offset"
                <*> o .:? "previous"
                <*> o .:  "total"

newtype GetOrdersTrackingsResourceItems = GetOrdersTrackingsResourceItems
  { gotriItems :: [GetOrdersTrackingsResourceItem]
  } deriving (Eq, Show, FromJSON)

data GetOrdersTrackingsResourceItem = GetOrdersTrackingsResourceItem
  { gotriResource         :: GetOrdersTrackingsResourceItemResource
  , gotriResourceLocation :: ResponseResourceLocation
  } deriving (Eq, Show)

instance FromJSON GetOrdersTrackingsResourceItem where
  parseJSON = withObject "GetOrdersTrackingsResourceItem" parse
    where
      parse o = GetOrdersTrackingsResourceItem
                <$> o .: "resource"
                <*> o .: "resourceLocation"

data GetOrdersTrackingsResourceItemResource = GetOrdersTrackingsResourceItemResource
  { gotrirId                  :: Id
  , gotrirOrderid             :: OrderId
  , gotrirOrderExternalid     :: Maybe OrderExternalId
  , gotrirTracking            :: Tracking
  , gotrirCarrier             :: CarrierName
  , gotrirUrl                 :: Maybe URL
  , gotrirSummary             :: Maybe Summary
  , gotrirSummaryDate         :: Maybe SummaryDate
  , gotrirLabelCreatedDate    :: Maybe LabelCreatedDate
  , gotrirTrackedDate         :: Maybe TrackedDate
  , gotrirFirstScanDate       :: Maybe FirstScanDate
  , gotrirFirstScanRegion     :: Maybe FirstScanRegion
  , gotrirFirstScanPostalCode :: Maybe FirstScanPostalCode
  , gotrirFirstScanCountry    :: Maybe FirstScanCountry
  , gotrirDeliveredDate       :: Maybe DeliveredDate
  , gotrirDeliveryCity        :: Maybe DeliveryCity
  , gotrirDeliveryRegion      :: Maybe DeliveryRegion
  , gotrirDeliveryPostalCode  :: Maybe DeliveryPostalCode
  , gotrirDeliveryCountry     :: Maybe DeliveryCountry
  } deriving (Eq, Show)

instance FromJSON GetOrdersTrackingsResourceItemResource where
  parseJSON = withObject "GetOrdersHoldsResourceItemResource" parse
    where
      parse o = GetOrdersTrackingsResourceItemResource
                <$> o .:  "id"
                <*> o .:  "orderId"
                <*> o .:? "orderExternalId"
                <*> o .:  "tracking"
                <*> o .:  "carrier"
                <*> o .:? "url"
                <*> o .:? "summary"
                <*> o .:? "summaryDate"
                <*> o .:? "labelCreatedDate"
                <*> o .:? "trackedDate"
                <*> o .:? "firstScanDate"
                <*> o .:? "firstScanRegion"
                <*> o .:? "firstScanPostalCode"
                <*> o .:? "firstScanCountry"
                <*> o .:? "deliveredDate"
                <*> o .:? "deliveryCity"
                <*> o .:? "deliveryRegion"
                <*> o .:? "deliveryPostalCode"
                <*> o .:? "deliveryCountry"

type FirstScanRegion     = Region

type FirstScanPostalCode = PostalCode

type FirstScanCountry    = Country

type DeliveryCity        = City

type DeliveryRegion      = Region

type DeliveryPostalCode  = PostalCode

type DeliveryCountry     = Country

newtype FirstScanDate = FirstScanDate
  { unFirstScanDate :: UTCTime
  } deriving (Eq, Show, FromJSON)

newtype LabelCreatedDate = LabelCreatedDate
  { unLabelCreatedDate :: UTCTime
  } deriving (Eq, Show, FromJSON)

data GetOrdersItems = GetOrdersItems
  { goiResource         :: Maybe GetOrdersItemsResource
  , goiResourceLocation :: ResponseResourceLocation
  } deriving (Eq, Show)

instance FromJSON GetOrdersItems where
  parseJSON = withObject "GetOrdersItems" parse
    where
      parse o = GetOrdersItems
                <$> o .:? "resource"
                <*> o .: "resourceLocation"

data GetOrdersItemsResource = GetOrdersItemsResource
  { goirItems    :: GetOrdersItemsResourceItems
  , goirNext     :: Maybe ResponseNext
  , goirOffset   :: ResponseOffset
  , goirPrevious :: Maybe ResponsePrevious
  , goirTotal    :: ResponseTotal
  } deriving (Eq, Show)

instance FromJSON GetOrdersItemsResource where
  parseJSON = withObject "GetOrdersHoldsResource" parse
    where
      parse o = GetOrdersItemsResource
                <$> o .:  "items"
                <*> o .:? "next"
                <*> o .:  "offset"
                <*> o .:? "previous"
                <*> o .:  "total"

newtype GetOrdersItemsResourceItems = GetOrdersItemsResourceItems
  { goiriItems :: [GetOrdersItemsResourceItem]
  } deriving (Eq, Show, FromJSON)

data GetOrdersItemsResourceItem = GetOrdersItemsResourceItem
  { goiriResource         :: GetOrdersItemsResourceItemResource
  , goiriResourceLocation :: Maybe ResponseResourceLocation
  } deriving (Eq, Show)

instance FromJSON GetOrdersItemsResourceItem where
  parseJSON = withObject "GetOrdersItemsResourceItem" parse
    where
      parse o = GetOrdersItemsResourceItem
                <$> o .:  "resource"
                <*> o .:  "resourceLocation"

data GetOrdersItemsResourceItemResource = GetOrdersItemsResourceItemResource
  { goirirProductId              :: ProductId
  , goirirProductExternalId      :: Maybe ProductExternalId
  , goirirSku                    :: SKU
  , goirirOrderId                :: OrderId
  , goirirOrderExternalId        :: Maybe OrderExternalId
  , goirirQuantity               :: Quantity
  , goirirCommercialInvoiceValue :: Maybe CommercialInvoiceValue
  , goirirSerialNumbers          :: SerialNumbersResponse
  , goirirOrdered                :: Ordered
  , goirirBackordered            :: Maybe Backordered
  , goirirReserved               :: Reserved
  , goirirShipping               :: Shipping
  , goirirShipped                :: Shipped
  } deriving (Eq, Show)

instance FromJSON GetOrdersItemsResourceItemResource where
  parseJSON = withObject "GetOrdersItemsResourceItemResource" parse
    where
      parse o = GetOrdersItemsResourceItemResource
                <$> o .:  "productId"
                <*> o .:? "productExternalId"
                <*> o .:  "sku"
                <*> o .:  "orderId"
                <*> o .:? "orderExternalId"
                <*> o .:  "quantity"
                <*> o .:? "commercialInvoiceValue"
                <*> o .:  "serialNumbers"
                <*> o .:  "ordered"
                <*> o .:? "backordered"
                <*> o .:  "reserved"
                <*> o .:  "shipping"
                <*> o .:  "shipped"

newtype Shipped = Shipped
  { unShipped :: Integer
  } deriving (Eq, Show, FromJSON)

newtype Shipping = Shipping
  { unShipping :: Integer
  } deriving (Eq, Show, FromJSON)

newtype Reserved = Reserved
  { unResered :: Integer
  } deriving (Eq, Show, FromJSON)

newtype Backordered = Backordered
  { unBackordered :: Integer
  } deriving (Eq, Show, FromJSON)

newtype Ordered = Ordered
  { unOrdered :: Integer
  } deriving (Eq, Show, FromJSON)

data SerialNumbersResponse = SerialNumbersResponse
  { snrResource         :: SerialNumbersResponseResource
  , snrResourceLocation :: ResponseResourceLocation
  } deriving (Eq, Show)

instance FromJSON SerialNumbersResponse where
  parseJSON = withObject "SerialNumbersResponse" parse
    where
      parse o = SerialNumbersResponse
                <$> o .: "resource"
                <*> o .: "resourceLocation"

data SerialNumbersResponseResource = SerialNumbersResponseResource
  { snrrItems    :: SerialNumbersResponseResourceItems
  , snrrNext     :: Maybe Next
  , snrrOffset   :: ResponseOffset
  , snrrPrevious :: Maybe ResponsePrevious
  , snrrTotal    :: ResponseTotal
  } deriving (Eq, Show)

instance FromJSON SerialNumbersResponseResource where
  parseJSON = withObject "SerialNumbersResponseResource" parse
    where
      parse o = SerialNumbersResponseResource
                <$> o .:  "items"
                <*> o .:? "next"
                <*> o .:  "offset"
                <*> o .:? "previous"
                <*> o .:  "total"

newtype SerialNumbersResponseResourceItems = SerialNumbersResponseResourceItems
  { snrriItems :: [SerialNumbersResponseResourceItem]
  } deriving (Eq, Show, FromJSON)

data SerialNumbersResponseResourceItem = SerialNumbersResponseResourceItem
  { snrriResource         :: SerialNumbersResponseResourceItemResource
  , snrriResourceLocation :: Maybe ResponseResourceLocation
  } deriving (Eq, Show)

instance FromJSON SerialNumbersResponseResourceItem where
  parseJSON = withObject "SerialNumbersResponseResourceItem" parse
    where
      parse o = SerialNumbersResponseResourceItem
                <$> o .:  "resource"
                <*> o .:? "resourceLocation"

data SerialNumbersResponseResourceItemResource = SerialNumbersResponseResourceItemResource
  { snrrirOrderId      :: OrderId
  , snrrirProductId    :: ProductId
  , snrrirSerialNumber :: Maybe SerialNumber
  } deriving (Eq, Show)

instance FromJSON SerialNumbersResponseResourceItemResource where
  parseJSON = withObject "SerialNumbersResponseResourceItemResource" parse
    where
      parse o = SerialNumbersResponseResourceItemResource
                <$> o .:  "orderId"
                <*> o .:  "productId"
                <*> o .:? "serialNumber"

newtype SerialNumber = SerialNumber
  { unSerialNumber :: Text
  } deriving (Eq, Show, FromJSON)

data GetOrdersHolds = GetOrdersHolds
  { gohResource         :: Maybe GetOrdersHoldsResource
  , gohResourceLocation :: ResponseResourceLocation
  } deriving (Eq, Show)

instance FromJSON GetOrdersHolds where
  parseJSON = withObject "GetOrdersHolds" parse
    where
      parse o = GetOrdersHolds
                <$> o .:? "resource"
                <*> o .:  "resourceLocation"

data GetOrdersHoldsResource = GetOrdersHoldsResource
  { gohrItems    :: GetOrdersHoldsResourceItems
  , gohrNext     :: Maybe ResponseNext
  , gohrOffset   :: ResponseOffset
  , gohrPrevious :: Maybe ResponsePrevious
  , gohrTotal    :: ResponseTotal
  } deriving (Eq, Show)

instance FromJSON GetOrdersHoldsResource where
  parseJSON = withObject "GetOrdersHoldsResource" parse
    where
      parse o = GetOrdersHoldsResource
                <$> o .:  "items"
                <*> o .:? "next"
                <*> o .:  "offset"
                <*> o .:? "previous"
                <*> o .:  "total"

newtype GetOrdersHoldsResourceItems = GetOrdersHoldsResourceItems
  { gohriItems :: [GetOrdersHoldsResourceItem]
  } deriving (Eq, Show, FromJSON)

data GetOrdersHoldsResourceItem = GetOrdersHoldsResourceItem
  { gohriResource         :: GetOrdersHoldsResourceItemResource
  , gohriResourceLocation :: Maybe ResponseResourceLocation
  } deriving (Eq, Show)

instance FromJSON GetOrdersHoldsResourceItem where
  parseJSON = withObject "GetOrdersHoldsResourceItem" parse
    where
      parse o = GetOrdersHoldsResourceItem
                <$> o .:  "resource"
                <*> o .:? "resourceLocation"

data GetOrdersHoldsResourceItemResource = GetOrdersHoldsResourceItemResource
  { gohrirId              :: Id
  , gohrirOrderId         :: OrderId
  , gohrirExternalOrderId :: Maybe ExternalOrderId
  , gohrirType            :: Maybe Type
  , gohrirDescription     :: Description
  , gohrirAppliedDate     :: AppliedDate
  , gohrirClearedDate     :: Maybe ClearedDate
  } deriving (Eq, Show)

instance FromJSON GetOrdersHoldsResourceItemResource where
  parseJSON = withObject "GetOrdersHoldsResourceItemResource" parse
    where
      parse o = GetOrdersHoldsResourceItemResource
                <$> o .:  "id"
                <*> o .:  "orderId"
                <*> o .:? "externalOrderId"
                <*> o .:? "type"
                <*> o .:  "description"
                <*> o .:  "appliedDate"
                <*> o .:? "clearedDate"

newtype NeedsReview = NeedsReview
  { unNeedsReview :: Integer
  } deriving (Eq, Show, FromJSON)

type EventsResponse = ItemResourceEvents

data CreateOrder = CreateOrder
  { coExternalId        :: Maybe ExternalId
  , coOrderNo           :: Maybe OrderNo
  , coProcessAfterDate  :: Maybe ProcessAfterDate
  , coCommerceName      :: Maybe CommerceName
  , coOptions           :: Maybe CreateOrderOptions
  , coShipFrom          :: Maybe OrderShipFrom
  , coShipTo            :: OrderShipTo
  , coCommercialInvoice :: Maybe CommercialInvoice
  , coPackingList       :: Maybe PackingList
  , coOrderItems        :: OrderItems
  } deriving (Eq, Show)

instance ToJSON CreateOrder where
  toJSON CreateOrder {..} = omitNulls ["externalId"        .= coExternalId
                                      ,"orderNo"           .= coOrderNo
                                      ,"processAfterDate"  .= coProcessAfterDate
                                      ,"commerceName"      .= coCommerceName
                                      ,"options"           .= coOptions
                                      ,"shipFrom"          .= coShipFrom
                                      ,"shipTo"            .= coShipTo
                                      ,"commercialInvoice" .= coCommercialInvoice
                                      ,"packingList"       .= coPackingList
                                      ,"items"             .= coOrderItems]

data OrderItem = OrderItem
  { oiCommercialInvoiceValue         :: Maybe CommercialInvoiceValue
  , oiCommercialInvoiceValueCurrency :: Maybe CommercialInvoiceValueCurrency
  , oiQuantity                       :: Quantity
  , oiSku                            :: SKU
  } deriving (Eq, Show)

instance ToJSON OrderItem where
  toJSON OrderItem {..} = omitNulls ["commercialInvoiceValue"         .= oiCommercialInvoiceValue
                                    ,"commercialInvoiceValueCurrency" .= oiCommercialInvoiceValueCurrency
                                    ,"quantity"                       .= oiQuantity
                                    ,"sku"                            .= oiSku]

newtype CommercialInvoiceValueCurrency = CommercialInvoiceValueCurrency
  { unCommercialInvoiceValueCurrency :: Text
  } deriving (Eq, Show, ToJSON)

newtype CommercialInvoiceValue = CommercialInvoiceValue
  { unCommercialInvoiceValue :: Centi
  } deriving (Eq, Show, ToJSON, FromJSON)

newtype OrderItems = OrderItems
  { unOrderItems :: [OrderItem]
  } deriving (Eq, Show, ToJSON)

data PackingList = PackingList
  { plMessage1 :: Maybe PackingListMessage
  , plMessage2 :: Maybe PackingListMessage
  , plMessage3 :: Maybe PackingListMessage
  , plOther    :: Maybe PackingListOther
  } deriving (Eq, Show)

instance ToJSON PackingList where
  toJSON PackingList {..} = omitNulls ["message1" .= plMessage1
                                      ,"message2" .= plMessage2
                                      ,"message3" .= plMessage3
                                      ,"other"    .= plOther]

data PackingListOther = PackingListOther
  { ploBody     :: Maybe MessageBody
  , ploHeader   :: Maybe MessageHeader
  , ploDocument :: Maybe MessageDocument
  , ploLocation :: Maybe MessageLocation
  } deriving (Eq, Show)

instance ToJSON PackingListOther where
  toJSON PackingListOther {..} = omitNulls ["body"     .= ploBody
                                           ,"header"   .= ploHeader
                                           ,"document" .= ploDocument
                                           ,"location" .= ploLocation]

instance FromJSON PackingListOther where
  parseJSON = withObject "PackingListOther" parse
    where
      parse o = PackingListOther
                <$> o .:? "body"
                <*> o .:? "header"
                <*> o .:? "document"
                <*> o .:? "location"

data PackingListMessage = PackingListMessage
  { plmBody     :: Maybe MessageBody
  , plmHeader   :: Maybe MessageHeader
  , plmDocument :: Maybe MessageDocument
  , plmLocation :: Maybe MessageLocation
  } deriving (Eq, Show)

instance ToJSON PackingListMessage where
  toJSON PackingListMessage {..} = omitNulls ["body"     .= plmBody
                                             ,"header"   .= plmHeader
                                             ,"document" .= plmDocument
                                             ,"location" .= plmLocation]

instance FromJSON PackingListMessage where
  parseJSON = withObject "PackingListMessage" parse
    where
      parse o = PackingListMessage
                <$> o .:? "body"
                <*> o .:? "header"
                <*> o .:? "document"
                <*> o .:? "location"

newtype MessageLocation = MessageLocation
  { unMessageLocation :: Text
  } deriving (Eq, Show, ToJSON, FromJSON)

newtype MessageDocument = MessageDocument
  { unMessageDocument :: Text
  } deriving (Eq, Show, ToJSON, FromJSON)

newtype MessageHeader = MessageHeader
  { unMessageHeader :: Text
  } deriving (Eq, Show, ToJSON, FromJSON)

newtype MessageBody = MessageBody
  { unMessageBody :: Text
  } deriving (Eq, Show, ToJSON, FromJSON)

data CommercialInvoice = CommercialInvoice
  { ciAdditionalValue         :: AdditionalValue
  , ciAdditionalValueCurrency :: AdditionalValueCurrency
  , ciInsuranceValue          :: InsuranceValue
  , ciInsuranceValueCurrency  :: InsuranceValueCurrency
  , ciShippingValue           :: ShippingValue
  , ciShippingValueCurrency   :: ShippingValueCurrency
  } deriving (Eq, Show)

instance ToJSON CommercialInvoice where
  toJSON CommercialInvoice {..} = object ["additionalValue"         .= ciAdditionalValue
                                         ,"additionalValueCurrency" .= ciAdditionalValueCurrency
                                         ,"insuranceValue"          .= ciInsuranceValue
                                         ,"insuranceValueCurrency"  .= ciInsuranceValueCurrency
                                         ,"shippingValue"           .= ciShippingValue
                                         ,"shippingValueCurrency"   .= ciShippingValueCurrency]

newtype ShippingValueCurrency = ShippingValueCurrency
  { unShippingValueCurrency :: Text
  } deriving (Eq, Show, ToJSON)

newtype ShippingValue = ShippingValue
  { unShippingValue :: Centi
  } deriving (Eq, Show, ToJSON, FromJSON)

newtype InsuranceValueCurrency = InsuranceValueCurrency
  { unInsuranceValueCurrency :: Text
  } deriving (Eq, Show, ToJSON)

newtype InsuranceValue = InsuranceValue
  { unInsuranceValue :: Centi
  } deriving (Eq, Show, ToJSON, FromJSON)

newtype AdditionalValueCurrency = AdditionalValueCurrency
  { unAdditionalValueCurrency :: Text
  } deriving (Eq, Show, ToJSON)

newtype AdditionalValue = AdditionalValue
  { unAdditionalValue :: Centi
  } deriving (Eq, Show, ToJSON, FromJSON)

data OrderShipTo = OrderShipTo
  { ostEmail        :: Maybe Email
  , ostName         :: Name
  , ostCompany      :: Maybe Company
  , ostAddress1     :: AddressLine
  , ostAddress2     :: Maybe AddressLine
  , ostAddress3     :: Maybe AddressLine
  , ostCity         :: City
  , ostState        :: State
  , ostPostalCode   :: Maybe PostalCode
  , ostCountry      :: Country
  , ostPhone        :: Phone
  , ostIsCommercial :: IsCommercial
  , ostIsPoBox      :: Maybe IsPoBox
  } deriving (Eq, Show)

instance ToJSON OrderShipTo where
  toJSON OrderShipTo {..} = omitNulls ["email"        .= ostEmail
                                      ,"name"         .= ostName
                                      ,"company"      .= ostCompany
                                      ,"address1"     .= ostAddress1
                                      ,"address2"     .= ostAddress2
                                      ,"address3"     .= ostAddress3
                                      ,"city"         .= ostCity
                                      ,"state"        .= ostState
                                      ,"postalCode"   .= ostPostalCode
                                      ,"country"      .= ostCountry
                                      ,"phone"        .= ostPhone
                                      ,"isCommercial" .= ostIsCommercial
                                      ,"isPoBox"      .= ostIsPoBox]

instance FromJSON OrderShipTo where
  parseJSON = withObject "OrderShpTo" parse
    where
      parse o = OrderShipTo
                <$> o .:? "email"
                <*> o .:  "name"
                <*> o .:? "company"
                <*> o .:  "address1"
                <*> o .:? "address2"
                <*> o .:? "address3"
                <*> o .:  "city"
                <*> o .:  "state"
                <*> o .:? "postalCode"
                <*> o .:  "country"
                <*> o .:  "phone"
                <*> o .:  "isCommercial"
                <*> o .:? "isPoBox"

newtype OrderShipFrom = OrderShipFrom
  { osfCompany :: Maybe ShipFromCompany
  } deriving (Eq, Show, ToJSON, FromJSON)

newtype ShipFromCompany = ShipFromCompany
  { unShipFromCompany :: Text
  } deriving (Eq, Show, FromJSON)

instance ToJSON ShipFromCompany where
  toJSON (ShipFromCompany x) = object ["company" .= x]

newtype Company = Company
  { unCompany :: Text
  } deriving (Eq, Show, ToJSON, FromJSON)

-- | Specify one of WarehouseId, WarehouseExternalId, WarehouseRegion, WarehouseArea
data CreateOrderOptions = CreateOrderOptions
  { cooWarehouseId         :: Maybe WarehouseId
  , cooWarehouseExternalId :: Maybe WarehouseExternalId
  , cooWarehouseRegion     :: Maybe WarehouseRegion
  , cooWarehouseArea       :: Maybe WarehouseArea
  , cooServiceLevelCode    :: ServiceLevelCode
  , cooCarrierCode         :: Maybe CarrierCode
  , cooSameDay             :: SameDay
  , cooForceDuplicate      :: Maybe ForceDuplicate
  , cooForceAddress        :: Maybe ForceAddress
  , cooChannelName         :: Maybe ChannelName
  , cooReferrer            :: Maybe Referrer
  , cooAffiliate           :: Maybe Affiliate
  , cooCurrency            :: Currency
  , cooCanSplit            :: Maybe CanSplit
  , cooNote                :: Maybe Note
  , cooDiscountCode        :: Maybe DiscountCode
  , cooHold                :: Maybe Hold
  , cooHoldReason          :: Maybe HoldReason
  , cooServer              :: Maybe Server
  } deriving (Eq, Show)

instance ToJSON CreateOrderOptions where
  toJSON CreateOrderOptions {..} = omitNulls ["warehouseId"         .= cooWarehouseId
                                             ,"warehouseExternalId" .= cooWarehouseExternalId
                                             ,"warehouseRegion"     .= cooWarehouseRegion
                                             ,"warehouseArea"       .= cooWarehouseArea
                                             ,"serviceLevelCode"    .= cooServiceLevelCode
                                             ,"carrierCode"         .= cooCarrierCode
                                             ,"sameDay"             .= cooSameDay
                                             ,"forceDuplicate"      .= cooForceDuplicate
                                             ,"forceAddress"        .= cooForceAddress
                                             ,"channelName"         .= cooChannelName
                                             ,"referrer"            .= cooReferrer
                                             ,"affiliate"           .= cooAffiliate
                                             ,"currency"            .= cooCurrency
                                             ,"canSplit"            .= cooCanSplit
                                             ,"note"                .= cooNote
                                             ,"discountCode"        .= cooDiscountCode
                                             ,"hold"                .= cooHold
                                             ,"holdReason"          .= cooHoldReason
                                             ,"server"              .= cooServer]

newtype Server = Server
  { unServer :: Text
  } deriving (Eq, Show, ToJSON)

newtype HoldReason = HoldReason
  { unHoldReason :: Text
  } deriving (Eq, Show, ToJSON)

data Hold
  = Hold
  | DontHold
  deriving (Eq, Show)

instance ToJSON Hold where
  toJSON Hold     = Number 1
  toJSON DontHold = Number 0

newtype DiscountCode = DiscountCode
  { unDiscountCode :: Text
  } deriving (Eq, Show, ToJSON)

newtype Affiliate = Affiliate
  { unAffiliate :: Text
  } deriving (Eq, Show, ToJSON)

newtype Referrer = Referrer
  { unReferrer :: Text
  } deriving (Eq, Show, ToJSON, FromJSON)

data ForceAddress
  = ForceAddress
  | DontForceAddress
  deriving (Eq, Show)

instance ToJSON ForceAddress where
  toJSON ForceAddress     = Number 0
  toJSON DontForceAddress = Number 1

instance FromJSON ForceAddress where
  parseJSON = withScientific "ForceAddress" parse
    where
      parse 0 = pure ForceAddress
      parse 1 = pure DontForceAddress
      parse o = fail $ "Unexpected ForceAddress: " <> show o

data ForceDuplicate
  = ForceDuplicate
  | DontForceDuplicate
  deriving (Eq, Show)

instance ToJSON ForceDuplicate where
  toJSON ForceDuplicate     = Number 0
  toJSON DontForceDuplicate = Number 1

instance FromJSON ForceDuplicate where
  parseJSON = withScientific "ForceDuplicate" parse
    where
      parse 0 = pure ForceDuplicate
      parse 1 = pure DontForceDuplicate
      parse o = fail $ "Unexpected ForceDuplicate: " <> show o

data SameDay
  = SameDay
  | NotSameDay
  deriving (Eq, Show)

instance ToJSON SameDay where
  toJSON SameDay    = Number 1
  toJSON NotSameDay = Number 0

instance FromJSON SameDay where
  parseJSON (Number 1) = pure SameDay
  parseJSON (Number 0) = pure NotSameDay
  parseJSON (String "NOT REQUESTED") = pure NotSameDay
  parseJSON o = fail $ "Unexpected SameDay: " <> show o

newtype ProcessAfterDate = ProcessAfterDate
  { unProcessAfterDate :: UTCTime
  } deriving (Eq, Show, FromJSON)

instance ToJSON ProcessAfterDate where
  toJSON (ProcessAfterDate x) = object ["processAfterDate" .= utcToShipwire x]

------------------------------------------------------------------------------------------
-- Address Validation Endpoint https://www.shipwire.com/w/developers/address-validation --
------------------------------------------------------------------------------------------

-- | POST /api/v3.1/addressValidation
data ValidateAddressRequest
type instance ShipwireReturn ValidateAddressRequest = ValidateAddressResponse

type AddressToValidate = OrderShipTo

data ValidateAddressResponse = ValidateAddressResponse
  { varStatus           :: ResponseStatus
  , varMessage          :: ResponseMessage
  , varResource         :: ValidateAddressResponseResource
  , varResourceLocation :: Maybe ResponseResourceLocation
  , varWarnings         :: Maybe ValidateAddressWarnings
  , varErrors           :: Maybe Object
  } deriving (Eq, Show)

instance FromJSON ValidateAddressResponse where
  parseJSON = withObject "ValidateAddressResponse" parse
    where
      parse o = ValidateAddressResponse
                <$> o .:  "status"
                <*> o .:  "message"
                <*> o .:  "resource"
                <*> o .:? "resourceLocation"
                <*> o .:? "warnings"
                <*> o .:? "errors"

type ValidateAddressResponseResource = OrderShipTo

newtype ValidateAddressErrors = ValidateAddressErrors
  { vaeErrorObject :: ErrorObject
  } deriving (Eq, Show)

instance FromJSON ValidateAddressErrors where
  parseJSON = withObject "ValidateAddressErrors" parse
    where
      parse o = ValidateAddressErrors
                <$> o .: "0"

data ErrorObject = ErrorObject
  { eoEmail        :: Maybe InnerErrorObject
  , eoName         :: Maybe InnerErrorObject
  , eoCompany      :: Maybe InnerErrorObject
  , eoAddress1     :: Maybe InnerErrorObject
  , eoAddress2     :: Maybe InnerErrorObject
  , eoAddress3     :: Maybe InnerErrorObject
  , eoCity         :: Maybe InnerErrorObject
  , eoState        :: Maybe InnerErrorObject
  , eoPostalCode   :: Maybe InnerErrorObject
  , eoCountry      :: Maybe InnerErrorObject
  , eoPhone        :: Maybe InnerErrorObject
  , eoIsCommercial :: Maybe InnerErrorObject
  , eoIsPoBox      :: Maybe InnerErrorObject
  } deriving (Eq, Show)

instance FromJSON ErrorObject where
  parseJSON = withObject "ErrorObject" parse
    where
      parse o = ErrorObject
                <$> o .:? "email"
                <*> o .:? "name"
                <*> o .:? "company"
                <*> o .:? "address1"
                <*> o .:? "address2"
                <*> o .:? "address3"
                <*> o .:? "city"
                <*> o .:? "state"
                <*> o .:? "postalCode"
                <*> o .:? "country"
                <*> o .:? "phone"
                <*> o .:? "isCommercial"
                <*> o .:? "isPoBox"

newtype InnerErrorObject = InnerErrorObject
  { ieoRules :: InnerErrorObjectRules
  } deriving (Eq, Show, FromJSON)

newtype InnerErrorObjectRules = InnerErrorObjectRules
  { ieorObject :: Object
  } deriving (Eq, Show, FromJSON)

newtype ValidateAddressWarnings = ValidateAddressWarnings
  { vawWarningObject :: WarningObject
  } deriving (Eq, Show)

instance FromJSON ValidateAddressWarnings where
  parseJSON = withObject "ValidateAddressWarnings" parse
    where
      parse o = ValidateAddressWarnings
                <$> o .: "0"

data WarningObject = WarningObject
  { woEmail        :: Maybe InnerWarningObject
  , woName         :: Maybe InnerWarningObject
  , woCompany      :: Maybe InnerWarningObject
  , woAddress1     :: Maybe InnerWarningObject
  , woAddress2     :: Maybe InnerWarningObject
  , woAddress3     :: Maybe InnerWarningObject
  , woCity         :: Maybe InnerWarningObject
  , woState        :: Maybe InnerWarningObject
  , woPostalCode   :: Maybe InnerWarningObject
  , woCountry      :: Maybe InnerWarningObject
  , woPhone        :: Maybe InnerWarningObject
  , woIsCommercial :: Maybe InnerWarningObject
  , woIsPoBox      :: Maybe InnerWarningObject
  } deriving (Eq, Show)

instance FromJSON WarningObject where
  parseJSON = withObject "WarningObject" parse
    where
      parse o = WarningObject
                <$> o .:? "email"
                <*> o .:? "name"
                <*> o .:? "company"
                <*> o .:? "address1"
                <*> o .:? "address2"
                <*> o .:? "address3"
                <*> o .:? "city"
                <*> o .:? "state"
                <*> o .:? "postalCode"
                <*> o .:? "country"
                <*> o .:? "phone"
                <*> o .:? "isCommercial"
                <*> o .:? "isPoBox"

newtype InnerWarningObject = InnerWarningObject
  { iwoRules :: InnerWarningObjectRules
  } deriving (Eq, Show, FromJSON)

newtype InnerWarningObjectRules = InnerWarningObjectRules
 { iworObject :: Object
 } deriving (Eq, Show, FromJSON)
