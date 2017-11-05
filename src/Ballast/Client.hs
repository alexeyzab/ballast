{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Ballast.Client where

import           Ballast.Types
import           Data.Aeson                 (eitherDecode, encode)
import           Data.Aeson.Types
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy.Char8 as BSL
import           Data.Maybe                 (fromJust)
import           Data.Monoid                ((<>))
import qualified Data.Text                  as T
import           Network.HTTP.Client
import           Network.HTTP.Client.TLS
import qualified Network.HTTP.Types.Method  as NHTM

-- | Conversion of a key value pair to a query parameterized string
paramsToByteString ::
    [Query]
    -> BS8.ByteString
paramsToByteString []           = mempty
paramsToByteString (x : []) = (fst $ unQuery x) <> "=" <> (snd $ unQuery x)
paramsToByteString (x : xs) =
    mconcat [ (fst $ unQuery x), "=", (snd $ unQuery x), "&" ] <> paramsToByteString xs

-- | Generate a real-time shipping quote
-- https://www.shipwire.com/w/developers/rate/
createRateRequest :: GetRate -> ShipwireRequest RateRequest TupleBS8 BSL.ByteString
createRateRequest getRate = mkShipwireRequest NHTM.methodPost url params
  where
    url = "/rate"
    params = Params (Just $ Body (encode getRate)) []

-- | Get stock information for your products.
-- https://www.shipwire.com/w/developers/stock/
getStockInfo :: ShipwireRequest StockRequest TupleBS8 BSL.ByteString
getStockInfo = mkShipwireRequest NHTM.methodGet url params
  where
    url = "/stock"
    params = Params Nothing []

-- | Get an itemized list of receivings.
-- https://www.shipwire.com/w/developers/receiving/#panel-shipwire0
getReceivings :: ShipwireRequest GetReceivingsRequest TupleBS8 BSL.ByteString
getReceivings = mkShipwireRequest NHTM.methodGet url params
  where
    url = "/receivings"
    params = Params Nothing []

-- | Create a new receiving
-- https://www.shipwire.com/w/developers/receiving/#panel-shipwire1
createReceiving :: CreateReceiving -> ShipwireRequest CreateReceivingRequest TupleBS8 BSL.ByteString
createReceiving crReceiving = mkShipwireRequest NHTM.methodPost url params
  where
    url = "/receivings"
    params = Params (Just $ Body (encode crReceiving)) []

-- | Get information about this receiving.
-- https://www.shipwire.com/w/developers/receiving/#panel-shipwire2
getReceiving :: ReceivingId -> ShipwireRequest GetReceivingRequest TupleBS8 BSL.ByteString
getReceiving receivingId = request
  where
    request = mkShipwireRequest NHTM.methodGet url params
    url = T.append "/receivings/" $ getReceivingId receivingId
    params = Params Nothing []

-- | Modify information about this receiving.
-- https://www.shipwire.com/w/developers/receiving/#panel-shipwire3
modifyReceiving :: ReceivingId -> ModifyReceiving -> ShipwireRequest ModifyReceivingRequest TupleBS8 BSL.ByteString
modifyReceiving receivingId modReceiving = request
  where
    request = mkShipwireRequest NHTM.methodPut url params
    url = T.append "/receivings/" $ getReceivingId receivingId
    params = Params (Just $ Body (encode modReceiving)) []

-- | Cancel this receiving.
-- https://www.shipwire.com/w/developers/receiving/#panel-shipwire4
cancelReceiving :: ReceivingId -> ShipwireRequest CancelReceivingRequest TupleBS8 BSL.ByteString
cancelReceiving receivingId = request
  where
    request = mkShipwireRequest NHTM.methodPost url params
    url = T.concat ["/receivings/", getReceivingId receivingId, "/cancel"]
    params = Params Nothing []

-- | Cancel shipping labels on this receiving.
-- https://www.shipwire.com/w/developers/receiving/#panel-shipwire5
cancelReceivingLabels :: ReceivingId -> ShipwireRequest CancelReceivingLabelsRequest TupleBS8 BSL.ByteString
cancelReceivingLabels receivingId = request
  where
    request = mkShipwireRequest NHTM.methodPost url params
    url = T.concat ["/receivings/", getReceivingId receivingId, "/labels/cancel"]
    params = Params Nothing []

-- | Get the list of holds, if any, on this receiving.
-- https://www.shipwire.com/w/developers/receiving/#panel-shipwire6
getReceivingHolds :: ReceivingId -> ShipwireRequest GetReceivingHoldsRequest TupleBS8 BSL.ByteString
getReceivingHolds receivingId = request
  where
    request = mkShipwireRequest NHTM.methodGet url params
    url = T.concat ["/receivings/", getReceivingId receivingId, "/holds"]
    params = Params Nothing []

-- | Get email recipients and instructions for this receiving.
-- https://www.shipwire.com/w/developers/receiving/#panel-shipwire7
getReceivingInstructionsRecipients :: ReceivingId -> ShipwireRequest GetReceivingInstructionsRecipientsRequest TupleBS8 BSL.ByteString
getReceivingInstructionsRecipients receivingId = request
  where
    request = mkShipwireRequest NHTM.methodGet url params
    url = T.concat ["/receivings/", getReceivingId receivingId, "/instructionsRecipients"]
    params = Params Nothing []

-- | Get the contents of this receiving.
-- https://www.shipwire.com/w/developers/receiving/#panel-shipwire8
getReceivingItems :: ReceivingId -> ShipwireRequest GetReceivingItemsRequest TupleBS8 BSL.ByteString
getReceivingItems receivingId = request
  where
    request = mkShipwireRequest NHTM.methodGet url params
    url = T.concat ["/receivings/", getReceivingId receivingId, "/items"]
    params = Params Nothing []

-- | Get shipping dimension and container information.
-- https://www.shipwire.com/w/developers/receiving/#panel-shipwire9
getReceivingShipments :: ReceivingId -> ShipwireRequest GetReceivingShipmentsRequest TupleBS8 BSL.ByteString
getReceivingShipments receivingId = request
  where
    request = mkShipwireRequest NHTM.methodGet url params
    url = T.concat ["/receivings/", getReceivingId receivingId, "/shipments"]
    params = Params Nothing []

-- | Get tracking information for this receiving.
-- https://www.shipwire.com/w/developers/receiving/#panel-shipwire10
getReceivingTrackings :: ReceivingId -> ShipwireRequest GetReceivingTrackingsRequest TupleBS8 BSL.ByteString
getReceivingTrackings receivingId = request
  where
    request = mkShipwireRequest NHTM.methodGet url params
    url = T.concat ["/receivings/", getReceivingId receivingId, "/trackings"]
    params = Params Nothing []

-- | Get labels information for this receiving.
-- https://www.shipwire.com/w/developers/receiving/#panel-shipwire11
getReceivingLabels :: ReceivingId -> ShipwireRequest GetReceivingLabelsRequest TupleBS8 BSL.ByteString
getReceivingLabels receivingId = request
  where
    request = mkShipwireRequest NHTM.methodGet url params
    url = T.concat ["/receivings/", getReceivingId receivingId, "/labels"]
    params = Params Nothing []

-- | Get an itemized list of products.
-- https://www.shipwire.com/w/developers/product/#panel-shipwire0
getProducts :: ShipwireRequest GetProductsRequest TupleBS8 BSL.ByteString
getProducts = request
  where
    request = mkShipwireRequest NHTM.methodGet url params
    url = "/products"
    params = Params Nothing []

-- | Create new products of any classification.
-- https://www.shipwire.com/w/developers/product/#panel-shipwire1
createProduct :: [CreateProductsWrapper] -> ShipwireRequest CreateProductsRequest TupleBS8 BSL.ByteString
createProduct cpr = request
  where
    request = mkShipwireRequest NHTM.methodPost url params
    url = "/products"
    params = Params (Just $ Body (encode cpr)) []

-- | Modify products of any classification.
-- https://www.shipwire.com/w/developers/product/#panel-shipwire2
modifyProducts :: [CreateProductsWrapper] -> ShipwireRequest ModifyProductsRequest TupleBS8 BSL.ByteString
modifyProducts mpr = request
  where
    request = mkShipwireRequest NHTM.methodPut url params
    url = "/products"
    params = Params (Just $ Body (encode mpr)) []

-- | Modify a product.
-- https://www.shipwire.com/w/developers/product/#panel-shipwire3
modifyProduct :: CreateProductsWrapper -> Id -> ShipwireRequest ModifyProductRequest TupleBS8 BSL.ByteString
modifyProduct mpr productId = request
  where
    request = mkShipwireRequest NHTM.methodPut url params
    url = T.append "/products/" $ T.pack . show $ unId productId
    params = Params (Just $ Body (encode mpr)) []

-- | Get information about a product.
-- https://www.shipwire.com/w/developers/product/#panel-shipwire4
getProduct :: Id -> ShipwireRequest GetProductRequest TupleBS8 BSL.ByteString
getProduct productId = request
  where
    request = mkShipwireRequest NHTM.methodGet url params
    url = T.append "/products/" $ T.pack . show $ unId productId
    params = Params Nothing []

-- | Indicates that the listed products will not longer be used.
-- https://www.shipwire.com/w/developers/product/#panel-shipwire5
retireProducts :: ProductsToRetire -> ShipwireRequest RetireProductsRequest TupleBS8 BSL.ByteString
retireProducts ptr = request
  where
    request = mkShipwireRequest NHTM.methodPost url params
    url = "/products/retire"
    params = Params (Just $ Body (encode ptr)) []

-- | Get an itemized list of orders.
-- https://www.shipwire.com/w/developers/order/#panel-shipwire0
getOrders :: ShipwireRequest GetOrdersRequest TupleBS8 BSL.ByteString
getOrders = request
  where
    request = mkShipwireRequest NHTM.methodGet url params
    url = "/orders"
    params = Params Nothing []

-- | Get information about this order.
-- https://www.shipwire.com/w/developers/order/#panel-shipwire1
getOrder :: IdWrapper -> ShipwireRequest GetOrderRequest TupleBS8 BSL.ByteString
getOrder idw = request
  where
    request = mkShipwireRequest NHTM.methodGet url params
    url = case idw of
      (WrappedId x) -> T.concat ["/orders/", T.pack . show $ unId x]
      (WrappedExternalId x) -> T.concat ["/orders/E", unExternalId x]
    params = Params Nothing []

-- | Create a new order.
-- https://www.shipwire.com/w/developers/order/#panel-shipwire2
createOrder :: CreateOrder -> ShipwireRequest CreateOrderRequest TupleBS8 BSL.ByteString
createOrder co = request
  where
    request = mkShipwireRequest NHTM.methodPost url params
    url = "/orders"
    params = Params (Just $ Body (encode co)) []

-- | Cancel this order.
-- https://www.shipwire.com/w/developers/order/#panel-shipwire4
cancelOrder :: IdWrapper -> ShipwireRequest CancelOrderRequest TupleBS8 BSL.ByteString
cancelOrder idw = request
  where
    request = mkShipwireRequest NHTM.methodPost url params
    url = case idw of
      (WrappedId x) -> T.concat ["/orders/", T.pack . show $ unId x, "/cancel"]
      (WrappedExternalId x) -> T.concat ["/orders/E", unExternalId x, "/cancel"]
    params = Params Nothing []

-- | Get tracking information for this order.
-- https://www.shipwire.com/w/developers/order/#panel-shipwire7
getOrderTrackings :: IdWrapper -> ShipwireRequest GetOrderTrackingsRequest TupleBS8 BSL.ByteString
getOrderTrackings idwr = request
  where
    request = mkShipwireRequest NHTM.methodGet url params
    url = case idwr of
      (WrappedId x) -> T.concat ["/orders/", T.pack . show $ unId x, "/trackings"]
      (WrappedExternalId x) -> T.concat ["/orders/E", unExternalId x, "/trackings"]
    params = Params Nothing []

-- | Validate Address
-- https://www.shipwire.com/w/developers/address-validation
validateAddress :: AddressToValidate -> ShipwireRequest ValidateAddressRequest TupleBS8 BSL.ByteString
validateAddress atv = request
  where
    request = mkShipwireRequest NHTM.methodPost url params
    url = ".1/addressValidation"
    params = Params (Just $ Body (encode atv)) []

shipwire' :: ShipwireConfig
          -> ShipwireRequest a TupleBS8 BSL.ByteString
          -> IO (Response BSL.ByteString)
shipwire' ShipwireConfig {..} ShipwireRequest {..} = do
  manager <- newManager tlsManagerSettings
  initReq <- parseRequest $ T.unpack $ T.append (hostUri host) endpoint
  let reqBody | rMethod == NHTM.methodGet = mempty
              | paramsBody params == Nothing = mempty
              | otherwise = unBody $ fromJust $ paramsBody params
      req = initReq { method = rMethod
                    , requestBody = RequestBodyLBS reqBody
                    , queryString = paramsToByteString $ paramsQuery params
                    }
      shipwireUser = unUsername email
      shipwirePass = unPassword pass
      authorizedRequest = applyBasicAuth shipwireUser shipwirePass req
  httpLbs authorizedRequest manager

data ShipwireError =
  ShipwireError {
    parseError       :: String
  , shipwireResponse :: Response BSL.ByteString
  } deriving (Eq, Show)

-- | Create a request to `Shipwire`'s API
shipwire
  :: (FromJSON (ShipwireReturn a))
  => ShipwireConfig
  -> ShipwireRequest a TupleBS8 BSL.ByteString
  -> IO (Either ShipwireError (ShipwireReturn a))
shipwire config request = do
  response <- shipwire' config request
  let result = eitherDecode $ responseBody response
  case result of
    Left s -> return (Left (ShipwireError s response))
    Right r -> return (Right r)

-- | This function is only used internally to speed up the test suite.
-- Instead of creating a new Manager we reuse the same one.
shipwireTest ::
     (FromJSON (ShipwireReturn a))
  => ShipwireConfig
  -> Manager
  -> ShipwireRequest a TupleBS8 BSL.ByteString
  -> IO (Either ShipwireError (ShipwireReturn a))
shipwireTest config tlsManager request = do
  response <- shipwireTest' config request tlsManager
  let result = eitherDecode $ responseBody response
  case result of
    Left s -> return (Left (ShipwireError s response))
    Right r -> return (Right r)

shipwireTest' :: ShipwireConfig
              -> ShipwireRequest a TupleBS8 BSL.ByteString
              -> Manager
              -> IO (Response BSL.ByteString)
shipwireTest' ShipwireConfig {..} ShipwireRequest {..} manager = do
  initReq <- parseRequest $ T.unpack $ T.append (hostUri host) endpoint
  let reqBody | rMethod == NHTM.methodGet = mempty
              | paramsBody params == Nothing = mempty
              | otherwise = unBody $ fromJust $ paramsBody params
      req = initReq { method = rMethod
                    , requestBody = RequestBodyLBS reqBody
                    , queryString = paramsToByteString $ paramsQuery params
                    }
      shipwireUser = unUsername email
      shipwirePass = unPassword pass
      authorizedRequest = applyBasicAuth shipwireUser shipwirePass req
  httpLbs authorizedRequest manager
