{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Ballast.Client where

import           Ballast.Types
import           Data.Aeson                 (eitherDecode, encode)
import           Data.Aeson.Types
import qualified Data.ByteString.Lazy.Char8 as BSL
import           Data.Monoid                ((<>))
import           Data.String                (IsString)
import qualified Data.Text                  as T
import           Network.HTTP.Client
import           Network.HTTP.Client.TLS
import qualified Network.HTTP.Types.Method  as NHTM

-- | Conversion of a key value pair to a query parameterized string
paramsToByteString
    :: (Monoid m, IsString m)
    => [(m, m)]
    -> m
paramsToByteString []           = mempty
paramsToByteString ((x,y) : []) = x <> "=" <> y
paramsToByteString ((x,y) : xs) =
    mconcat [ x, "=", y, "&" ] <> paramsToByteString xs

-- | Generate a real-time shipping quote
-- https://www.shipwire.com/w/developers/rate/
createRateRequest :: GetRate -> ShipwireRequest RateRequest TupleBS8 BSL.ByteString
createRateRequest getRate = mkShipwireRequest NHTM.methodPost url params
  where
    url = "/rate"
    params = [Body (encode getRate)]

-- | Get stock information for your products.
-- https://www.shipwire.com/w/developers/stock/
getStockInfo :: ShipwireRequest StockRequest TupleBS8 BSL.ByteString
getStockInfo = mkShipwireRequest NHTM.methodGet url params
  where
    url = "/stock"
    params = []

-- | Get an itemized list of receivings.
-- https://www.shipwire.com/w/developers/receiving/#panel-shipwire0
getReceivings :: ShipwireRequest GetReceivingsRequest TupleBS8 BSL.ByteString
getReceivings = mkShipwireRequest NHTM.methodGet url params
  where
    url = "/receivings"
    params = []

-- | Create a new receiving
-- https://www.shipwire.com/w/developers/receiving/#panel-shipwire1
createReceiving :: CreateReceiving -> ShipwireRequest CreateReceivingRequest TupleBS8 BSL.ByteString
createReceiving crReceiving = mkShipwireRequest NHTM.methodPost url params
  where
    url = "/receivings"
    params = [Body (encode crReceiving)]

-- | Get information about this receiving.
-- https://www.shipwire.com/w/developers/receiving/#panel-shipwire2
getReceiving :: ReceivingId -> ShipwireRequest GetReceivingRequest TupleBS8 BSL.ByteString
getReceiving receivingId = request
  where
    request = mkShipwireRequest NHTM.methodGet url params
    url = T.append "/receivings/" $ getReceivingId receivingId
    params = []

-- | Modify information about this receiving.
-- https://www.shipwire.com/w/developers/receiving/#panel-shipwire3
modifyReceiving :: ReceivingId -> ModifyReceiving -> ShipwireRequest ModifyReceivingRequest TupleBS8 BSL.ByteString
modifyReceiving receivingId modReceiving = request
  where
    request = mkShipwireRequest NHTM.methodPut url params
    url = T.append "/receivings/" $ getReceivingId receivingId
    params = [Body (encode modReceiving)]

-- | Cancel this receiving.
-- https://www.shipwire.com/w/developers/receiving/#panel-shipwire4
cancelReceiving :: ReceivingId -> ShipwireRequest CancelReceivingRequest TupleBS8 BSL.ByteString
cancelReceiving receivingId = request
  where
    request = mkShipwireRequest NHTM.methodPost url params
    url = T.append (T.append "/receivings/" $ getReceivingId receivingId) "/cancel"
    params = []

-- | Cancel shipping labels on this receiving.
-- https://www.shipwire.com/w/developers/receiving/#panel-shipwire5
cancelReceivingLabels :: ReceivingId -> ShipwireRequest CancelReceivingLabelsRequest TupleBS8 BSL.ByteString
cancelReceivingLabels receivingId = request
  where
    request = mkShipwireRequest NHTM.methodPost url params
    url = T.concat ["/receivings/", (getReceivingId receivingId), "/labels/cancel"]
    params = []

-- | Get the list of holds, if any, on this receiving.
-- https://www.shipwire.com/w/developers/receiving/#panel-shipwire6
getReceivingHolds :: ReceivingId -> ShipwireRequest GetReceivingHoldsRequest TupleBS8 BSL.ByteString
getReceivingHolds receivingId = request
  where
    request = mkShipwireRequest NHTM.methodGet url params
    url = T.concat ["/receivings/", (getReceivingId receivingId), "/holds"]
    params = []
    
-- | Get email recipients and instructions for this receiving.
-- https://www.shipwire.com/w/developers/receiving/#panel-shipwire7
getReceivingInstructionsRecipients :: ReceivingId -> ShipwireRequest GetReceivingInstructionsRecipientsRequest TupleBS8 BSL.ByteString
getReceivingInstructionsRecipients receivingId = request
  where
    request = mkShipwireRequest NHTM.methodGet url params
    url = T.concat ["/receivings/", (getReceivingId receivingId), "/instructionsRecipients"]
    params = []

-- | Get the contents of this receiving.
-- https://www.shipwire.com/w/developers/receiving/#panel-shipwire8
getReceivingItems :: ReceivingId -> ShipwireRequest GetReceivingItemsRequest TupleBS8 BSL.ByteString
getReceivingItems receivingId = request
  where
    request = mkShipwireRequest NHTM.methodGet url params
    url = T.concat ["/receivings/", (getReceivingId receivingId), "/items"]
    params = []

-- | Get shipping dimension and container information.
-- https://www.shipwire.com/w/developers/receiving/#panel-shipwire9
getReceivingShipments :: ReceivingId -> ShipwireRequest GetReceivingShipmentsRequest TupleBS8 BSL.ByteString
getReceivingShipments receivingId = request
  where
    request = mkShipwireRequest NHTM.methodGet url params
    url = T.concat ["/receivings/", (getReceivingId receivingId), "/shipments"]
    params = []

-- "{\"status\":401,\"message\":\"Please include a valid Authorization header (Basic)\",\"resourceLocation\":null}"

shipwire' :: (FromJSON (ShipwireReturn a))
          => ShipwireConfig
          -> ShipwireRequest a TupleBS8 BSL.ByteString
          -> IO (Response BSL.ByteString)
shipwire' ShipwireConfig {..} ShipwireRequest {..} = do
  manager <- newManager tlsManagerSettings
  initReq <- parseRequest $ T.unpack $ T.append (hostUri host) endpoint
  let reqBody | rMethod == NHTM.methodGet = mempty
              | otherwise = filterBody params
      reqURL  = paramsToByteString $ filterQuery params
      req = initReq { method = rMethod
                    , requestBody = RequestBodyLBS reqBody
                    , queryString = reqURL
                    }
      shipwireUser = unUsername email
      shipwirePass = unPassword pass
      authorizedRequest = applyBasicAuth shipwireUser shipwirePass req
  httpLbs authorizedRequest manager

data ShipwireError =
  ShipwireError {
    parseError :: String
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
    (Right r) -> return (Right r)
