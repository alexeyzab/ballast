{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Ballast.Client where

import           Ballast.Types
import           Data.Aeson                 (eitherDecode, encode)
import           Data.Aeson.Types
import qualified Data.ByteString.Char8      as BS8
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

-- | Create a request to `Shipwire`'s API
shipwire
  :: (FromJSON (ShipwireReturn a))
  => ShipwireConfig
  -> ShipwireRequest a TupleBS8 BSL.ByteString
  -> IO (Either String (ShipwireReturn a))
shipwire ShipwireConfig {..} ShipwireRequest {..} = do
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
  response <- httpLbs authorizedRequest manager
  let result = eitherDecode $ responseBody response
  return result

-- | Print the JSON body
debug
  :: (FromJSON (ShipwireReturn a))
  => ShipwireConfig
  -> ShipwireRequest a TupleBS8 BSL.ByteString
  -> IO BSL.ByteString
debug ShipwireConfig {..} ShipwireRequest {..} = do
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
  response <- httpLbs authorizedRequest manager
  let result = responseBody response
  return result
  
-- shipwire usage:
-- let config = ShipwireConfig sandboxUrl (BS8.pack *email*) (BS8.pack *pass*)
-- shipwire config $ getStockInfo -&- (SKU $ T.pack "sku")
