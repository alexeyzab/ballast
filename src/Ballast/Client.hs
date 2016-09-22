{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module Ballast.Client where

import           Ballast.Types
import           Data.Aeson                 (eitherDecode, encode)
import           Data.Aeson.Types
import qualified Data.ByteString.Char8      as BS8
import qualified Data.ByteString.Lazy.Char8 as BSL
import           Data.Maybe                 (fromMaybe)
import qualified Data.Text                  as T
import           Network.HTTP.Client
import           Network.HTTP.Client.TLS
import qualified Network.HTTP.Types.Method  as NHTM
import           System.Environment

baseUrl :: Host
baseUrl = "https://api.shipwire.com/api/v3"

sandboxUrl :: Host
sandboxUrl = "https://api.beta.shipwire.com/api/v3"

-- | Generate a real-time shipping quote
-- | https://www.shipwire.com/w/developers/rate/
createRateRequest :: GetRate -> ShipwireRequest RateRequest
createRateRequest getRate = request
  where
    request = mkShipwireRequest NHTM.methodPost url (Just $ encode getRate)
    url = "/rate"

-- | Get stock information for your products.
-- | https://www.shipwire.com/w/developers/stock/
getStockInfo :: ShipwireRequest StockRequest
getStockInfo = request
  where
    request = mkShipwireRequest NHTM.methodGet url Nothing
    url = "/stock"

dispatch
  :: (FromJSON (ShipwireReturn a))
  => Host
  -> ShipwireRequest a
  -> Params
  -> IO (Either String (ShipwireReturn a))
dispatch hst (ShipwireRequest meth ep bod) par = do
  manager <- newManager tlsManagerSettings
  initialRequest <- parseRequest $ T.unpack (T.append hst ep)
  let req =
        initialRequest
        { method = meth
        , requestBody = RequestBodyLBS $ fromMaybe (BSL.pack "") bod
        }
      req' = setQueryString par req
  shipwireUser <- getEnv "SHIPWIRE_USER"
  shipwirePass <- getEnv "SHIPWIRE_PASS"
  let authorizedRequest =
        applyBasicAuth (BS8.pack shipwireUser) (BS8.pack shipwirePass) req'
  response <- httpLbs authorizedRequest manager
  let result = eitherDecode $ responseBody response
  return result

-- Test case for dispatch:
-- let rateReq = createRateRequest defaultGetRate
-- dispatch sandboxUrl rateReq []
