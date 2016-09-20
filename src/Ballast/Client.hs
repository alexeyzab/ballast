{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module Ballast.Client where

import           Ballast.Types
import           Data.Aeson                 (eitherDecode, encode)
import           Data.Aeson.Types
import qualified Data.ByteString.Char8      as BS8
import qualified Data.ByteString.Lazy.Char8 as BSL
import           Data.Maybe                 (fromMaybe)
import           Data.Text                  (Text)
import qualified Data.Text                  as T
import           Network.HTTP.Client
import           Network.HTTP.Client.TLS
import qualified Network.HTTP.Types.Method  as NHTM
import           System.Environment

baseUrl :: Text
baseUrl = "https://api.shipwire.com/api/v3"

sandboxUrl :: Text
sandboxUrl = "https://api.beta.shipwire.com/api/v3"

-- | Generate a real-time shipping quote
-- | https://www.shipwire.com/w/developers/rate/
createRateRequest :: GetRate -> ShipWireRequest RateRequest
createRateRequest getRate = request
  where
    request = mkShipWireRequest NHTM.methodPost url (Just $ encode getRate)
    url = (T.append sandboxUrl "/rate")

-- | Get stock information for your products.
-- | https://www.shipwire.com/w/developers/stock/
createStockRequest :: ShipWireRequest StockRequest
createStockRequest = request
  where
    request = mkShipWireRequest NHTM.methodGet url Nothing
    url = (T.append sandboxUrl "/stock")

dispatch
  :: (FromJSON (ShipWireReturn a))
  => ShipWireRequest a -> IO (Either String (ShipWireReturn a))
dispatch (ShipWireRequest meth ep bod) = do
  manager <- newManager tlsManagerSettings
  initialRequest <- parseRequest $ (T.unpack ep)
  let req =
        initialRequest
        { method = meth
        , requestBody = RequestBodyLBS $ fromMaybe (BSL.pack "") bod
        }
  shipwireUser <- getEnv "SHIPWIRE_USER"
  shipwirePass <- getEnv "SHIPWIRE_PASS"
  let authorizedRequest =
        applyBasicAuth (BS8.pack shipwireUser) (BS8.pack shipwirePass) req
  response <- httpLbs authorizedRequest manager
  let result = eitherDecode $ responseBody response
  return result

-- Test case for dispatch:
-- let rateReq = createRateRequest defaultGetRate
-- dispatch rateReq
