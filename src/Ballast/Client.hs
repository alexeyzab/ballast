{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module Ballast.Client where

import           Ballast.Types
import           Control.Monad.IO.Class
import           Data.Aeson                 (decode, eitherDecode, encode)
import           Data.Aeson.Types
import qualified Data.ByteString            as BS
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
createRateResponse :: Maybe BSL.ByteString -> ShipWireRequest CreateRateResponse
createRateResponse body = request
  where request = mkShipWireRequest NHTM.methodPost url bod
        url = (T.append sandboxUrl "/rate")
        bod = body

dispatch
  :: (FromJSON (ShipWireReturn a))
  => ShipWireRequest a -> IO (Either String (ShipWireReturn a))
dispatch (ShipWireRequest method endpoint body) = do
  manager <- newManager tlsManagerSettings
  initialRequest <- parseRequest $ (T.unpack endpoint)
  let request =
        initialRequest
        { method = method
        , requestBody = RequestBodyLBS $ fromMaybe (BSL.pack "") body
        }
  shipwireUser <- getEnv "SHIPWIRE_USER"
  shipwirePass <- getEnv "SHIPWIRE_PASS"
  let authorizedRequest =
        applyBasicAuth (BS8.pack shipwireUser) (BS8.pack shipwirePass) request
  response <- httpLbs authorizedRequest manager
  let result = eitherDecode $ responseBody response
  return result

-- Test case for dispatch:
-- let rateReq = createRateResponse (Just $ encode defaultRate)
-- dispatch rateReq
