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
import           Network.HTTP.Types.Status  (statusCode)
import           System.Environment

baseUrl = "https://api.shipwire.com/api/v3"
sandboxUrl = "https://api.beta.shipwire.com/api/v3"

main :: IO RateResponse
main = do
  manager <- newManager tlsManagerSettings
  initialRequest <- parseRequest $ sandboxUrl ++ "/rate"
  let request =
        initialRequest
        { method = (BS8.pack "POST")
        , requestBody = RequestBodyLBS $ encode defaultRate
        }
  shipwireUser <- getEnv "SHIPWIRE_USER"
  shipwirePass <- getEnv "SHIPWIRE_PASS"
  let authorizedRequest =
        applyBasicAuth (BS8.pack shipwireUser) (BS8.pack shipwirePass) request
  response <- httpLbs authorizedRequest manager
  let mkResult = decode $ responseBody response
  case mkResult of
    Just rateResult -> return rateResult
    Nothing -> error "Something went wrong"

mkRequest :: Method -> Text -> Maybe BSL.ByteString -> IO Reply
mkRequest rMethod endpoint body = do
  manager <- newManager tlsManagerSettings
  initialRequest <- parseRequest $ (T.unpack $ T.append (T.pack sandboxUrl) endpoint)
  let reqBody = RequestBodyLBS $ fromMaybe (BSL.pack "") body
  let req = initialRequest { method = rMethod, requestBody = reqBody }
  shipwireUser <- liftIO $ getEnv "SHIPWIRE_USER"
  shipwirePass <- liftIO $ getEnv "SHIPWIRE_PASS"
  let authorizedRequest =
        applyBasicAuth (BS8.pack shipwireUser) (BS8.pack shipwirePass) req
  httpLbs authorizedRequest manager

-- Test case for mkRequest:
-- mkRequest NHTM.methodPost (T.pack "/rate") (Just $ encode defaultRate)
