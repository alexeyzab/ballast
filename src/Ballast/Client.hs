module Ballast.Client where

import           Ballast.Types
import qualified Data.ByteString            as BS
import qualified Data.ByteString.Lazy.Char8 as BSL
import qualified Data.ByteString.Char8      as BS8
import           Network.HTTP.Client
import           Network.HTTP.Client.TLS
import           Network.HTTP.Types.Status (statusCode)
import           System.Environment

baseUrl = "https://api.shipwire.com/api/v3"
sandboxUrl = "https://api.beta.shipwire.com/api/v3"

main :: IO ()
main = do
  manager <- newManager tlsManagerSettings
  request <- parseRequest $ sandboxUrl ++ "/stock"
  shipwireUser <- getEnv "SHIPWIRE_USER"
  shipwirePass <- getEnv "SHIPWIRE_PASS"
  let authorizedRequest =
        applyBasicAuth (BS8.pack shipwireUser) (BS8.pack shipwirePass) request
  response <- httpLbs authorizedRequest manager

  putStrLn $ "The status code was: " ++ (show $ statusCode $ responseStatus response)
  BSL.putStrLn $ responseBody response
