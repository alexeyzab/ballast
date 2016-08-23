module Ballast.Client where

import           Ballast.Types
import qualified Data.ByteString           as BS
import           Network.HTTP.Client
import           Network.HTTP.Client.TLS
import           Network.HTTP.Types.Status (statusCode)

baseUrl = "https://api.shipwire.com"

main :: Username -> Password -> IO ()
main user pass = do
  manager <- newManager tlsManagerSettings

  request <- parseRequest $ baseUrl ++ "/api/v3/orders"
  let authorizedRequest = applyBasicAuth (username user) (password pass) request
  response <- httpLbs authorizedRequest manager

  putStrLn $ "The status code was: " ++ (show $ statusCode $ responseStatus response)
  print $ responseBody response
