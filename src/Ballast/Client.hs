module Ballast.Client where

import           Ballast.Types
import           Data.Aeson                 (encode)
import qualified Data.ByteString            as BS
import qualified Data.ByteString.Char8      as BS8
import qualified Data.ByteString.Lazy.Char8 as BSL
import           Network.HTTP.Client
import           Network.HTTP.Client.TLS
import           Network.HTTP.Types.Status  (statusCode)
import           System.Environment

baseUrl = "https://api.shipwire.com/api/v3"
sandboxUrl = "https://api.beta.shipwire.com/api/v3"

main :: IO ()
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
    putStrLn $
        "The status code was: " ++ (show $ statusCode $ responseStatus response)
    BSL.putStrLn $ responseBody response
