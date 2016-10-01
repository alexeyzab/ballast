{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Ballast.Client
import           Ballast.Types
import qualified Data.ByteString.Char8 as BS8
import           System.Environment (getEnv)

import           Test.Hspec
import           Test.Hspec.Expectations.Contrib (isRight)

mkGetRate :: RateOptions -> RateOrder -> GetRate
mkGetRate ropts rord = GetRate ropts rord

exampleItems :: Items
exampleItems = [ItemInfo ((SKU "Ballasttest"), 1)]

exampleShipTo :: ShipTo
exampleShipTo =
  ShipTo
    (AddressLine "6501 Railroad Avenue SE")
    (AddressLine "Room 315")
    (AddressLine "")
    (City "Snoqualmie")
    (PostalCode "85283")
    (Region "WA")
    (Country "US")
    Commercial
    NotPoBox

main :: IO ()
main = hspec $ do
  describe "get rates" $ do
    it "gets the correct rates" $ do
      login <- getEnv "SHIPWIRE_USER"
      passw <- getEnv "SHIPWIRE_PASS"
      let config = ShipwireConfig "https://api.beta.shipwire.com/api/v3" (BS8.pack login) (BS8.pack passw)
          getRt = mkGetRate (RateOptions USD GroupByAll 1 WarehouseAreaUS Nothing) (RateOrder exampleShipTo exampleItems)
      result <- shipwire config $ createRateRequest getRt
      result `shouldSatisfy` isRight
  describe "get stock info" $ do
    it "gets stock info without optional args" $ do
      login <- getEnv "SHIPWIRE_USER"
      passw <- getEnv "SHIPWIRE_PASS"
      let config = ShipwireConfig "https://api.beta.shipwire.com/api/v3" (BS8.pack login) (BS8.pack passw)
      result <- shipwire config $ getStockInfo
      result `shouldSatisfy` isRight
    it "gets stock info with optional args" $ do
      login <- getEnv "SHIPWIRE_USER"
      passw <- getEnv "SHIPWIRE_PASS"
      let config = ShipwireConfig "https://api.beta.shipwire.com/api/v3" (BS8.pack login) (BS8.pack passw)
      result <- shipwire config $ getStockInfo -&- (SKU "Ballasttest")
      result `shouldSatisfy` isRight
