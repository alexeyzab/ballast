{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Ballast.Client
import           Ballast.Types
import           Test.Hspec
import           Test.Hspec.Expectations.Contrib (isRight)

mkGetRate :: RateOptions -> RateOrder -> GetRate
mkGetRate ropts rord = GetRate ropts rord

exampleItems :: Items
exampleItems = [ItemInfo ((SKU "Ballasttest"), Quantity 1)]

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

exampleCreateReceiving :: CreateReceiving
exampleCreateReceiving =
  CreateReceiving
    Nothing
    Nothing
    (Just $ ExpectedDateText "2016-05-27T00:00:00-07:00")
    (ReceivingOptions Nothing Nothing $ Just $ WarehouseRegion "TEST 1")
    (ReceivingArrangement
       ArrangementTypeNone
       Nothing
       Nothing)
    (ReceivingShipments
       [ReceivingShipment Nothing Nothing Nothing Nothing $ Type "box"])
    Nothing
    Nothing     
    (ReceivingItems [ReceivingItem (SKU "Ballasttest") (Quantity 3)])
    (ReceivingShipFrom
       Nothing
       (Name "Stephen Alexander")
       (AddressLine "11 Prsilla St")
       Nothing
       (City "New York")
       (State "NY")
       (PostalCode "12345")
       (Country "TestCountry")
       (Phone "12346"))
    Nothing
        
main :: IO ()
main = hspec $ do
  describe "get rates" $ do
    it "gets the correct rates" $ do
      config <- sandboxEnvConfig
      let getRt = mkGetRate (RateOptions USD GroupByAll (CanSplit 1) WarehouseAreaUS Nothing) (RateOrder exampleShipTo exampleItems)
      result <- shipwire config $ createRateRequest getRt
      result `shouldSatisfy` isRight
  describe "get stock info" $ do
    it "gets stock info without optional args" $ do
      config <- sandboxEnvConfig
      result <- shipwire config $ getStockInfo
      result `shouldSatisfy` isRight
    it "gets stock info with optional args" $ do
      config <- sandboxEnvConfig
      result <- shipwire config $ getStockInfo -&- (SKU "Ballasttest")
      result `shouldSatisfy` isRight
  describe "get receivings" $ do
    it "gets an itemized list of receivings without optional args" $ do
      config <- sandboxEnvConfig
      result <- shipwire config $ getReceivings
      result `shouldSatisfy` isRight
  describe "create a new receivings" $ do
    it "creates a new receiving without optional args" $ do
      config <- sandboxEnvConfig
      result <- shipwire config $ createReceiving exampleCreateReceiving
      result `shouldSatisfy` isRight
