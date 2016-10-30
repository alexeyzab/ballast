{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Main where

import           Ballast.Client
import           Ballast.Types
import           Test.Hspec
import           Test.Hspec.Expectations.Contrib (isRight)
-- isLeft,

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

exampleBadCreateReceiving :: CreateReceiving
exampleBadCreateReceiving =
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
    (ReceivingItems [ReceivingItem (SKU "Ballasttest") (Quantity 0)])
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
main = do
  config <- sandboxEnvConfig
  hspec $ do
    describe "get rates" $ do
      it "gets the correct rates" $ do
        let getRt = mkGetRate (RateOptions USD GroupByAll (CanSplit 1) WarehouseAreaUS Nothing) (RateOrder exampleShipTo exampleItems)
        result <- shipwire config $ createRateRequest getRt
        result `shouldSatisfy` isRight
        let Right RateResponse{..} = result
        rateResponseWarnings `shouldBe` Nothing
        rateResponseErrors `shouldBe` Nothing

    describe "get stock info" $ do
      it "gets stock info without optional args" $ do
        result <- shipwire config $ getStockInfo
        result `shouldSatisfy` isRight

      it "gets stock info with optional args" $ do
        result <- shipwire config $ getStockInfo -&- (SKU "Ballasttest")
        result `shouldSatisfy` isRight

    describe "get receivings" $ do
      it "gets an itemized list of receivings without optional args" $ do
        result <- shipwire config $ getReceivings
        result `shouldSatisfy` isRight

      it "gets an itemized list of receivings with optional args" $ do
        result <- shipwire config $ getReceivings -&- (ExpandParamReceivings [ExpandAll])
                                                  -&- (StatusParams [StatusCanceled])
                                                  -&- (WarehouseIdParam ["TEST 1"])
        result `shouldSatisfy` isRight

    describe "create a new receiving" $ do
      it "creates a new receiving without optional args" $ do
        result <- shipwire config $ createReceiving exampleCreateReceiving
        result `shouldSatisfy` isRight
        let Right ReceivingsResponse{..} = result
        receivingsResponseErrors `shouldBe` Nothing
        receivingsResponseWarnings `shouldBe` Nothing

      it "creates a new receiving with optional args" $ do
        result <- shipwire config $ createReceiving exampleCreateReceiving -&- (ExpandParamReceivings [ExpandHolds, ExpandItems])
        result `shouldSatisfy` isRight
        let Right ReceivingsResponse{..} = result
        receivingsResponseErrors `shouldBe` Nothing
        receivingsResponseWarnings `shouldBe` Nothing

      it "doesn't create a receiving with bad JSON" $ do
        result <- shipwire config $ createReceiving exampleBadCreateReceiving
        let Right ReceivingsResponse {..} = result
        receivingsResponseErrors `shouldBe`
          Just
            (ResponseErrors
               [ Error
                   (ErrorCode "orderSubmitFailed")
                   (ErrorMessage
                      "Item quantity too low, please insert a quantity greater than 2.")
                   ErrorError
               ])

