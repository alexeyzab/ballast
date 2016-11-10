{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Main where

import           Ballast.Client
import           Ballast.Types
import qualified Data.Text                       as T
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
    (Just $ ExpectedDateText "2016-11-08T00:00:00-07:00")
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
       (Country "USA")
       (Phone "12346"))
    Nothing

exampleBadCreateReceiving :: CreateReceiving
exampleBadCreateReceiving =
  CreateReceiving
    Nothing
    Nothing
    (Just $ ExpectedDateText "2016-11-27T00:00:00-07:00")
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

exampleModifiedReceiving :: ModifyReceiving
exampleModifiedReceiving =
  CreateReceiving
    Nothing
    Nothing
    (Just $ ExpectedDateText "2016-11-27T00:00:00-07:00")
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
       (Country "Modified Country")
       (Phone "12346"))
    Nothing

createReceivingHelper :: ShipwireConfig -> CreateReceiving -> IO (Either ShipwireError (ShipwireReturn CreateReceivingRequest), ReceivingId)
createReceivingHelper conf cr = do
  receiving <- shipwire conf $ createReceiving cr
  let Right ReceivingsResponse {..} = receiving
  let ReceivingsResource {..} = receivingsResponseResource
  let ReceivingsItems {..} = receivingsResponseItems
  let ReceivingsItem {..} = last unReceivingsItems
  let ReceivingsItemResource {..} = receivingsItemResource
  let receivingId = T.pack $ show $ unId rirId
  return (receiving, ReceivingId receivingId)

main :: IO ()
main = do
  config <- sandboxEnvConfig
  (receiving, receivingId) <- createReceivingHelper config exampleCreateReceiving
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
      it "gets stock info with optional args" $ do
        result <- shipwire config $ getStockInfo -&- (SKU "Ballasttest")
        result `shouldSatisfy` isRight

    describe "get receivings" $ do
      it "gets an itemized list of receivings with optional args" $ do
        result <- shipwire config $ getReceivings -&- (ExpandReceivingsParam [ExpandAll])
                                                  -&- (StatusParams [StatusCanceled])
                                                  -&- (WarehouseIdParam ["TEST 1"])
        result `shouldSatisfy` isRight

    describe "create a new receiving" $ do
      it "creates a new receiving with optional args" $ do
        receiving `shouldSatisfy` isRight
        let Right ReceivingsResponse {..} = receiving
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

    describe "get infromation about a receiving" $ do
      it "gets info about a receiving" $ do
        result <- shipwire config $ getReceiving receivingId -&- (ExpandReceivingsParam [ExpandHolds, ExpandItems])
        result `shouldSatisfy` isRight
        let Right ReceivingResponse {..} = result
        receivingResponseErrors `shouldBe` Nothing
        receivingResponseWarnings `shouldBe` Nothing

    describe "modify information about a receiving" $ do
      it "modifies info about a receiving" $ do
        result <- shipwire config $ modifyReceiving receivingId exampleModifiedReceiving
        result `shouldSatisfy` isRight
        let Right ReceivingsResponse {..} = result
        receivingsResponseErrors `shouldBe` Nothing
        receivingsResponseWarnings `shouldBe` Nothing
        modifiedReceiving <- shipwire config $ getReceiving receivingId
        let Right ReceivingResponse {..} = modifiedReceiving
        let ReceivingsItemResource {..} = receivingResponseResource
        let ItemResourceShipFrom {..} = rirShipFrom
        let ItemResourceShipFromResource {..} = irsfResource
        irsfrCountry `shouldBe` Just (Country "Modified Country")

    describe "cancel a receiving" $ do
      it "cancels a receiving" $ do
        result <- shipwire config $ cancelReceiving receivingId
        result `shouldSatisfy` isRight
        let Right SimpleResponse {..} = result
        message `shouldBe` (ResponseMessage "Receiving was cancelled")

    describe "cancel shipping labels" $ do
      it "cancels shipping labels on a receiving" $ do
        result <- shipwire config $ cancelReceivingLabels receivingId
        result `shouldSatisfy` isRight
        let Right SimpleResponse {..} = result
        message `shouldBe` (ResponseMessage "Labels cancelled")

    describe "get list of holds for a receiving" $ do
      it "gets a list of holds for a receiving" $ do
        result <- shipwire config $ getReceivingHolds receivingId -&- IncludeCleared
        result `shouldSatisfy` isRight
        let Right GetReceivingHoldsResponse {..} = result
        grhrWarnings `shouldBe` Nothing
        grhrErrors `shouldBe` Nothing

    describe "get email recipients and instructions for a receiving" $ do
      it "gets email recipients and instructions for a receiving" $ do
        result <- shipwire config $ getReceivingInstructionsRecipients receivingId
        result `shouldSatisfy` isRight
        let Right GetReceivingInstructionsRecipientsResponse {..} = result
        grirrWarnings `shouldBe` Nothing
        grirrErrors `shouldBe` Nothing

    describe "get contents of a receiving" $ do
      it "gets contents of a receiving" $ do
        result <- shipwire config $ getReceivingItems receivingId
        result `shouldSatisfy` isRight
        let Right GetReceivingItemsResponse {..} = result
        grirWarnings `shouldBe` Nothing
        grirErrors `shouldBe` Nothing

    describe "get shipping dimension and container information" $ do
      it "gets shipping dimension and container infromation" $ do
        result <- shipwire config $ getReceivingShipments receivingId
        result `shouldSatisfy` isRight
        let Right GetReceivingShipmentsResponse {..} = result
        grsrWarnings `shouldBe` Nothing
        grsrErrors `shouldBe` Nothing

    describe "get tracking information for a receiving" $ do
      it "gets tracking information for a receiving" $ do
        result <- shipwire config $ getReceivingTrackings receivingId
        result `shouldSatisfy` isRight
        let Right GetReceivingTrackingsResponse {..} = result
        grtrWarnings `shouldBe` Nothing
        grtrErrors `shouldBe` Nothing

    describe "get labels information for a receiving" $ do
      it "gets labels information for a receiving" $ do
        result <- shipwire config $ getReceivingLabels receivingId
        result `shouldSatisfy` isRight
        let Right GetReceivingLabelsResponse {..} = result
        grlrWarnings `shouldBe` Nothing
        grlrErrors `shouldBe` Nothing
