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
exampleItems = [ItemInfo ((SKU "HspecTest"), Quantity 1)]

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
    (ReceivingItems [ReceivingItem (SKU "HspecTest") (Quantity 3)])
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
    (ReceivingItems [ReceivingItem (SKU "HspecTest") (Quantity 0)])
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
    (ReceivingItems [ReceivingItem (SKU "HspecTest") (Quantity 3)])
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

-- | Takes a product id as an integer and inserts it in the right spot.
exampleCreateProduct :: Integer -> [CreateProductsWrapper]
exampleCreateProduct productId =
                       [CpwVirtualKit $ VirtualKit
                          (SKU "HspecTestVKit")
                          (VirtualKitClassification)
                          (Description "This is a virtual kit test")
                          (VirtualKitContent
                            [(VirtualKitContentObject
                              (ProductId productId)
                              Nothing
                              (Quantity 5)
                            )
                            ]
                          ),
                        CpwKit $ Kit
                          (SKU "HspecTestKit")
                          Nothing
                          (KitClassification)
                          (Description "This is a kit test")
                          (BatteryConfiguration "HASLOOSEBATTERY")
                          (HsCode "010612")
                          (CountryOfOrigin "US")
                          (KitValues
                            (CostValue 1)
                            (WholesaleValue 2)
                            (RetailValue 4)
                            (CostCurrency "USD")
                            (WholesaleCurrency "USD")
                            (RetailCurrency "USD")
                          )
                          (KitAlternateNames [KitAlternateName (Name "HspecTestAlt")])
                          -- KitContentObject needs to include ids of other products
                          -- included in this kit. We use a helper function to create a
                          -- product and get back that product's id.
                          (KitContent
                            [KitContentObject
                              (ProductId productId)
                              Nothing
                              (Quantity 5)
                            ]
                          )
                          (KitDimensions
                            (KitLength 2)
                            (KitWidth 2)
                            (KitHeight 2)
                            (KitWeight 2)
                          )
                          (KitTechnicalData
                            (KitTechnicalDataBattery
                              (BatteryType "ALKALINE")
                              (BatteryWeight 3)
                              (NumberOfBatteries 5)
                              (Capacity 6)
                              (NumberOfCells 7)
                              (CapacityUnit "WATTHOUR")
                            )
                          )
                          (KitFlags
                            NotPackagedReadyToShip
                            NotFragile
                            NotDangerous
                            NotPerishable
                            NotMedia
                            NotAdult
                            Liquid
                            HasBattery
                            HasInnerPack
                            HasMasterCase
                            HasPallet
                          )
                          (KitInnerPack
                           (IndividualItemsPerCase 2)
                           (SKU "KitInnerPack")
                           (Description "This is a test for kit inner pack")
                           (KitValues
                             (CostValue 1)
                             (WholesaleValue 2)
                             (RetailValue 4)
                             (CostCurrency "USD")
                             (WholesaleCurrency "USD")
                             (RetailCurrency "USD")
                           )
                           (KitDimensions
                             (KitLength 2)
                             (KitWidth 2)
                             (KitHeight 2)
                             (KitWeight 2)
                           )
                           (KitInnerPackFlags
                             NotPackagedReadyToShip
                           )
                          )
                          (KitMasterCase
                            (IndividualItemsPerCase 10)
                            (SKU "KitMasterCase")
                            (Description "This is a test for kit master case")
                            (KitValues
                              (CostValue 1)
                              (WholesaleValue 2)
                              (RetailValue 4)
                              (CostCurrency "USD")
                              (WholesaleCurrency "USD")
                              (RetailCurrency "USD")
                            )
                            (KitDimensions
                              (KitLength 4)
                              (KitWidth 4)
                              (KitHeight 4)
                              (KitWeight 4)
                            )
                            (KitMasterCaseFlags
                              NotPackagedReadyToShip
                            )
                          )
                          (KitPallet
                            (IndividualItemsPerCase 1000)
                            (SKU "KitPallet")
                            (Description "A pallet for hspec kit")
                            (KitValues
                              (CostValue 1)
                              (WholesaleValue 2)
                              (RetailValue 4)
                              (CostCurrency "USD")
                              (WholesaleCurrency "USD")
                              (RetailCurrency "USD")
                            )
                            (KitDimensions
                              (KitLength 8)
                              (KitWidth 8)
                              (KitHeight 8)
                              (KitWeight 8)
                            )
                            (KitPalletFlags
                              NotPackagedReadyToShip
                            )
                          ),
                        CpwMarketingInsert $ MarketingInsert
                          (SKU "HspecTestInsert2")
                          Nothing
                          (MarketingInsertClassification)
                          (Description "Hspec test marketing insert2")
                          (InclusionRuleType "CUSTOM")
                          (MarketingInsertAlternateNames [MarketingInsertAlternateName (Name "HspecMI22")])
                          (MarketingInsertDimensions
                            (MarketingInsertLength 0.1)
                            (MarketingInsertWidth 0.1)
                            (MarketingInsertHeight 0.1)
                            (MarketingInsertWeight 0.2)
                          )
                          (MarketingInsertFlags
                            ShouldNotFold
                          )
                          (MarketingInsertInclusionRules
                            (InsertAfterDate "3016-02-15T13:04:26-05:00")
                            (InsertBeforeDate "3016-02-15T13:04:26-05:00")
                            (InsertWhenWorthValue 5)
                            (InsertWhenQuantity 5)
                            (InsertWhenWorthCurrency "USD")
                          )
                          (MarketingInsertMasterCase
                            (IndividualItemsPerCase 10)
                            (SKU "HspecTestMIMCSKU")
                            Nothing
                            (Description "Hspec test marketing insert master case2")
                            (MarketingInsertMasterCaseDimensions
                              (MarketingInsertMasterCaseDimensionsLength 8)
                              (MarketingInsertMasterCaseDimensionsWidth 8)
                              (MarketingInsertMasterCaseDimensionsHeight 8)
                              (MarketingInsertMasterCaseDimensionsWeight 8)
                            )
                          ),
                        CpwBaseProduct $ BaseProduct
                          (SKU "HspecTest2")
                          Nothing
                          (BaseProductClassification)
                          (Description "Hspec test product2")
                          (Just $ HsCode "010612")
                          (CountryOfOrigin "US")
                          (Category "TOYS_SPORTS_HOBBIES")
                          (BatteryConfiguration "ISBATTERY")
                          (Values 
                            (CostValue 1)
                            (WholesaleValue 2)
                            (RetailValue 4)
                            (CostCurrency "USD")
                            (WholesaleCurrency "USD")
                            (RetailCurrency "USD")
                          )
                          (BaseProductAlternateNames [BaseProductAlternateName (Name "HspecAlt2")])
                          (BaseProductDimensions 
                            (BaseProductLength 10)
                            (BaseProductWidth 10)
                            (BaseProductHeight 10)
                            (BaseProductWeight 10)
                          )
                          (BaseProductTechnicalData 
                            (BaseProductTechnicalDataBattery 
                                (BatteryType "ALKALINE")
                                (BatteryWeight 3)
                                (NumberOfBatteries 5)
                                (Capacity 6)
                                (NumberOfCells 7)
                                (CapacityUnit "WATTHOUR")
                            )
                          )
                          (BaseProductFlags 
                            PackagedReadyToShip
                            Fragile
                            NotDangerous
                            NotPerishable
                            NotMedia
                            NotAdult
                            NotLiquid
                            HasInnerPack
                            HasMasterCase
                            HasPallet
                          )
                          (BaseProductInnerPack 
                            (IndividualItemsPerCase 2)
                            (ExternalId "narp222")
                            (SKU "singleInner22")
                            (Description "InnerDec2")
                            (Values 
                                (CostValue 1)
                                (WholesaleValue 2)
                                (RetailValue 4)
                                (CostCurrency "USD")
                                (WholesaleCurrency "USD")
                                (RetailCurrency "USD")
                            )
                            (BaseProductDimensions 
                                (BaseProductLength 20)
                                (BaseProductWidth 20)
                                (BaseProductHeight 20)
                                (BaseProductWeight 20)
                            )
                            (BaseProductInnerPackFlags 
                                NotPackagedReadyToShip
                            )
                          )
                          (BaseProductMasterCase 
                            (IndividualItemsPerCase 10)
                            (ExternalId "narp33")
                            (SKU "singleMaster23")
                            (Description "masterdesc3")
                            (Values
                                (CostValue 1)
                                (WholesaleValue 2)
                                (RetailValue 4)
                                (CostCurrency "USD")
                                (WholesaleCurrency "USD")
                                (RetailCurrency "USD")
                            )
                            (BaseProductDimensions 
                                (BaseProductLength 30)
                                (BaseProductWidth 30)
                                (BaseProductHeight 30)
                                (BaseProductWeight 30)
                            )
                            (BaseProductMasterCaseFlags PackagedReadyToShip)
                          )
                          (BaseProductPallet 
                            (IndividualItemsPerCase 1000)
                            (ExternalId "narp42")
                            (SKU "singlePallet22")
                            (Description "palletdesc2")
                            (Values 
                                (CostValue 1)
                                (WholesaleValue 2)
                                (RetailValue 4)
                                (CostCurrency "USD")
                                (WholesaleCurrency "USD")
                                (RetailCurrency "USD")
                            )
                            (BaseProductDimensions 
                                (BaseProductLength 40)
                                (BaseProductWidth 40)
                                (BaseProductHeight 40)
                                (BaseProductWeight 40)
                            )
                            (BaseProductPalletFlags 
                                NotPackagedReadyToShip
                            )
                          )
                        ]

exampleCreateBaseProduct :: [CreateProductsWrapper]
exampleCreateBaseProduct =
  [ CpwBaseProduct $
    BaseProduct
      (SKU "HspecTest3")
      Nothing
      (BaseProductClassification)
      (Description "Hspec test product3")
      (Just $ HsCode "010612")
      (CountryOfOrigin "US")
      (Category "TOYS_SPORTS_HOBBIES")
      (BatteryConfiguration "ISBATTERY")
      (Values
         (CostValue 1)
         (WholesaleValue 2)
         (RetailValue 4)
         (CostCurrency "USD")
         (WholesaleCurrency "USD")
         (RetailCurrency "USD"))
      (BaseProductAlternateNames [BaseProductAlternateName (Name "HspecAlt3")])
      (BaseProductDimensions
         (BaseProductLength 10)
         (BaseProductWidth 10)
         (BaseProductHeight 10)
         (BaseProductWeight 10))
      (BaseProductTechnicalData
         (BaseProductTechnicalDataBattery
            (BatteryType "ALKALINE")
            (BatteryWeight 3)
            (NumberOfBatteries 5)
            (Capacity 6)
            (NumberOfCells 7)
            (CapacityUnit "WATTHOUR")))
      (BaseProductFlags
         PackagedReadyToShip
         Fragile
         NotDangerous
         NotPerishable
         NotMedia
         NotAdult
         NotLiquid
         HasInnerPack
         HasMasterCase
         HasPallet)
      (BaseProductInnerPack
         (IndividualItemsPerCase 2)
         (ExternalId "narp55")
         (SKU "singleInner3")
         (Description "InnerDesc3")
         (Values
            (CostValue 1)
            (WholesaleValue 2)
            (RetailValue 4)
            (CostCurrency "USD")
            (WholesaleCurrency "USD")
            (RetailCurrency "USD"))
         (BaseProductDimensions
            (BaseProductLength 20)
            (BaseProductWidth 20)
            (BaseProductHeight 20)
            (BaseProductWeight 20))
         (BaseProductInnerPackFlags NotPackagedReadyToShip))
      (BaseProductMasterCase
         (IndividualItemsPerCase 10)
         (ExternalId "narp66")
         (SKU "singleMaster4")
         (Description "masterdesc4")
         (Values
            (CostValue 1)
            (WholesaleValue 2)
            (RetailValue 4)
            (CostCurrency "USD")
            (WholesaleCurrency "USD")
            (RetailCurrency "USD"))
         (BaseProductDimensions
            (BaseProductLength 30)
            (BaseProductWidth 30)
            (BaseProductHeight 30)
            (BaseProductWeight 30))
         (BaseProductMasterCaseFlags PackagedReadyToShip))
      (BaseProductPallet
         (IndividualItemsPerCase 1000)
         (ExternalId "narp77")
         (SKU "singlePallet3")
         (Description "palletdesc3")
         (Values
            (CostValue 1)
            (WholesaleValue 2)
            (RetailValue 4)
            (CostCurrency "USD")
            (WholesaleCurrency "USD")
            (RetailCurrency "USD"))
         (BaseProductDimensions
            (BaseProductLength 40)
            (BaseProductWidth 40)
            (BaseProductHeight 40)
            (BaseProductWeight 40))
         (BaseProductPalletFlags NotPackagedReadyToShip))
  ]
  
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

createProductHelper :: ShipwireConfig -> [CreateProductsWrapper] -> IO (Either ShipwireError (ShipwireReturn CreateProductsRequest), Integer)
createProductHelper conf cp = do
  baseProduct <- shipwire conf $ createProduct cp
  let Right GetProductsResponse {..} = baseProduct
  let GetProductsResponseResource {..} = gprResource
  let GetProductsResponseResourceItems {..} = gprrItems
  let GetProductsResponseResourceItem {..} = last gprriItems
  let pwBaseProduct@(PwBaseProduct x) = gprriResource
  let productId = unId $ bprId $ unwrapBaseProduct pwBaseProduct
  return (baseProduct, productId)

unwrapBaseProduct :: ProductsWrapper -> BaseProductResponseResource
unwrapBaseProduct (PwBaseProduct x) = x
unwrapBaseProduct _ = error "Bad input"
main :: IO ()
main = do
  config <- sandboxEnvConfig
  (_, productId) <- createProductHelper config exampleCreateBaseProduct
  (receiving, receivingId) <- createReceivingHelper config exampleCreateReceiving
  hspec $ do
    describe "get rates" $ do
      it "gets the correct rates" $ do
        let getRt = mkGetRate (RateOptions USD GroupByAll Nothing Nothing Nothing (Just IgnoreUnknownSkus) (CanSplit 1) WarehouseAreaUS Nothing) (RateOrder exampleShipTo exampleItems)
        result <- shipwire config $ createRateRequest getRt
        result `shouldSatisfy` isRight
        let Right RateResponse{..} = result
        rateResponseWarnings `shouldBe` Nothing
        rateResponseErrors `shouldBe` Nothing

    describe "get stock info" $ do
      it "gets stock info with optional args" $ do
        result <- shipwire config $ getStockInfo -&- (SKU "HspecTest3")
        result `shouldSatisfy` isRight

    describe "get receivings" $ do
      it "gets an itemized list of receivings with optional args" $ do
        result <- shipwire config $ getReceivings -&- (ExpandReceivingsParam [ExpandAll])
                                                  -&- (ReceivingStatusParams [StatusCanceled])
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

    describe "get an itemized list of products" $ do
      it "gets an itemized list of products" $ do
        result <- shipwire config $ getProducts -&- (ExpandProductsParam [ExpandEnqueuedDimensions])
        result `shouldSatisfy` isRight
        let Right GetProductsResponse {..} = result
        gprWarnings `shouldBe` Nothing
        gprErrors `shouldBe` Nothing

    describe "create a product" $ do
      it "creates all possible product classifications" $ do
        result <- shipwire config $ createProduct (exampleCreateProduct productId)
        result `shouldSatisfy` isRight
        let Right GetProductsResponse {..} = result
        gprWarnings `shouldBe` Nothing
        gprErrors `shouldBe` Nothing
