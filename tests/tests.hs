{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Main where

import           Ballast.Client
import           Ballast.Types
import           Data.Maybe (fromJust)
import           Data.Monoid ((<>))
import qualified Data.Text as T
import           Data.Time (Day(..), secondsToDiffTime)
import           Data.Time.Clock (UTCTime(..), getCurrentTime, utctDayTime)
import           Network.HTTP.Client (Manager, newManager)
import           Network.HTTP.Client.TLS (tlsManagerSettings)
import           Test.Hspec
import           Test.Hspec.Expectations.Contrib (isRight)

mkGetRate :: RateOptions -> RateOrder -> GetRate
mkGetRate ropts rord = GetRate ropts rord

exampleItems :: SKU -> Items
exampleItems productSku = [ItemInfo ((productSku), Quantity 5)]

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

exampleCreateReceiving :: SKU -> CreateReceiving
exampleCreateReceiving productSku =
  CreateReceiving
    Nothing
    Nothing
    (Just $ ExpectedDate $ UTCTime (ModifiedJulianDay 155000) (secondsToDiffTime 10))
    (ReceivingOptions Nothing Nothing $ Just $ WarehouseRegion "TEST 1")
    (ReceivingArrangement
       ArrangementTypeNone
       Nothing
       Nothing)
    (ReceivingShipments
       [ReceivingShipment Nothing Nothing Nothing Nothing $ Type "box"])
    Nothing
    Nothing
    (ReceivingItems [ReceivingItem (productSku) (Quantity 5)])
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
    (Just $ ExpectedDate $ UTCTime (ModifiedJulianDay 100000) (secondsToDiffTime 10))
    (ReceivingOptions Nothing Nothing $ Just $ WarehouseRegion "TEST 1")
    (ReceivingArrangement
       ArrangementTypeNone
       Nothing
       Nothing)
    (ReceivingShipments
       [ReceivingShipment Nothing Nothing Nothing Nothing $ Type "box"])
    Nothing
    Nothing
    (ReceivingItems [ReceivingItem (SKU "HspecTest8") (Quantity 0)])
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
    (Just $ ExpectedDate $ UTCTime (ModifiedJulianDay 100000) (secondsToDiffTime 10))
    (ReceivingOptions Nothing Nothing $ Just $ WarehouseRegion "TEST 1")
    (ReceivingArrangement
       ArrangementTypeNone
       Nothing
       Nothing)
    (ReceivingShipments
       [ReceivingShipment Nothing Nothing Nothing Nothing $ Type "box"])
    Nothing
    Nothing
    (ReceivingItems [ReceivingItem (SKU "HspecTest8") (Quantity 5)])
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
exampleCreateProduct :: ProductId -> [CreateProductsWrapper]
exampleCreateProduct productId =
                       [CpwVirtualKit $ VirtualKit
                          Nothing
                          (SKU "HspecTestVKit")
                          (VirtualKitClassification)
                          (Description "This is a virtual kit test")
                          (VirtualKitContent
                            [(VirtualKitContentObject
                              (Just $ productId)
                              Nothing
                              (Quantity 5)
                            )
                            ]
                          ),
                        CpwKit $ Kit
                          Nothing
                          (SKU "HspecTestKit")
                          Nothing
                          (KitClassification)
                          (Description "This is a kit test")
                          (HasLooseBatteryConfiguration)
                          (Just $ HsCode "010612")
                          (Just $ CountryOfOrigin "US")
                          (KitValues
                            (CostValue 1)
                            (WholesaleValue 2)
                            (RetailValue 4)
                            (Just $ CostCurrency "USD")
                            (Just $ WholesaleCurrency "USD")
                            (Just $ RetailCurrency "USD")
                          )
                          (Just $ KitAlternateNames [KitAlternateName (Name "HspecTestAlt")])
                          -- KitContentObject needs to include ids of other products
                          -- included in this kit. We use a helper function to create a
                          -- product and get back that product's id.
                          (KitContent
                            [KitContentObject
                              (Just $ productId)
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
                          (Just $ KitTechnicalData
                            (KitTechnicalDataBattery
                              (BatteryType "ALKALINE")
                              (Just $ BatteryWeight 3)
                              (Just $ NumberOfBatteries 5)
                              (Just $ Capacity 6)
                              (Just $ NumberOfCells 7)
                              (Just $ CapacityUnit "WATTHOUR")
                            )
                          )
                          Nothing
                          Nothing
                          Nothing
                          Nothing,
                        CpwMarketingInsert $ MarketingInsert
                          Nothing
                          (SKU "HspecTestInsert3")
                          Nothing
                          (MarketingInsertClassification)
                          (Description "Hspec test marketing insert2")
                          (InclusionRuleType "CUSTOM")
                          (Just $ MarketingInsertAlternateNames [MarketingInsertAlternateName (Name "HspecMI3")])
                          (MarketingInsertDimensions
                            (MarketingInsertLength 0.1)
                            (MarketingInsertWidth 0.1)
                            (MarketingInsertHeight 0.1)
                            (MarketingInsertWeight 0.2)
                          )
                          Nothing
                          (Just $ MarketingInsertInclusionRules
                            (Just $ InsertAfterDate $ UTCTime (ModifiedJulianDay 160000) (secondsToDiffTime 10))
                            (Just $ InsertBeforeDate $ UTCTime (ModifiedJulianDay 160000) (secondsToDiffTime 10))
                            (Just $ InsertWhenWorthValue 5)
                            (Just $ InsertWhenQuantity 5)
                            (Just $ InsertWhenWorthCurrency "USD")
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
                          Nothing
                          (SKU "HspecTest9")
                          Nothing
                          BaseProductClassification
                          (Description "Hspec test product2")
                          Nothing
                          Nothing
                          (Category "TOYS_SPORTS_HOBBIES")
                          NoBatteryConfiguration
                          (Values
                            (CostValue 1)
                            Nothing
                            (RetailValue 4)
                            (Just $ CostCurrency "USD")
                            (Just $ WholesaleCurrency "USD")
                            (Just $ RetailCurrency "USD")
                          )
                          Nothing
                          (BaseProductDimensions
                            (BaseProductLength 10)
                            (BaseProductWidth 10)
                            (BaseProductHeight 10)
                            (BaseProductWeight 10)
                          )
                          Nothing
                          Nothing
                          Nothing
                          Nothing
                          Nothing
                        ]

exampleModifyProducts :: Id -> SKU -> [CreateProductsWrapper]
exampleModifyProducts i sku =
  [ CpwBaseProduct $
    BaseProduct
      (Just i)
      sku
      Nothing
      BaseProductClassification
      (Description "Modified description")
      Nothing
      Nothing
      (Category "TOYS_SPORTS_HOBBIES")
      NoBatteryConfiguration
      (Values
         (CostValue 1)
         Nothing
         (RetailValue 4)
         (Just $ CostCurrency "USD")
         (Just $ WholesaleCurrency "USD")
         (Just $ RetailCurrency "USD"))
      Nothing
      (BaseProductDimensions
         (BaseProductLength 10)
         (BaseProductWidth 10)
         (BaseProductHeight 10)
         (BaseProductWeight 10))
      Nothing
      Nothing
      Nothing
      Nothing
      Nothing
  ]

exampleModifyProduct :: Id -> SKU -> CreateProductsWrapper
exampleModifyProduct prId prSku =
  CpwBaseProduct $
    BaseProduct
      (Just prId)
      (prSku)
      Nothing
      BaseProductClassification
      (Description "Modified description")
      Nothing
      Nothing
      (Category "TOYS_SPORTS_HOBBIES")
      NoBatteryConfiguration
      (Values
         (CostValue 1)
         Nothing
         (RetailValue 4)
         (Just $ CostCurrency "USD")
         (Just $ WholesaleCurrency "USD")
         (Just $ RetailCurrency "USD"))
      Nothing
      (BaseProductDimensions
         (BaseProductLength 10)
         (BaseProductWidth 10)
         (BaseProductHeight 10)
         (BaseProductWeight 10))
      Nothing
      Nothing
      Nothing
      Nothing
      Nothing

exampleCreateBaseProduct :: T.Text -> [CreateProductsWrapper]
exampleCreateBaseProduct randomSecond =
  [ CpwBaseProduct $
    BaseProduct
      Nothing
      (SKU $ "HspecTest" <> randomSecond)
      (Just $ ExternalId $ "hspectest" <> randomSecond)
      BaseProductClassification
      (Description "Hspec test product5")
      (Just $ HsCode "010612")
      (Just $ CountryOfOrigin "US")
      (Category "TOYS_SPORTS_HOBBIES")
      NoBatteryConfiguration
      (Values
         (CostValue 1)
         Nothing
         (RetailValue 4)
         (Just $ CostCurrency "USD")
         (Just $ WholesaleCurrency "USD")
         (Just $ RetailCurrency "USD"))
      Nothing
      (BaseProductDimensions
         (BaseProductLength 10)
         (BaseProductWidth 10)
         (BaseProductHeight 10)
         (BaseProductWeight 10))
      Nothing
      (Just $ BaseProductFlags
        (PackagedReadyToShip)
        NotFragile
        NotDangerous
        NotPerishable
        NotMedia
        NotAdult
        NotLiquid
        NoInnerPack
        NoMasterCase
        NoPallet)
      Nothing
      Nothing
      Nothing
  ]

exampleOrder :: T.Text -> SKU -> CreateOrder
exampleOrder randomPart productSku =
  CreateOrder
    Nothing
    (Just $ OrderNo (T.concat ["testorder", randomPart]))
    Nothing
    Nothing
    (Just $ CreateOrderOptions
      (Just $ WarehouseId 10281)
      Nothing
      Nothing
      Nothing
      DomesticOneDay
      Nothing
      NotSameDay
      (Just $ DontForceDuplicate)
      (Just $ DontForceAddress)
      Nothing
      Nothing
      Nothing
      USD
      Nothing
      Nothing
      Nothing
      Nothing
      Nothing
      Nothing
    )
    Nothing
    (OrderShipTo
      (Just $ Email "test@example.com")
      (Name "Test Person")
      (Just $ Company "Best Company")
      (AddressLine "First line")
      (Just $ AddressLine "Second line 25")
      (Just $ AddressLine "")
      (City "Best city")
      (State "WA")
      (Just $ PostalCode "100100")
      (Country "US")
      (Phone "6315613729")
      (NotCommercial)
      (Just $ NotPoBox)
    )
    Nothing
    Nothing
    (OrderItems [OrderItem (Just $ CommercialInvoiceValue 4.5) (Just $ CommercialInvoiceValueCurrency "USD") (Quantity 5) (productSku)])

exampleAddress :: AddressToValidate
exampleAddress = OrderShipTo
                  (Just $ Email "test@example.com")
                  (Name "Test Person")
                  (Just $ Company "My Company")
                  (AddressLine "3351 Michelson Dr STE 100")
                  Nothing
                  Nothing
                  (City "Irvine")
                  (State "CA")
                  (Just $ PostalCode "92612-0697")
                  (Country "US")
                  (Phone "8885551212")
                  Commercial
                  Nothing

createReceivingHelper :: ShipwireConfig -> Manager -> CreateReceiving -> IO (Either ShipwireError (ShipwireReturn CreateReceivingRequest), ReceivingId)
createReceivingHelper conf manager cr = do
  receiving <- shipwireTest conf manager $ createReceiving cr
  let Right GenericResponse {..} = receiving
      Just ReceivingsResource {..} = genericResponseResource
      ReceivingsItems {..} = receivingsResponseItems
      ReceivingsItem {..} = last unReceivingsItems
      ReceivingsItemResource {..} = receivingsItemResource
      receivingId = T.pack $ show $ unId rirId
  return (receiving, ReceivingId receivingId)

createBaseProductHelper :: ShipwireConfig -> Manager -> [CreateProductsWrapper] -> IO (Either ShipwireError (ShipwireReturn CreateProductsRequest), Id, SKU)
createBaseProductHelper conf manager cp = do
  baseProduct <- shipwireTest conf manager $ createProduct cp
  let Right GetProductsResponse {..} = baseProduct
      GetProductsResponseResource {..} = gprResource
      GetProductsResponseResourceItems {..} = gprrItems
      GetProductsResponseResourceItem {..} = last gprriItems
      pwBaseProduct@(PwBaseProduct _) = gprriResource
      productId = bprId $ unwrapBaseProduct pwBaseProduct
      productSku = bprSku $ unwrapBaseProduct pwBaseProduct
  return (baseProduct, productId, productSku)

createOrderHelper :: ShipwireConfig -> Manager -> CreateOrder -> IO (Either ShipwireError (ShipwireReturn CreateOrderRequest), Id)
createOrderHelper conf manager co = do
  order <- shipwireTest conf manager $ createOrder co
  exampleOrd <- shipwireTest conf manager $ getOrders -&- (OrderNoParam $ getOrderNo co)
  let Right GenericResponse {..} = exampleOrd
      Just GetOrdersResponseResource {..} = genericResponseResource
      GetOrdersResponseResourceItems {..} = gorrItems
      GetOrdersResponseResourceItem {..} = head $ gorriItems
      GetOrdersResponseResourceItemResource {..} = gorriResource
      proId = gorrirId
  return (order, proId)

getOrderNo :: CreateOrder -> [T.Text]
getOrderNo co = (unOrderNo $ fromJust $ coOrderNo co) : []

getTimestamp :: IO T.Text
getTimestamp = do
  x <- getCurrentTime
  let prec = show $ utctDayTime x
      result = T.pack $ takeWhile (/= '.') . reverse $ prec
  return result

-- | This function unwraps the wrappers for each type of product and gets all of those products' ids
-- It helps retire the products to clean up after the tests.
getProductIds :: GetProductsResponse -> IO [Integer]
getProductIds products = do
  let getProductsResponseResource      = gprResource products
      getProductsResponseResourceItems = gprrItems getProductsResponseResource
      responseResourceItems            = gprriItems getProductsResponseResourceItems
      productWrappers                  = map gprriResource responseResourceItems
      unwrappedBaseProducts            = unwrapPwBaseProduct productWrappers
      unwrappedMarketingInserts        = unwrapPwMarketingInsert productWrappers
      unwrappedKits                    = unwrapPwKit productWrappers
      unwrappedVirtualKits             = unwrapPwVirtualKit productWrappers
      baseProductIds                   = map (unId . bprId) unwrappedBaseProducts
      marketingInsertIds               = map (unId . mirId) unwrappedMarketingInserts
      kitIds                           = map (unId . krId) unwrappedKits
      virtualKitIds                    = map (unId . vkrId) unwrappedVirtualKits
      allIds                           = baseProductIds <> marketingInsertIds <> kitIds <> virtualKitIds
  return allIds

getModifiedProductsIds :: ModifyProductsResponse -> IO [Integer]
getModifiedProductsIds products = do
  let getProductsResponseResource      = mprResource products
      getProductsResponseResourceItems = gprrItems getProductsResponseResource
      responseResourceItems            = gprriItems getProductsResponseResourceItems
      productWrappers                  = map gprriResource responseResourceItems
      unwrappedBaseProducts            = unwrapPwBaseProduct productWrappers
      unwrappedMarketingInserts        = unwrapPwMarketingInsert productWrappers
      unwrappedKits                    = unwrapPwKit productWrappers
      unwrappedVirtualKits             = unwrapPwVirtualKit productWrappers
      baseProductIds                   = map (unId . bprId) unwrappedBaseProducts
      marketingInsertIds               = map (unId . mirId) unwrappedMarketingInserts
      kitIds                           = map (unId . krId) unwrappedKits
      virtualKitIds                    = map (unId . vkrId) unwrappedVirtualKits
      allIds                           = baseProductIds <> marketingInsertIds <> kitIds <> virtualKitIds
  return allIds

getAllReceivingsIds :: GenericResponse ReceivingsResource -> IO [Integer]
getAllReceivingsIds rr = do
  let (Just receivingsResource) = genericResponseResource rr
      receivingsItems = receivingsResponseItems receivingsResource
      ids = map (unId . rirId . receivingsItemResource) $ unReceivingsItems receivingsItems
  return ids

unwrapBaseProduct :: ProductsWrapper -> BaseProductResponseResource
unwrapBaseProduct (PwBaseProduct x) = x
unwrapBaseProduct _ = error "Bad input"

unwrapMarketingInsert :: ProductsWrapper -> MarketingInsertResponseResource
unwrapMarketingInsert (PwMarketingInsert x) = x
unwrapMarketingInsert _ = error "Bad input"

unwrapPwBaseProduct :: [ProductsWrapper] -> [BaseProductResponseResource]
unwrapPwBaseProduct [] = []
unwrapPwBaseProduct ((PwBaseProduct x):xs) = x : unwrapPwBaseProduct xs
unwrapPwBaseProduct (_:xs) = unwrapPwBaseProduct xs

unwrapPwMarketingInsert :: [ProductsWrapper] -> [MarketingInsertResponseResource]
unwrapPwMarketingInsert [] = []
unwrapPwMarketingInsert ((PwMarketingInsert x):xs) = x : unwrapPwMarketingInsert xs
unwrapPwMarketingInsert (_:xs) = unwrapPwMarketingInsert xs

unwrapPwKit :: [ProductsWrapper] -> [KitResponseResource]
unwrapPwKit [] = []
unwrapPwKit ((PwKit x):xs) = x : unwrapPwKit xs
unwrapPwKit (_:xs) = unwrapPwKit xs

unwrapPwVirtualKit :: [ProductsWrapper] -> [VirtualKitResponseResource]
unwrapPwVirtualKit [] = []
unwrapPwVirtualKit ((PwVirtualKit x):xs) = x : unwrapPwVirtualKit xs
unwrapPwVirtualKit (_:xs) = unwrapPwVirtualKit xs

main :: IO ()
main = do
  manager <- newManager tlsManagerSettings
  config <- sandboxEnvConfig
  hspec $ parallel $ do
    describe "get rates" $ do
      it "gets the correct rates" $ do
        randomPart <- getTimestamp
        (_, productId, productSku) <- createBaseProductHelper config manager $ exampleCreateBaseProduct randomPart
        let getRt = mkGetRate (RateOptions USD GroupByAll Nothing Nothing Nothing (Just IgnoreUnknownSkus) CanSplit (WarehouseArea "US") Nothing Nothing Nothing) (RateOrder exampleShipTo (exampleItems productSku))
        result <- shipwireTest config manager $ createRateRequest getRt
        result `shouldSatisfy` isRight
        _ <- shipwireTest config manager $ retireProducts $ ProductsToRetire [productId]
        let Right GenericResponse {..} = result
        genericResponseWarnings `shouldBe` Nothing
        genericResponseErrors `shouldBe` Nothing

    describe "get stock info" $ do
      it "gets stock info with optional args" $ do
        randomPart <- getTimestamp
        (_, productId, _) <- createBaseProductHelper config manager $ exampleCreateBaseProduct randomPart
        result <- shipwireTest config manager $ getStockInfo -&- (SKU "HspecTest5")
        _ <- shipwireTest config manager $ retireProducts $ ProductsToRetire [productId]
        result `shouldSatisfy` isRight
        let Right GenericResponse {..} = result
        genericResponseWarnings `shouldBe` Nothing
        genericResponseErrors `shouldBe` Nothing

    describe "get receivings" $ do
      it "gets an itemized list of receivings with optional args" $ do
        randomPart <- getTimestamp
        (_, productId, productSku) <- createBaseProductHelper config manager $ exampleCreateBaseProduct randomPart
        (_, receivingId) <- createReceivingHelper config manager $ exampleCreateReceiving productSku
        result <- shipwireTest config manager $ getReceivings -&- (ExpandReceivingsParam [ExpandAll])
                                                  -&- (ReceivingStatusParams [StatusCanceled])
                                                  -&- (WarehouseIdParam ["TEST 1"])
                                                  -&- (UpdatedAfter $ (UTCTime (ModifiedJulianDay 88000) (secondsToDiffTime 10)))
        _ <- shipwireTest config manager $ cancelReceiving receivingId
        _ <- shipwireTest config manager $ retireProducts $ ProductsToRetire [productId]
        result `shouldSatisfy` isRight

    describe "create a new receiving" $ do
      it "creates a new receiving with optional args" $ do
        randomPart <- getTimestamp
        (_, productId, productSku) <- createBaseProductHelper config manager $ exampleCreateBaseProduct randomPart
        (receiving, receivingId) <- createReceivingHelper config manager $ exampleCreateReceiving productSku
        receiving `shouldSatisfy` isRight
        _ <- shipwireTest config manager $ cancelReceiving receivingId
        _ <- shipwireTest config manager $ retireProducts $ ProductsToRetire [productId]
        let Right GenericResponse {..} = receiving
        genericResponseErrors `shouldBe` Nothing
        genericResponseWarnings `shouldBe` Nothing

      it "doesn't create a receiving with bad JSON" $ do
        randomPart <- getTimestamp
        (_, productId, _) <- createBaseProductHelper config manager $ exampleCreateBaseProduct randomPart
        result <- shipwireTest config manager $ createReceiving exampleBadCreateReceiving
        _ <- shipwireTest config manager $ retireProducts $ ProductsToRetire [productId]
        let Right GenericResponse {..} = result
        genericResponseErrors `shouldBe`
          Just
            (ResponseErrors
               [ Error
                   (ErrorCodeText "orderSubmitFailed")
                   (ErrorMessage
                      "Item quantity too low, please insert a quantity greater than 2.")
                   ErrorError
               ])

    describe "get information about a receiving" $ do
      it "gets info about a receiving" $ do
        randomPart <- getTimestamp
        (_, productId, productSku) <- createBaseProductHelper config manager $ exampleCreateBaseProduct randomPart
        (_, receivingId) <- createReceivingHelper config manager $ exampleCreateReceiving productSku
        result <- shipwireTest config manager $ getReceiving receivingId -&- (ExpandReceivingsParam [ExpandHolds, ExpandItems])
        result `shouldSatisfy` isRight
        _ <- shipwireTest config manager $ cancelReceiving receivingId
        _ <- shipwireTest config manager $ retireProducts $ ProductsToRetire [productId]
        let Right GenericResponse {..} = result
        genericResponseErrors `shouldBe` Nothing
        genericResponseWarnings `shouldBe` Nothing

    describe "modify information about a receiving" $ do
      it "modifies info about a receiving" $ do
        randomPart <- getTimestamp
        (_, productId, productSku) <- createBaseProductHelper config manager $ exampleCreateBaseProduct randomPart
        (_, receivingId) <- createReceivingHelper config manager $ exampleCreateReceiving productSku
        result <- shipwireTest config manager $ modifyReceiving receivingId exampleModifiedReceiving
        result `shouldSatisfy` isRight
        let Right modifyReceivingResponse = result
        genericResponseErrors modifyReceivingResponse `shouldBe` Nothing
        genericResponseWarnings modifyReceivingResponse `shouldBe` Nothing
        modifiedReceiving <- shipwireTest config manager $ getReceiving receivingId
        _ <- shipwireTest config manager $ cancelReceiving receivingId
        _ <- shipwireTest config manager $ retireProducts $ ProductsToRetire [productId]
        let Right GenericResponse {..} = modifiedReceiving
            Just ReceivingsItemResource {..} = genericResponseResource
            ItemResourceShipFrom {..} = rirShipFrom
            ItemResourceShipFromResource {..} = irsfResource
        irsfrCountry `shouldBe` Just (Country "Modified Country")

    describe "cancel a receiving" $ do
      it "cancels a receiving" $ do
        randomPart <- getTimestamp
        (_, productId, productSku) <- createBaseProductHelper config manager $ exampleCreateBaseProduct randomPart
        (_, receivingId) <- createReceivingHelper config manager $ exampleCreateReceiving productSku
        result <- shipwireTest config manager $ cancelReceiving receivingId
        result `shouldSatisfy` isRight
        _ <- shipwireTest config manager $ cancelReceiving receivingId
        _ <- shipwireTest config manager $ retireProducts $ ProductsToRetire [productId]
        let Right SimpleResponse {..} = result
        message `shouldBe` (ResponseMessage "Receiving was cancelled")

    describe "cancel shipping labels" $ do
      it "cancels shipping labels on a receiving" $ do
        randomPart <- getTimestamp
        (_, productId, productSku) <- createBaseProductHelper config manager $ exampleCreateBaseProduct randomPart
        (_, receivingId) <- createReceivingHelper config manager $ exampleCreateReceiving productSku
        result <- shipwireTest config manager $ cancelReceivingLabels receivingId
        result `shouldSatisfy` isRight
        _ <- shipwireTest config manager $ cancelReceiving receivingId
        _ <- shipwireTest config manager $ retireProducts $ ProductsToRetire [productId]
        let Right SimpleResponse {..} = result
        message `shouldBe` (ResponseMessage "Labels cancelled")

    describe "get list of holds for a receiving" $ do
      it "gets a list of holds for a receiving" $ do
        randomPart <- getTimestamp
        (_, productId, productSku) <- createBaseProductHelper config manager $ exampleCreateBaseProduct randomPart
        (_, receivingId) <- createReceivingHelper config manager $ exampleCreateReceiving productSku
        result <- shipwireTest config manager $ getReceivingHolds receivingId -&- IncludeCleared
        result `shouldSatisfy` isRight
        _ <- shipwireTest config manager $ cancelReceiving receivingId
        _ <- shipwireTest config manager $ retireProducts $ ProductsToRetire [productId]
        let Right GenericResponse {..} = result
        genericResponseWarnings `shouldBe` Nothing
        genericResponseErrors `shouldBe` Nothing

    describe "get email recipients and instructions for a receiving" $ do
      it "gets email recipients and instructions for a receiving" $ do
        randomPart <- getTimestamp
        (_, productId, productSku) <- createBaseProductHelper config manager $ exampleCreateBaseProduct randomPart
        (_, receivingId) <- createReceivingHelper config manager $ exampleCreateReceiving productSku
        result <- shipwireTest config manager $ getReceivingInstructionsRecipients receivingId
        result `shouldSatisfy` isRight
        _ <- shipwireTest config manager $ cancelReceiving receivingId
        _ <- shipwireTest config manager $ retireProducts $ ProductsToRetire [productId]
        let Right GenericResponse {..} = result
        genericResponseWarnings `shouldBe` Nothing
        genericResponseErrors `shouldBe` Nothing

    describe "get contents of a receiving" $ do
      it "gets contents of a receiving" $ do
        randomPart <- getTimestamp
        (_, productId, productSku) <- createBaseProductHelper config manager $ exampleCreateBaseProduct randomPart
        (_, receivingId) <- createReceivingHelper config manager $ exampleCreateReceiving productSku
        result <- shipwireTest config manager $ getReceivingItems receivingId
        result `shouldSatisfy` isRight
        _ <- shipwireTest config manager $ cancelReceiving receivingId
        _ <- shipwireTest config manager $ retireProducts $ ProductsToRetire [productId]
        let Right GenericResponse {..} = result
        genericResponseWarnings `shouldBe` Nothing
        genericResponseErrors `shouldBe` Nothing

    describe "get shipping dimension and container information" $ do
      it "gets shipping dimension and container infromation" $ do
        randomPart <- getTimestamp
        (_, productId, productSku) <- createBaseProductHelper config manager $ exampleCreateBaseProduct randomPart
        (_, receivingId) <- createReceivingHelper config manager $ exampleCreateReceiving productSku
        result <- shipwireTest config manager $ getReceivingShipments receivingId
        result `shouldSatisfy` isRight
        _ <- shipwireTest config manager $ cancelReceiving receivingId
        _ <- shipwireTest config manager $ retireProducts $ ProductsToRetire [productId]
        let Right GenericResponse {..} = result
        genericResponseWarnings `shouldBe` Nothing
        genericResponseErrors `shouldBe` Nothing

    describe "get tracking information for a receiving" $ do
      it "gets tracking information for a receiving" $ do
        randomPart <- getTimestamp
        (_, productId, productSku) <- createBaseProductHelper config manager $ exampleCreateBaseProduct randomPart
        (_, receivingId) <- createReceivingHelper config manager $ exampleCreateReceiving productSku
        result <- shipwireTest config manager $ getReceivingTrackings receivingId
        result `shouldSatisfy` isRight
        _ <- shipwireTest config manager $ cancelReceiving receivingId
        _ <- shipwireTest config manager $ retireProducts $ ProductsToRetire [productId]
        let Right GenericResponse {..} = result
        genericResponseWarnings `shouldBe` Nothing
        genericResponseErrors `shouldBe` Nothing

    describe "get labels information for a receiving" $ do
      it "gets labels information for a receiving" $ do
        randomPart <- getTimestamp
        (_, productId, productSku) <- createBaseProductHelper config manager $ exampleCreateBaseProduct randomPart
        (_, receivingId) <- createReceivingHelper config manager $ exampleCreateReceiving productSku
        result <- shipwireTest config manager $ getReceivingLabels receivingId
        result `shouldSatisfy` isRight
        _ <- shipwireTest config manager $ cancelReceiving receivingId
        _ <- shipwireTest config manager $ retireProducts $ ProductsToRetire [productId]
        let Right GenericResponse {..} = result
        genericResponseWarnings `shouldBe` Nothing
        genericResponseErrors `shouldBe` Nothing

    describe "get an itemized list of products" $ do
      it "gets an itemized list of products" $ do
        randomPart <- getTimestamp
        (_, productId, _) <- createBaseProductHelper config manager $ exampleCreateBaseProduct randomPart
        result <- shipwireTest config manager $ getProducts
        result `shouldSatisfy` isRight
        _ <- shipwireTest config manager $ retireProducts $ ProductsToRetire [productId]
        let Right GetProductsResponse {..} = result
        gprWarnings `shouldBe` Nothing
        gprErrors `shouldBe` Nothing

    describe "create a product" $ do
      it "creates all possible product classifications" $ do
        randomPart <- getTimestamp
        (prd, productId, _) <- createBaseProductHelper config manager $ exampleCreateBaseProduct randomPart
        prd `shouldSatisfy` isRight
        let Right GetProductsResponse {..} = prd
        _ <- shipwireTest config manager $ retireProducts $ ProductsToRetire [productId]
        gprWarnings `shouldBe` Nothing
        gprErrors `shouldBe` Nothing

    describe "modify products" $ do
      it "modifies several previously created products" $ do
        randomPart <- getTimestamp
        (_, productId, productSku) <- createBaseProductHelper config manager $ exampleCreateBaseProduct randomPart
        result <- shipwireTest config manager $ modifyProducts $ exampleModifyProducts productId productSku
        result `shouldSatisfy` isRight
        let Right GenericResponse {..} = result
        _ <- shipwireTest config manager $ retireProducts $ ProductsToRetire [productId]
        genericResponseWarnings `shouldBe` Nothing
        genericResponseErrors `shouldBe` Nothing

    describe "modify a product" $ do
      it "modifies a single product" $ do
        randomPart <- getTimestamp
        (_, productId, productSku) <- createBaseProductHelper config manager $ exampleCreateBaseProduct randomPart
        result <- shipwireTest config manager $ modifyProduct (exampleModifyProduct productId productSku) productId
        result `shouldSatisfy` isRight
        let Right GenericResponse {..} = result
        _ <- shipwireTest config manager $ retireProducts $ ProductsToRetire [productId]
        genericResponseWarnings `shouldBe` Nothing
        genericResponseErrors `shouldBe` Nothing

    describe "get a product" $ do
      it "gets information about a single product" $ do
        randomPart <- getTimestamp
        (_, productId, _) <- createBaseProductHelper config manager $ exampleCreateBaseProduct randomPart
        result <- shipwireTest config manager $ getProduct productId
        result `shouldSatisfy` isRight
        _ <- shipwireTest config manager $ retireProducts $ ProductsToRetire [productId]
        let Right GenericResponse {..} = result
        genericResponseStatus `shouldNotBe` (ResponseStatus 404)
        genericResponseWarnings `shouldBe` Nothing
        genericResponseErrors `shouldBe` Nothing

    describe "retire a product" $ do
      it "retires a product" $ do
        randomPart <- getTimestamp
        (_, productId, _) <- createBaseProductHelper config manager $ exampleCreateBaseProduct randomPart
        result <- shipwire config $ retireProducts $ ProductsToRetire [productId]
        let Right RetireProductsResponse {..} = result
            MoreInfo {..} = fromJust $ rprMoreInfo
            MoreInfoItems {..} = last miItems
            MoreInfoItem {..} = last miiItems
            status@Status {..} = miiStatus
        result `shouldSatisfy` isRight
        status `shouldBe` Status "deprecated"

    describe "create an order" $ do
      it "creates an order" $ do
        randomPart <- getTimestamp
        (_, productId, productSku) <- createBaseProductHelper config manager $ exampleCreateBaseProduct randomPart
        (result, orderId) <- createOrderHelper config manager $ exampleOrder randomPart productSku
        _ <- shipwireTest config manager $ cancelOrder $ WrappedId $ orderId
        _ <- shipwireTest config manager $ retireProducts $ ProductsToRetire [productId]
        result `shouldSatisfy` isRight
        let Right GenericResponse {..} = result
        genericResponseWarnings `shouldBe` Nothing
        genericResponseErrors `shouldBe` Nothing

    describe "get orders" $ do
      it "gets all the orders" $ do
        randomPart <- getTimestamp
        (_, productId, productSku) <- createBaseProductHelper config manager $ exampleCreateBaseProduct randomPart
        (_, orderId) <- createOrderHelper config manager $ exampleOrder randomPart productSku
        result <- shipwireTest config manager $ getOrders
        _ <- shipwireTest config manager $ cancelOrder $ WrappedId $ orderId
        _ <- shipwireTest config manager $ retireProducts $ ProductsToRetire [productId]
        result `shouldSatisfy` isRight
        let Right GenericResponse {..} = result
        genericResponseWarnings `shouldBe` Nothing
        genericResponseErrors `shouldBe` Nothing

    describe "get an order" $ do
      it "gets an information about an order" $ do
        randomPart <- getTimestamp
        (_, productId, productSku) <- createBaseProductHelper config manager $ exampleCreateBaseProduct randomPart
        (_, orderId) <- createOrderHelper config manager $ exampleOrder randomPart productSku
        result <- shipwireTest config manager $ getOrder $ WrappedId $ orderId
        _ <- shipwireTest config manager $ cancelOrder $ WrappedId $ orderId
        _ <- shipwireTest config manager $ retireProducts $ ProductsToRetire [productId]
        result `shouldSatisfy` isRight
        let Right GenericResponse {..} = result
        genericResponseWarnings `shouldBe` Nothing
        genericResponseErrors `shouldBe` Nothing

    describe "cancel an order" $ do
      it "cancels an order" $ do
        randomPart <- getTimestamp
        (_, productId, productSku) <- createBaseProductHelper config manager $ exampleCreateBaseProduct randomPart
        (_, orderId) <- createOrderHelper config manager $ exampleOrder randomPart productSku
        result <- shipwireTest config manager $ cancelOrder $ WrappedId $ orderId
        _ <- shipwireTest config manager $ retireProducts $ ProductsToRetire [productId]
        result `shouldSatisfy` isRight
        let Right SimpleResponse {..} = result
        warnings `shouldBe` Nothing
        errors `shouldBe` Nothing
        message `shouldBe` (ResponseMessage "Order cancelled")

    describe "get tracking information for this order" $ do
      it "gets tracking information for this order" $ do
        randomPart <- getTimestamp
        (_, productId, productSku) <- createBaseProductHelper config manager $ exampleCreateBaseProduct randomPart
        (_, orderId) <- createOrderHelper config manager $ exampleOrder randomPart productSku
        result <- shipwireTest config manager $ getOrderTrackings $ WrappedId $ orderId
        _ <- shipwireTest config manager $ cancelOrder $ WrappedId $ orderId
        _ <- shipwireTest config manager $ retireProducts $ ProductsToRetire [productId]
        result `shouldSatisfy` isRight
        let Right GenericResponse {..} = result
        genericResponseWarnings `shouldBe` Nothing
        genericResponseErrors `shouldBe` Nothing

    describe "validate address" $ do
      it "validates an address" $ do
        result <- shipwireTest config manager $ validateAddress exampleAddress
        result `shouldSatisfy` isRight
        let Right ValidateAddressResponse {..} = result
        varMessage `shouldBe` (ResponseMessage "The address provided is valid")
        varWarnings `shouldBe` Nothing
        varErrors `shouldBe` Nothing
