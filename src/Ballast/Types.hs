{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Ballast.Types
       ( Username(..)
       , Password(..)
       , RateResponse(..)
       , defaultRate
       ) where

import           Control.Monad              (mzero)
import           Data.Aeson
import           Data.Aeson.Types
import           Data.ByteString            (ByteString)
import qualified Data.ByteString            as BS
import qualified Data.ByteString.Lazy.Char8 as BSL
import qualified Data.Char                  as DC
import           Data.Text                  (Text)
import qualified Data.Text                  as T
import           GHC.Generics

-- | Username type used for HTTP Basic authentication.
newtype Username = Username { username :: ByteString } deriving (Read, Show, Eq)

-- | Password type used for HTTP Basic authentication.
newtype Password = Password { password :: ByteString } deriving (Read, Show, Eq)

newtype SKU =
  SKU { unSku :: Text }
  deriving (Eq, Generic, Show)

mkSku :: Text -> Maybe SKU
mkSku sku
  | T.length sku > 16 = Nothing
  | T.length sku < 1 = Nothing
  | otherwise = Just (SKU sku)

instance ToJSON SKU where
  toJSON sku =
    genericToJSON options sku
    where
      options =
        defaultOptions { unwrapUnaryRecords = True }

-- max 16 characters
-- haskellbookskuty

data Rate =
  Rate {
    rateOptions :: RateOptions
  , rateOrder   :: RateOrder
  } deriving (Eq, Generic, Show)

instance ToJSON Rate where
  toJSON rate =
    genericToJSON options rate
    where
      options =
        defaultOptions { fieldLabelModifier = downcaseHead . drop 4
                       , omitNothingFields = True
                       }

data RateOptions =
  RateOptions {
    rateOptionCurrency      :: Currency
  , rateOptionGroupBy       :: GroupBy
  , rateOptionCanSplit      :: Integer
  , rateOptionWarehouseArea :: WarehouseArea
  , rateOptionChannelName   :: Maybe Text
  } deriving (Eq, Generic, Show)

data RateOrder =
  RateOrder {
    rateOrderShipTo :: ShipTo
  , rateOrderItems  :: Items
  } deriving (Eq, Generic, Show)

instance ToJSON RateOrder where
  toJSON order =
    genericToJSON options order
    where
      options =
        defaultOptions { fieldLabelModifier = downcaseHead . drop 9
                       , omitNothingFields = True
                       }

data ShipTo =
  ShipTo {
    shipToAddress1     :: AddressLine
  , shipToAddress2     :: AddressLine
  , shipToAddress3     :: AddressLine
  , shipToCity         :: City
  , shipToPostalCode   :: PostalCode
  , shipToRegion       :: Region
  , shipToCountry      :: Country
  , shipToIsCommercial :: Integer
  , shipToIsPoBox      :: Integer
  } deriving (Eq, Generic, Show)

type Items = [ItemInfo]
data ItemInfo = ItemInfo (SKU, Quantity) deriving (Eq, Show)

instance ToJSON ItemInfo where
  toJSON (ItemInfo (sku, q)) = object ["sku" .= sku, "quantity" .= q]

type Quantity = Integer

instance ToJSON ShipTo where
  toJSON shipto =
    genericToJSON options shipto
    where
      options =
        defaultOptions { fieldLabelModifier = downcaseHead . drop 6
                       , omitNothingFields = True
                       }

defaultRate = Rate defaultRateOptions defaultRateOrder

defaultRateOrder = RateOrder defaultShipTo defaultItems

defaultItems = [ItemInfo ((SKU "Ballasttest"), 1)]

defaultShipTo =
  ShipTo (AddressLine "6501 Railroad Avenue SE") (AddressLine "Room 315")
         (AddressLine "") (City "Snoqualmie")
         (PostalCode "85283") (Region "WA") (Country "US")
         1 0

newtype AddressLine =
  AddressLine { unAddressLine :: Text }
  deriving (Eq, Generic, Show)

instance ToJSON AddressLine where
  toJSON address =
    genericToJSON options address
    where
      options =
        defaultOptions { unwrapUnaryRecords = True }

newtype City =
  City { unCity :: Text }
  deriving (Eq, Generic, Show)

instance ToJSON City where
  toJSON city =
    genericToJSON options city
    where
      options =
        defaultOptions { unwrapUnaryRecords = True }

newtype PostalCode =
  PostalCode { unPostalCode :: Text }
  deriving (Eq, Generic, Show)

instance ToJSON PostalCode where
  toJSON code =
    genericToJSON options code
    where
      options =
        defaultOptions { unwrapUnaryRecords = True }

newtype Region =
  Region { unRegion :: Text }
  deriving (Eq, Generic, Show)

instance ToJSON Region where
  toJSON region =
    genericToJSON options region
    where
      options =
        defaultOptions { unwrapUnaryRecords = True }

newtype Country =
  Country { unCountry :: Text }
  deriving (Eq, Generic, Show)

instance ToJSON Country where
  toJSON country =
    genericToJSON options country
    where
      options =
        defaultOptions { unwrapUnaryRecords = True }

downcaseHead :: [Char] -> [Char]
downcaseHead [] = []
downcaseHead (x:xs) =
  (DC.toLower x) : xs

instance ToJSON RateOptions where
  toJSON ro =
    genericToJSON options ro
    where
      options =
        defaultOptions { fieldLabelModifier = downcaseHead . drop 10
                       , omitNothingFields = True
                       }

defaultRateOptions =
  RateOptions USD GroupByAll 1 WarehouseAreaUS Nothing

data Currency =
  USD
  deriving (Eq, Show)

instance ToJSON Currency where
  toJSON USD = String "USD"

tshow :: Show a => a -> Text
tshow = T.pack . show

omitNulls :: [(Text, Value)] -> Value
omitNulls = object . filter notNull where
  notNull (_, Null)      = False
  -- notNull (_, Array a) = (not . V.null) a
  notNull _              = True

data GroupBy =
      GroupByAll
    | GroupByWarehouse
    deriving (Eq, Generic, Show)

instance ToJSON GroupBy where
  toJSON GroupByAll = String "all"
  toJSON GroupByWarehouse = String "warehouse"

instance FromJSON GroupBy where
  parseJSON = withText "groupBy" parse
    where parse "all" = pure GroupByAll
          parse "warehouse" = pure GroupByWarehouse

data WarehouseArea =
  WarehouseAreaUS
  deriving (Eq, Show)

instance ToJSON WarehouseArea where
  toJSON WarehouseAreaUS = String "US"

-- defaultRateResponse :: IO RateResponse
-- defaultRateResponse = do
--   file <- BSL.readFile "test4.json"
--   let decoded = eitherDecode file
--   case decoded of
--     Right info -> return info
--     Left err -> error err

data RateResponse =
  RateResponse {
    rateResponseStatus           :: Integer
  , rateResponseMessage          :: Text
  , rateResponseWarnings         :: Maybe [Warnings]
  , rateResponseResourceLocation :: Maybe Text
  , rateResponseResource         :: Resource
  } deriving (Eq, Generic, Show)

instance FromJSON RateResponse where
  parseJSON rr =
    genericParseJSON options rr
    where
      options =
        defaultOptions { fieldLabelModifier = downcaseHead . drop 12 }

data Warnings =
  Warnings {
    warningsCode    :: Text
  , warningsMessage :: Text
  , warningsType    :: WarningType
  } deriving (Eq, Generic, Show)

data WarningType =
    WarningsWarning
  | WarningsError
  deriving (Eq, Generic, Show)

instance FromJSON WarningType where
  parseJSON = withText "warningType" parse
    where parse "warning" = pure WarningsWarning
          parse "error" = pure WarningsError

instance FromJSON Warnings where
  parseJSON w =
    genericParseJSON options w
    where
      options =
        defaultOptions { fieldLabelModifier = downcaseHead . drop 8 }

data Resource =
  Resource {
    resourceGroupBy :: GroupBy
  -- , resourceRates   :: Rates
  } deriving (Eq, Generic, Show)

instance FromJSON Resource where
  parseJSON res =
    genericParseJSON options res
    where
      options =
        defaultOptions { fieldLabelModifier = downcaseHead . drop 8 }

data Rates =
  Rates {
  rates :: [ServiceOptions]
  } deriving (Eq, Generic, Show)

instance FromJSON Rates where
  parseJSON = genericParseJSON defaultOptions

data ServiceOptions =
  ServiceOptions {
  serviceOptions :: (Maybe [Object])
  } deriving (Eq, Generic, Show)

instance FromJSON ServiceOptions where
  parseJSON = genericParseJSON defaultOptions
