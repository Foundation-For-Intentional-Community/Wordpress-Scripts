{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
import           Control.Applicative            ( (<|>) )
import           Control.Monad                  ( forM )
import           Data.Csv                       ( ToNamedRecord
                                                , DefaultOrdered
                                                )
import           Data.Maybe                     ( mapMaybe
                                                , fromMaybe
                                                )
import           Data.Text                      ( Text )
import           Database.Persist               ( Entity(..) )
import           GHC.Generics                   ( Generic )
import           System.Console.CmdArgs.Implicit
                                                ( (&=)
                                                , Data
                                                , Typeable
                                                , cmdArgs
                                                , program
                                                , help
                                                , summary
                                                , def
                                                , args
                                                , argPos
                                                , typ
                                                )

import           DB                             ( runDB
                                                , User(..)
                                                , getBestUserName
                                                , Order(..)
                                                , getOrders
                                                , getOrderCustomer
                                                , orderHasProductOrVariant
                                                , decodeSerializedOrderItems
                                                )
import           Export                         ( toCsvFile
                                                , nameWithProductTags
                                                )

import qualified Data.List                     as L
import qualified Data.Map.Strict               as M
import qualified Data.Text                     as T


main :: IO ()
main = do
    productIds <- (\a -> map T.pack $ initialProduct a : products a)
        <$> cmdArgs argSpec
    ordersAndUsers <- runDB $ do
        orders <- filterOrders productIds <$> getOrders
        forM orders $ \o -> (o, ) <$> getOrderCustomer (orderPostMetas o)
    toCsvFile
            (nameWithProductTags "order-contact-export" productIds [] <> ".csv")
        . L.nubBy (\x y -> email x == email y)
        . filter (\d -> email d /= "" && country d /= "")
        $ map orderToData ordersAndUsers

filterOrders :: [Text] -> [Order] -> [Order]
filterOrders productIds = filter
    (\order ->
        let productIdsFromPostMeta =
                    mapMaybe (M.lookup "id")
                        $ maybe [] decodeSerializedOrderItems
                        $ M.lookup "_order_items" (orderPostMetas order)
        in  not (null (productIdsFromPostMeta `L.intersect` productIds))
                || orderHasProductOrVariant productIds [] (orderLineItems order)
    )


-- CLI

data Args
    = Args
        { initialProduct :: String
        , products :: [String]
        }
    deriving (Generic, Data, Typeable)

argSpec :: Args
argSpec =
    Args { initialProduct = def &= argPos 0 &= typ "PRODUCT_ID"
         , products       = def &= args &= typ "PRODUCT_IDS"
         }
        &= program "order-contact-export"
        &= summary "WooCommerce - Order Contact Export"
        &= help
               "Export the name, email, & country for every Order containing the given products."


-- Export

data ExportData
    = ExportData
        { name :: Text
        , email :: Text
        , country :: Text
        } deriving (Show, Generic)

instance ToNamedRecord ExportData
instance DefaultOrdered ExportData

orderToData :: (Order, Maybe (Entity User, M.Map Text Text)) -> ExportData
orderToData (o, maybeUser) =
    let customerName =
                fromMaybe ""
                    $   maybeFirstLast "_billing_"
                    <|> maybeFirstLast "_shipping_"
                    <|> fmap getBestUserName maybeUser
        customerEmail =
                fromMaybe ""
                    $   getMeta "_billing_email"
                    <|> getMeta "_shipping_email"
                    <|> fmap (userEmail . entityVal . fst) maybeUser
    in  ExportData { name    = customerName
                   , email   = customerEmail
                   , country = fromMaybe "" $ getMeta "_billing_country"
                   }
  where
    getMeta k = M.lookup k $ orderPostMetas o
    maybeFirstLast prefix =
        (,)
            <$> getMeta (prefix <> "first_name")
            <*> getMeta (prefix <> "last_name")
            >>= \(f, l) -> if not (T.null f || T.null l)
                    then Just (f <> " " <> l)
                    else Nothing
