{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
import           Data.Csv                       ( ToNamedRecord
                                                , DefaultOrdered
                                                )
import           Data.Maybe                     ( mapMaybe
                                                , fromMaybe
                                                )
import           Data.Text                      ( Text )
import           Data.Time.Format               ( formatTime
                                                , defaultTimeLocale
                                                )
import           Database.Persist               ( Entity(..) )
import           Database.Persist.Sql           ( fromSqlKey )
import           GHC.Exts                       ( sortWith )
import           GHC.Generics                   ( Generic )
import           System.Console.CmdArgs.Implicit
                                                ( (&=)
                                                , cmdArgs
                                                , program
                                                , help
                                                , summary
                                                )

import           DB                             ( runDB
                                                , OrderItem(..)
                                                , Order(..)
                                                , getOrders
                                                , Post(..)
                                                , decodeSerializedOrderItems
                                                )
import           Export                         ( toCsvFile )

import qualified Data.Map.Strict               as M
import qualified Data.Text                     as T

main :: IO ()
main =
    cmdArgs args
        >>  sortWith date
        .   map orderToData
        <$> runDB getOrders
        >>= toCsvFile "full-order-export.csv"

args :: ()
args =
    ()
        &= program "full-order-billing-export"
        &= summary "WooCommerce - Full Order Billing Details Export"
        &= help
               "Export the date, name, email, country, & products of every Order."


data ExportData =
        ExportData
            { order :: Int
            , date :: Text
            , firstName :: Text
            , lastName :: Text
            , email :: Text
            , country :: Text
            , products :: Text
            } deriving (Show, Generic)

instance ToNamedRecord ExportData
instance DefaultOrdered ExportData

orderToData :: Order -> ExportData
orderToData o = ExportData
    { order     = fromIntegral . fromSqlKey . entityKey $ orderPost o
    , date      = T.pack
        . formatTime defaultTimeLocale "%F"
        . postDate
        . entityVal
        $ orderPost o
    , firstName = getMeta "_billing_first_name"
    , lastName  = getMeta "_billing_last_name"
    , email     = getMeta "_billing_email"
    , country   = getMeta "_billing_country"
    , products  = T.intercalate "; "
        $  map (orderItemName . entityVal . fst) (orderLineItems o)
        ++ getProductNames (orderPostMetas o)
    }
    where getMeta k = fromMaybe "" $ M.lookup k $ orderPostMetas o

getProductNames :: M.Map Text Text -> [Text]
getProductNames m =
    mapMaybe (M.lookup "name") $ maybe [] decodeSerializedOrderItems $ M.lookup
        "_order_items"
        m
