{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
import           Data.Csv                       ( ToNamedRecord
                                                , DefaultOrdered
                                                , encodeDefaultOrderedByName
                                                )
import           Data.Maybe                     ( maybeToList
                                                , mapMaybe
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

import           DB

import qualified Data.ByteString.Lazy          as LBS
import qualified Data.Map.Strict               as M
import qualified Data.Text                     as T

main :: IO ()
main =
    encodeDefaultOrderedByName
        .   sortWith date
        .   map orderToData
        <$> runDB getOrders
        >>= LBS.writeFile "full-order-export.csv"


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
    , products  = T.intercalate "; " $ orderLineItems o ++ getProductNames
        (orderPostMetas o)
    }
    where getMeta k = fromMaybe "" $ M.lookup k $ orderPostMetas o

getProductNames :: M.Map Text Text -> [Text]
getProductNames m =
    mapMaybe (\(key, val) -> if key == "name" then Just val else Nothing)
        $   concat
        .   maybeToList
        $   decodePHPStringArray
        <$> M.lookup "_order_items" m
