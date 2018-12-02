{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
import           Data.Csv                       ( ToNamedRecord
                                                , DefaultOrdered
                                                )
import           Data.Maybe                     ( fromMaybe )
import           Data.Text                      ( Text )
import           Database.Persist               ( Entity(..) )
import           Database.Persist.Sql           ( fromSqlKey )
import           GHC.Generics                   ( Generic )

import           DB                             ( Post(..)
                                                , FormItem(..)
                                                , runDB
                                                , getListings
                                                )
import           Export                         ( toCsvFile )

import qualified Data.Map.Strict               as M

main :: IO ()
main = map listingToData . filterListings <$> runDB getListings >>= toCsvFile
    "directory-address-export.csv"
  where
    filterListings = filter $ \(_, metaMap, post) ->
        (M.lookup 424 metaMap == Just "United States")
            && (M.lookup 933 metaMap /= Just "Yes")
            && (fmap postStatus post == Just "publish")

data ExportData =
  ExportData
    { listing :: Int
    , contact :: Text
    , name :: Text
    , addressOne :: Text
    , addressTwo :: Text
    , city :: Text
    , state :: Text
    , zipCode :: Text
    , country :: Text
    } deriving (Show, Generic)

instance ToNamedRecord ExportData
instance DefaultOrdered ExportData

listingToData :: (Entity FormItem, M.Map Int Text, Maybe Post) -> ExportData
listingToData (Entity itemId i, metaMap, post) =
    let listingName = case post of
            Just p  -> postTitle p
            Nothing -> formItemName i
    in  ExportData
            { listing    = fromIntegral $ fromSqlKey itemId
            , contact    = getMeta 202
            , name       = listingName
            , addressOne = getMeta 425
            , addressTwo = getMeta 426
            , city       = getMeta 427
            , state      = getMeta 815
            , zipCode    = getMeta 429
            , country    = getMeta 424
            }
    where getMeta f = fromMaybe "" $ M.lookup f metaMap
