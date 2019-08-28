{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-| Export the Contact Info & Member/Visitor Size for Published Listings
-}
import           Data.Csv                       ( ToNamedRecord
                                                , DefaultOrdered
                                                )
import           Data.Maybe                     ( fromMaybe )
import           Data.Text                      ( Text
                                                , pack
                                                , unpack
                                                )
import           Data.Time                      ( formatTime
                                                , defaultTimeLocale
                                                )
import           Database.Persist               ( Entity(..) )
import           Database.Persist.Sql           ( fromSqlKey )
import           GHC.Generics                   ( Generic )
import           System.Console.CmdArgs.Implicit
                                                ( (&=)
                                                , cmdArgs
                                                , program
                                                , help
                                                , summary
                                                )
import           Text.Read                      ( readMaybe )

import qualified DB
import           DB                             ( Post(..)
                                                , FormItem(..)
                                                , runDB
                                                , getListings
                                                , getBestCommunityName
                                                , getAddressMetas
                                                )
import           Export                         ( toCsvFile )

import qualified Data.Map.Strict               as M


main :: IO ()
main = do
    cmdArgs args
    runDB getListings
        >>= toCsvFile "directory-contact-size-export.csv"
        .   map listingToData


args :: ()
args =
    ()
        &= program "directory-contact-size-export"
        &= summary "Directory - Contact & Size Export"
        &= help
               "Export the Contact Info & Membership/Visitor size of Published Communities."


data ExportData =
    ExportData
        { listing :: Int
        , communityName :: Text
        , membershipSize :: Int
        , guestSize :: Text
        , formedDate :: Text
        , createdDate :: Text
        , state :: Text
        , country :: Text
        , stage :: Text
        , contactPerson :: Text
        , contactPhone :: Text
        , contactEmail :: Text
        } deriving (Show, Generic)

instance ToNamedRecord ExportData
instance DefaultOrdered ExportData


listingToData :: (Entity FormItem, M.Map Int Text, Maybe Post) -> ExportData
listingToData l@(Entity itemId i, metaMap, _) = ExportData
    { listing        = fromIntegral $ fromSqlKey itemId
    , communityName  = getBestCommunityName l
    , membershipSize = getSize 254 420
    , guestSize      = getMeta 255
    , formedDate     = getMeta 274
    , createdDate    = pack . renderDate $ formItemCreatedAt i
    , state          = DB.state addressMetas
    , country        = DB.country addressMetas
    , stage          = getMeta 291
    , contactPerson  = getMeta 202
    , contactPhone   = getMeta 201
    , contactEmail   = getMeta 199
    }
  where
    getMeta f = fromMaybe "" $ M.lookup f metaMap
    renderDate utc = formatTime defaultTimeLocale "%F" utc
    getSize adultFieldId childFieldId =
        (fromMaybe 0 . readMaybe . unpack $ getMeta adultFieldId)
            + (fromMaybe 0 . readMaybe . unpack $ getMeta childFieldId)
    addressMetas = getAddressMetas metaMap
