{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-| Export the Contact Information for Published Listings by their Zipcode.
-}
import           Control.Monad                  ( when )
import           Data.Csv                       ( ToNamedRecord
                                                , DefaultOrdered
                                                )
import qualified Data.List                     as L
import qualified Data.Map.Strict               as M
import           Data.Maybe                     ( mapMaybe
                                                , fromMaybe
                                                )
import qualified Data.Text                     as T
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
                                                , details
                                                , def
                                                , args
                                                , typ
                                                )
import           System.Exit                    ( exitFailure )

import qualified DB
import           DB                             ( FormItem
                                                , Post
                                                , runDB
                                                , getBestCommunityName
                                                , getListings
                                                , getAddressMetas
                                                )
import           Export                         ( toCsvFile )

main :: IO ()
main = do
    locations <- mapMaybe makeLocation . filters <$> cmdArgs argSpec
    when (null locations) $ do
        putStrLn "Error: At least one ZIPCODE:ZIPCODE location is required."
        exitFailure
    listings <- runDB $ filter (filterListings locations) <$> getListings
    let exportRows = map listingToData listings
    toCsvFile (nameWithLocationTags "directory-zipcode-export" locations)
              exportRows


data Args
    = Args
        { filters :: [String]
        }
    deriving (Data, Typeable, Generic)

argSpec :: Args
argSpec =
    Args {filters = def &= args &= typ "ZIPCODE:ZIPCODE"}
        &= program "directory-zipcode-export"
        &= summary "Directory - Export by Zipcode"
        &= help
               "Export the contact information for Listings in the given Zipcodes."
        &= details
               [ "Expected arguments are numeric zipcode ranges seperated by colons."
               , "e.g., directory-zipcode-export 90001:93999"
               ]


data Location
    = Location
        { startZip :: Text
        , endZip :: Text
        } deriving (Eq, Show)

makeLocation :: String -> Maybe Location
makeLocation raw = case T.split (== ':') (T.pack raw) of
    [startZip, endZip] -> Just $ Location {..}
    _                  -> Nothing

nameWithLocationTags :: Text -> [Location] -> Text
nameWithLocationTags basename locations =
    let tagList =
            L.nub $ map (\l -> startZip l <> "_to_" <> endZip l) locations
        tags = if null tagList then "" else "-" <> T.intercalate "-" tagList
    in  basename <> tags <> ".csv"


filterListings
    :: [Location] -> (Entity FormItem, M.Map Int Text, Maybe Post) -> Bool
filterListings locations (_, metas, _)
    = let
          address = getAddressMetas metas
          matchingLocations =
              flip filter locations
                  $ \l ->
                        (DB.zipCode address >= startZip l)
                            && (DB.zipCode address <= endZip l)
      in
          not $ null matchingLocations


listingToData :: (Entity FormItem, M.Map Int Text, Maybe Post) -> ExportData
listingToData l@(_, metas, _) =
    let address = getAddressMetas metas
    in  ExportData
            { communityName = getBestCommunityName l
            , city          = DB.city address
            , state         = DB.state address
            , zipCode       = DB.zipCode address
            , contactName   = fromMaybe "" $ M.lookup 202 metas
            , contactPhone  = fromMaybe "" $ M.lookup contactPhoneFieldId metas
            , contactEmail  = fromMaybe "" $ M.lookup 199 metas
            }

contactPhoneFieldId :: Int
contactPhoneFieldId = 201

data ExportData =
    ExportData
        { communityName :: Text
        , city :: Text
        , state :: Text
        , zipCode :: Text
        , contactName :: Text
        , contactPhone :: Text
        , contactEmail :: Text
        } deriving (Show, Generic)
instance ToNamedRecord ExportData
instance DefaultOrdered ExportData
