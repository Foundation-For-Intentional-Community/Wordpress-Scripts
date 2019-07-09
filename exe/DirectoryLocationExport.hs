{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-| Export the Contact Information for Published Listings by their Location.
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
        putStrLn "Error: At least one COUNTRY:STATE location is required."
        exitFailure
    listings <- runDB $ filter (filterListings locations) <$> getListings
    let exportRows = map listingToData listings
    toCsvFile (nameWithLocationTags "directory-location-export" locations)
              exportRows

data Args
    = Args
        { filters :: [String]
        }
    deriving (Generic, Data, Typeable)


argSpec :: Args
argSpec =
    Args {filters = def &= args &= typ "COUNTRY:STATE"}
        &= program "directory-location-export"
        &= summary "Directory - Location Export"
        &= help
               "Export the contact information for Listings in the State/Provinces"
        &= details
               [ "Expected arguments are 2-letter codes for Countries and full "
                   ++ "names for States/Provinces or asteriks for all State/Provinces."
               , "E.g., directory-location-export \"US:Maryland\" \"CA:British Columbia\" \"FR:*\""
               ]

data Location
    = Location
        { lCountryCode :: Text
        , lRegion :: Text
        } deriving (Eq, Show)

makeLocation :: String -> Maybe Location
makeLocation raw = case T.split (== ':') (T.pack raw) of
    [country, region] -> Just $ Location country region
    _                 -> Nothing

nameWithLocationTags :: Text -> [Location] -> Text
nameWithLocationTags baseName locations =
    let regionTag region = if region == "*" then "all" else region
        tagList = L.nub $ map
            (\l -> lCountryCode l <> "_" <> regionTag (lRegion l))
            locations
        tags = if null tagList then "" else "-" <> T.intercalate "-" tagList
    in  baseName <> tags <> ".csv"

filterListings
    :: [Location] -> (Entity FormItem, M.Map Int Text, Maybe Post) -> Bool
filterListings locations (_, metas, _)
    = let
          address           = getAddressMetas metas
          matchingLocations = flip filter locations $ \l ->
              (DB.country address == lCountryCode l)
                  && ((DB.state address == lRegion l) || (lRegion l == "*"))
      in
          not $ null matchingLocations

listingToData :: (Entity FormItem, M.Map Int Text, Maybe Post) -> ExportData
listingToData l@(_, metas, _) =
    let address = getAddressMetas metas
    in  ExportData
            { communityName = getBestCommunityName l
            , city          = DB.city address
            , state         = DB.state address
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
        , contactName :: Text
        , contactPhone :: Text
        , contactEmail :: Text
        } deriving (Show, Generic)
instance ToNamedRecord ExportData
instance DefaultOrdered ExportData
