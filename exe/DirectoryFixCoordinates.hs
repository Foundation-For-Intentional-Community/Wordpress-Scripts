{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{- | This script geocodes either the map address or contact address for
listings & saves the returned latitude & longitude to the item meta table.

Public contact addresses are geocoded with the entire address and private
ones use only the city, state, zip, & country.

There is a CLI flag to only update listings that were last updated in the
last 2 days and one to disable the creation of a CSV export containing
Geocoding API errors.

TODO: Email directory manager on failed geocodings?

-}
import           Control.Monad                  ( unless )
import           Control.Monad.Reader           ( MonadReader
                                                , MonadIO
                                                , ReaderT
                                                , runReaderT
                                                , asks
                                                , liftIO
                                                )
import           Data.Csv                       ( ToNamedRecord
                                                , DefaultOrdered
                                                )
import           Data.Maybe                     ( catMaybes )
import           Data.Text                      ( Text )
import           Data.Time                      ( Day
                                                , UTCTime(utctDay)
                                                , diffDays
                                                , getCurrentTime
                                                , formatTime
                                                , defaultTimeLocale
                                                )
import           Database.Persist.Sql           ( Entity(..)
                                                , (==.)
                                                , deleteWhere
                                                , fromSqlKey
                                                )
import           GHC.Generics                   ( Generic )
import           Network.HTTP.Client            ( Manager
                                                , newManager
                                                )
import           Network.HTTP.Client.TLS        ( tlsManagerSettings )
import           System.Console.CmdArgs.Implicit
                                                ( (&=)
                                                , Data
                                                , Typeable
                                                , cmdArgs
                                                , def
                                                , program
                                                , help
                                                , summary
                                                , name
                                                , explicit
                                                )
import           System.Environment             ( lookupEnv )
import           Web.Google.Geocoding           ( geocode
                                                , Key(..)
                                                , Address(..)
                                                , Geometry(..)
                                                , LatLng(..)
                                                , Result(..)
                                                , Status(..)
                                                , GeocodingResponse(..)
                                                )

import           DB                             ( runDB
                                                , getListings
                                                , AddressMetas(..)
                                                , getAddressMetas
                                                , getBestCommunityName
                                                , upsertItemMeta
                                                )
import           Export                         ( toCsvFile )
import           Schema                  hiding ( Key )

import qualified Data.Map.Strict               as M
import qualified Data.Text                     as T


main :: IO ()
main = do
    args        <- cmdArgs argsSpec
    env         <- Env <$> getApiKey <*> newManager tlsManagerSettings
    currentTime <- getCurrentTime
    let currentDay = utctDay currentTime
    errors <- flip runReaderT env . unScript $ do
        let filterListings = if updatedRecently args
                then filter (updatedInLastTwoDays currentDay)
                else id
        listingData <- liftIO $ filterListings <$> runDB getListings
        catMaybes <$> mapM fixCoords listingData
    unless (null errors || disableExport args) $ toCsvFile
        (  "geocoding-errors-"
        <> T.pack (formatTime defaultTimeLocale "%FT%H-%M-%S" currentTime)
        <> ".csv"
        )
        errors
  where
    getApiKey :: IO Key
    getApiKey = lookupEnv "MAPS_API_KEY" >>= \case
        Nothing ->
            error "You must define a `MAPS_API_KEY` environmental variable."
        Just key -> return . Key $ T.pack key
    updatedInLastTwoDays
        :: Day -> (Entity FormItem, M.Map Int Text, Maybe Post) -> Bool
    updatedInLastTwoDays currentDay (Entity _ item, _, _) =
        abs (diffDays currentDay $ utctDay $ formItemUpdatedAt item) <= 2


-- CLI Args

data Args
    = Args
        { updatedRecently :: Bool
        , disableExport :: Bool
        } deriving (Show, Data, Typeable)

argsSpec :: Args
argsSpec =
    Args
            { updatedRecently =
                def &= name "recently-updated" &= explicit &= help
                    "Only listings updated in the last 2 days"
            , disableExport   =
                def &= name "disable-export" &= explicit &= help
                    "Do not generate an export containing geocoding errors"
            }
        &= program "directory-fix-coordinates"
        &= summary "Directory - Address Geocoding"
        &= help "Geocode the map or contact addresses of listings."


-- Script Environment

newtype Script a
    = Script { unScript :: ReaderT Env IO a }
    deriving (Functor, Applicative, Monad, MonadIO, MonadReader Env)

data Env
    = Env
        { apiKey :: Key
        , manager :: Manager
        }


-- Listings Fields

isAddressPublicFieldId :: Int
isAddressPublicFieldId = 285
mapAddressFieldId :: Int
mapAddressFieldId = 683
latitudeFieldId :: Int
latitudeFieldId = 684
longitudeFieldId :: Int
longitudeFieldId = 685


-- Processing Functions

-- | Determine whether we should geocode, build the address & geocode if
-- necessary, removing any private coordinates if specified.
fixCoords
    :: (Entity FormItem, M.Map Int Text, Maybe Post)
    -> Script (Maybe GeocodingError)
fixCoords l@(Entity itemId _, metaMap, _) =
    let publicAddress = M.lookup isAddressPublicFieldId metaMap
        visibility = if publicAddress == Just "Public" then Public else Private
        mapAddress    = M.lookup mapAddressFieldId metaMap
        latitude      = M.lookup latitudeFieldId metaMap
        longitude     = M.lookup longitudeFieldId metaMap
        addressMetas  = getAddressMetas metaMap
    in  fmap makeError <$> case (latitude, longitude) of
            (Just _, Just _) ->
                geocodeAndSave itemId visibility mapAddress addressMetas
            (Nothing, Nothing) ->
                geocodeAndSave itemId visibility mapAddress addressMetas
            _ -> do
                deleteLatAndLong
                geocodeAndSave itemId visibility mapAddress addressMetas
  where
    -- Delete the Latitude & Longitude metas for the Listing if they exist.
    deleteLatAndLong :: Script ()
    deleteLatAndLong =
        liftIO
            .  runDB
            $  deleteWhere
                   [ FormItemMetaField ==. latitudeFieldId
                   , FormItemMetaItem ==. itemId
                   ]
            >> deleteWhere
                   [ FormItemMetaField ==. longitudeFieldId
                   , FormItemMetaItem ==. itemId
                   ]
    makeError :: (Text, Text) -> GeocodingError
    makeError (address, errorMessage) = GeocodingError
        { geCommunityName = getBestCommunityName l
        , geListingId     = fromIntegral $ fromSqlKey itemId
        , geAddress       = address
        , geErrorText     = errorMessage
        }


-- | Geocode the given address, returning the Coordiantes or the Error
-- Text.
geocodeAddress :: Text -> Script (Either Text LatLng)
geocodeAddress address = if address == ""
    then return $ Left "Address is blank."
    else do
        key    <- asks apiKey
        mgr    <- asks manager
        result <- liftIO $ geocode mgr
                                   key
                                   (Just $ Address address)
                                   Nothing
                                   Nothing
                                   Nothing
                                   Nothing
        case result of
            Right resp -> case (status resp, results resp) of
                (OK, res : _) -> return . Right . location $ geometry res
                (OK, []) ->
                    return
                        .  Left
                        $  "Address `"
                        <> address
                        <> "` returned no results"
                _ -> return . Left . T.pack $ "API Error Status: " ++ show resp
            _ -> return . Left . T.pack $ "API Error Result: " ++ show result



-- | Geocode either the Map Address or Contact Address for a Listing,
-- saving the coordinates to the database if successful.
geocodeAndSave
    :: FormItemId
    -> AddressVisibility
    -> Maybe Text
    -> AddressMetas
    -> Script (Maybe (Text, Text))
geocodeAndSave itemId visibility mapAddress addressMetas = do
    let address = case mapAddress of
            Nothing   -> buildAddress
            Just ""   -> buildAddress
            Just addr -> addr
    geocodeAddress address >>= \case
        Left  errorText -> return $ Just (address, errorText)
        Right latlng    -> do
            liftIO . runDB $ mapM_
                (uncurry (upsertItemMeta itemId))
                [ (latitudeFieldId , T.pack . show $ lat latlng)
                , (longitudeFieldId, T.pack . show $ lng latlng)
                ]
            return Nothing
  where
    -- Build an Address string from the Contact Address metas.
    buildAddress :: Text
    buildAddress =
        let AddressMetas {..} = addressMetas
        in  T.intercalate ", " $ filter (/= "") $ if visibility == Public
                then [addressOne, addressTwo, city, state, zipCode, country]
                else [city, state, zipCode, country]


data AddressVisibility
    = Private
    | Public
    deriving (Eq)

data GeocodingError
    = GeocodingError
        { geCommunityName :: Text
        , geListingId :: Int
        , geAddress :: Text
        , geErrorText :: Text
        } deriving (Show, Generic)
instance ToNamedRecord GeocodingError
instance DefaultOrdered GeocodingError
