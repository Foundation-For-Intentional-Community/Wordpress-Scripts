{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{- | This script searches for listings with public addresses that have no
latitude or longitude set. It attempts to geocode either the map address or
the contact address, saving the returned latitude & longitude to the item
meta table.

It also does this for listings at the default values(the maps center point)
or at (0, 0).

There are CLI flags to delete default values and values for listings with
private addresses.

-}
import           Control.Monad                  ( void
                                                , when
                                                )
import           Control.Monad.Reader           ( MonadReader
                                                , MonadIO
                                                , ReaderT
                                                , runReaderT
                                                , asks
                                                , liftIO
                                                )
import           Data.Maybe                     ( isNothing )
import           Data.Text                      ( Text )
import           Database.Persist               ( Entity(..)
                                                , (==.)
                                                , deleteWhere
                                                )
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
                                                , upsertItemMeta
                                                )
import           Schema                  hiding ( Key )

import qualified Data.Map.Strict               as M
import qualified Data.Text                     as T


main :: IO ()
main = do
    env <-
        Env <$> cmdArgs argsSpec <*> getApiKey <*> newManager tlsManagerSettings
    flip runReaderT env . unScript $ do
        listingData <- liftIO $ runDB getListings
        mapM_ fixCoords listingData
  where
    getApiKey = lookupEnv "MAPS_API_KEY" >>= \case
        Nothing ->
            error "You must define a `MAPS_API_KEY` environmental variable."
        Just key -> return . Key $ T.pack key


-- CLI Args

data Args
    = Args
        { deletePrivateCoordinates :: Bool
        , deleteDefaultCoordinates :: Bool
        } deriving (Show, Data, Typeable)

argsSpec :: Args
argsSpec =
    Args
            { deletePrivateCoordinates = def
                &= name "delete-private"
                &= explicit
                &= help "Remove metas for private addresses"
            , deleteDefaultCoordinates = def
                &= name "delete-default"
                &= explicit
                &= help "Remove any default coordinates"
            }
        &= program "directory-fix-coordinates"
        &= summary "Directory - Address Geocoding"
        &= help
               "Geocode the map/contact addresses of listings with public addresses & no coordinates."


-- Script Environment

newtype Script a
    = Script { unScript :: ReaderT Env IO a }
    deriving (Functor, Applicative, Monad, MonadIO, MonadReader Env)

data Env
    = Env
        { args :: Args
        , apiKey :: Key
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
fixCoords :: (Entity FormItem, M.Map Int Text, Maybe Post) -> Script ()
fixCoords (Entity itemId _, metaMap, _) =
    let publicAddress = M.lookup isAddressPublicFieldId metaMap
        mapAddress    = M.lookup mapAddressFieldId metaMap
        latitude      = M.lookup latitudeFieldId metaMap
        longitude     = M.lookup longitudeFieldId metaMap
        addressMetas  = getAddressMetas metaMap
    in  case (publicAddress, latitude, longitude) of
            -- Existng Coordinates
            (Just "Public", Just lat, Just long) ->
                when (isDefaultLatAndLong lat long) $ do
                    result <- geocodeAndSave itemId mapAddress addressMetas
                    deleteDefault <- asks $ deleteDefaultCoordinates . args
                    when (isNothing result && deleteDefault) deleteLatAndLong
            -- No or Partial Coordinates
            (Just "Public", _, _) ->
                void $ geocodeAndSave itemId mapAddress addressMetas
            -- Not Public
            (_, Nothing, Nothing) -> return ()
            -- Not Public w/ Incomplete Lat/Lng
            (_, _      , _      ) -> do
                deletePrivate <- asks $ deletePrivateCoordinates . args
                when deletePrivate deleteLatAndLong
  where
    -- Default Latitude/Longitude might be (0, 0), or our maps default
    -- center point.
    isDefaultLatAndLong :: Text -> Text -> Bool
    isDefaultLatAndLong lat long =
        (lat == "0" && long == "0")
            || (lat == "39.095963" && long == "-96.606447")
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
geocodeAndSave :: FormItemId -> Maybe Text -> AddressMetas -> Script (Maybe ())
geocodeAndSave itemId mapAddress addressMetas = do
    let address = case mapAddress of
            Nothing   -> buildAddress
            Just ""   -> buildAddress
            Just addr -> addr
    geocodeAddress address >>= \case
        Left errorText -> do
            liftIO
                .  putStrLn
                $  "Error: Could not geocode address `"
                <> T.unpack address
                <> "` for Item "
                <> show itemId
                <> " - "
                <> T.unpack errorText
            return Nothing
        Right latlng -> do
            liftIO . runDB $ mapM_
                (uncurry (upsertItemMeta itemId))
                [ (latitudeFieldId , T.pack . show $ lat latlng)
                , (longitudeFieldId, T.pack . show $ lng latlng)
                ]
            return $ Just ()
  where
    -- Build an Address string from the Contact Address metas.
    buildAddress :: Text
    buildAddress =
        let AddressMetas {..} = addressMetas
        in  T.intercalate ", " $ filter
                (/= "")
                [addressOne, addressTwo, city, state, zipCode, country]
