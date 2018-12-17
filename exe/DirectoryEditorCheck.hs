{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-| Export the Entry Author, Listing User, & Post Author of Listings where
they don't match.
-}
import           Data.Csv                       ( ToNamedRecord
                                                , DefaultOrdered
                                                )
import           Data.Maybe                     ( fromMaybe
                                                , isJust
                                                )
import qualified Data.Text                     as T
import           Database.Persist.Sql           ( Entity(..)
                                                , fromSqlKey
                                                , toSqlKey
                                                , get
                                                )
import           Data.Text                      ( Text )
import           GHC.Generics                   ( Generic )
import           System.Console.CmdArgs.Implicit
                                                ( (&=)
                                                , cmdArgs
                                                , program
                                                , summary
                                                , help
                                                )
import           Text.Read                      ( readMaybe )

import           Export                         ( toCsvFile )
import           DB                             ( DB
                                                , User(..)
                                                , UserId
                                                , runDB
                                                , getListings
                                                , Post(..)
                                                , FormItem(..)
                                                )

import qualified Data.Map                      as M

main :: IO ()
main =
    cmdArgs args >> runDB (getListings >>= nonMatchingListings) >>= toCsvFile
        "directory-editor-check.csv"


args :: ()
args =
    ()
        &= program "directory-editor-check"
        &= summary "Directory - Check Editor/Author/Owner Users"
        &= help
               "Export the editor, author, & owner of each listing where they are not the same user."

userIdFieldId :: Int
userIdFieldId = 430
communityNameFieldId :: Int
communityNameFieldId = 9

nonMatchingListings
    :: [(Entity FormItem, M.Map Int Text, Maybe Post)] -> DB [ExportData]
nonMatchingListings = mapMaybeM filterListings
  where
    mapMaybeM :: Monad m => (a -> m (Maybe b)) -> [a] -> m [b]
    mapMaybeM f xs = case xs of
        []       -> return []
        x : rest -> do
            x_ <- f x
            case x_ of
                Nothing  -> mapMaybeM f rest
                Just x__ -> (x__ :) <$> mapMaybeM f rest
    filterListings
        :: (Entity FormItem, M.Map Int Text, Maybe Post)
        -> DB (Maybe ExportData)
    filterListings (item, fields, maybePost) = do
        let listingID = fromIntegral . fromSqlKey $ entityKey item
            communityName =
                fromMaybe (formItemName $ entityVal item)
                    $ M.lookup communityNameFieldId fields
            postExists = isJustText maybePost
            postIsDraft =
                maybe "" (boolText . (/= "publish") . postStatus) maybePost
            formidableIsDraft = boolText . formItemIsDraft $ entityVal item
        postUser        <- maybeEmptyName $ fmap postAuthor maybePost
        formidableOwner <- getUserName (formItemUser $ entityVal item)
        listingUser     <-
            maybeEmptyName
            $   fmap toSqlKey
            .   readMaybe
            .   T.unpack
            =<< M.lookup userIdFieldId fields
        if listingUser
               == ""
               || listingUser
               /= postUser
               || listingUser
               /= formidableOwner
               || formidableOwner
               /= postUser
            then return $ Just ExportData { .. }
            else return Nothing
    getUserName :: UserId -> DB Text
    getUserName userId = maybe "" userLogin <$> get userId
    maybeEmptyName :: Maybe UserId -> DB Text
    maybeEmptyName = maybe (return "") getUserName
    isJustText :: Maybe a -> Text
    isJustText x = boolText $ isJust x
    boolText :: Bool -> Text
    boolText b = if b then "Yes" else "No"

data ExportData
    = ExportData
        { listingID :: Int
        , communityName :: Text
        , formidableOwner :: Text
        , listingUser :: Text
        , postUser :: Text
        , postExists :: Text
        , postIsDraft :: Text
        , formidableIsDraft :: Text
        }
    deriving (Show, Generic)

instance ToNamedRecord ExportData
instance DefaultOrdered ExportData
