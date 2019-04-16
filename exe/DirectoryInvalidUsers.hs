{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-| Export Listings Whose Users No Longer Exist. -}
import           Data.Csv                       ( ToNamedRecord
                                                , DefaultOrdered
                                                )
import           Data.Maybe                     ( catMaybes
                                                , isJust
                                                , fromMaybe
                                                )
import           Data.Text                      ( Text )
import           Database.Persist.Sql           ( Entity(..)
                                                , fromSqlKey
                                                , toSqlKey
                                                )
import           GHC.Generics                   ( Generic )
import           System.Console.CmdArgs.Implicit
                                                ( (&=)
                                                , cmdArgs
                                                , program
                                                , summary
                                                , help
                                                )

import           DB                             ( DB
                                                , FormItem(..)
                                                , Post(..)
                                                , runDB
                                                , getUser
                                                , getListings
                                                , getBestCommunityName
                                                )
import           Export                         ( toCsvFile )

import qualified Data.Map                      as M
import qualified Data.Text                     as T

main :: IO ()
main = do
    cmdArgs args
    exportData <- runDB $ getListings >>= fmap catMaybes . mapM invalidFilter
    toCsvFile "directory-invalid-users.csv" exportData

args :: ()
args =
    ()
        &= program "directory-invalid-users"
        &= summary "Directory - Export Listings with Invalid Users"
        &= help
               "Generate an export of listings with a non-existent formidable user, meta-field user, or post author."


userIdFieldId :: Int
userIdFieldId = 430


invalidFilter
    :: (Entity FormItem, M.Map Int Text, Maybe Post) -> DB (Maybe ExportData)
invalidFilter l@(item, fields, maybePost) = do
    let listingID     = fromIntegral . fromSqlKey $ entityKey item
        communityName = getBestCommunityName l
    entryUserExists <- boolText . isJust <$> getUser
        (fromMaybe (toSqlKey 0) . formItemUser $ entityVal item)
    listingUser <- sequence
        (getUser . toSqlKey . read . T.unpack <$> M.lookup userIdFieldId fields)
    let listingUserExists = boolText $ isJust listingUser
    postUserExists <-
        fmap boolText . maybe (return False) (fmap isJust . getUser) $ fmap
            postAuthor
            maybePost
    if "No" `elem` [entryUserExists, listingUserExists, postUserExists]
        then return $ Just ExportData { .. }
        else return Nothing
  where
    boolText :: Bool -> Text
    boolText b = if b then "Yes" else "No"

data ExportData
    = ExportData
        { listingID :: Int
        , communityName :: Text
        , entryUserExists :: Text
        , listingUserExists :: Text
        , postUserExists :: Text
        }
    deriving (Show, Generic)

instance ToNamedRecord ExportData
instance DefaultOrdered ExportData
