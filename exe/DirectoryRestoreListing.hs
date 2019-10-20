{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
import           Control.Monad.IO.Class         ( liftIO )
import           Database.Persist.MySQL
import           System.Console.CmdArgs.Implicit
                                                ( Data
                                                , Typeable
                                                , (&=)
                                                , cmdArgs
                                                , program
                                                , help
                                                , summary
                                                , typ
                                                , def
                                                , argPos
                                                )
import           System.Environment             ( lookupEnv )

import           DB

import qualified Data.Text                     as T

main :: IO ()
main = do
    listingName <- aName <$> cmdArgs args
    (oldPost, oldPostMetas, oldEntry, oldEntryMetas) <- runInBackupDB $ do
        p <-
            selectFirst
                    [PostTitle ==. T.pack listingName, PostType ==. "directory"]
                    []
                >>= justOrError
                        (  "Could not find post for listing w/ name: "
                        <> listingName
                        )
        pms <- selectList [PostMetaPost ==. entityKey p] []
        e   <-
            selectFirst [FormItemPost ==. Just (entityKey p)] [] >>= justOrError
                ("Could not find entry for post w/ ID: " <> show (entityKey p))
        ems <- selectList [FormItemMetaItem ==. entityKey e] []
        return (p, pms, e, ems)
    runDB $ do
        postId <- insert $ entityVal oldPost
        mapM_ (\(Entity _ m) -> insert $ m { postMetaPost = postId })
              oldPostMetas
        entryId <- insert $ (entityVal oldEntry) { formItemPost = Just postId }
        mapM_ (\(Entity _ m) -> insert $ m { formItemMetaItem = entryId })
              oldEntryMetas
  where
    justOrError err = \case
        Just val -> return val
        Nothing  -> error err

data Args
    = Args
        { aName :: String
        } deriving (Show, Data, Typeable)


args :: Args
args =
    (Args { aName = def &= argPos 0 &= typ "LISTING_NAME" })
        &= program "directory-restore-listing"
        &= summary "Directory - Restore Listing"
        &= help
               "Restore a listing's Entry, Metas, & Post from a backup database."


runInBackupDB :: DB a -> IO a
runInBackupDB f = do
    (u, p, n) <-
        liftIO
        $   (,,)
        <$> lookupEnv "DB_USER"
        <*> lookupEnv "DB_PASS"
        <*> lookupEnv "BACKUP_DB_NAME"
        >>= \case
                (Just u, Just p, Just n) -> return (u, p, n)
                _ -> error "Need Env Vars: DB_USER, DB_PASS, BACKUP_DB_NAME"
    let cfg = defaultConnectInfo { connectUser     = u
                                 , connectPassword = p
                                 , connectDatabase = n
                                 }
    runSQL cfg f
