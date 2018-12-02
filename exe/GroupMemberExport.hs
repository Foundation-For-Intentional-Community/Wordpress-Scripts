{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-| This script exports the Name & Email of all users belonging it a Group.

Either the group name or id must be passed as an argument.
-}
import           Control.Monad.IO.Class         ( liftIO )
import           Data.Csv                       ( ToNamedRecord
                                                , DefaultOrdered
                                                )
import           Data.Text                      ( Text )
import           Database.Persist               ( Entity(..)
                                                , getBy
                                                )
import           Database.Persist.Sql           ( toSqlKey )
import           GHC.Generics                   ( Generic )
import           System.Console.CmdArgs.Implicit
                                                ( (&=)
                                                , Data
                                                , Typeable
                                                , cmdArgs
                                                , def
                                                , argPos
                                                , typ
                                                , program
                                                , help
                                                , details
                                                , summary
                                                )
import           Text.Read                      ( readMaybe )
import           System.Exit                    ( exitFailure )

import           DB                             ( runDB
                                                , User(..)
                                                , getBestUserName
                                                , getGroupUsers
                                                )
import           Export                         ( toCsvFile )
import           Schema                         ( Unique(UniqueGroupName) )

import qualified Data.Map                      as M
import qualified Data.Text                     as T


main :: IO ()
main = do
    args <- cmdArgs argSpec
    let groupIdentifier = parseGroupIdentifier $ group args
    users <- runDB $ getGroupUsers =<< case groupIdentifier of
        ByName groupName -> getBy (UniqueGroupName groupName) >>= \case
            Nothing -> liftIO $ do
                putStrLn $ "Error: Could not find group `" ++ group args ++ "`."
                exitFailure
            Just (Entity key _) -> return key
        ById rawId -> return . toSqlKey $ fromIntegral rawId
    toCsvFile ("group-membership-export-" <> T.pack (group args) <> ".csv")
        $ map userToData users


-- CLI Args
data Args
    = Args
        { group :: String
        }
    deriving (Show, Data, Typeable)

argSpec :: Args
argSpec =
    Args {group = def &= argPos 0 &= typ "GROUP"}
        &= program "group-member-export"
        &= summary "Groups - Member Export"
        &= help "Export the name & email of all members of a Group."
        &= details ["You may pass either a Name or ID for the GROUP."]

data GroupIdentifier
    = ById Int
    | ByName Text

parseGroupIdentifier :: String -> GroupIdentifier
parseGroupIdentifier ident = case readMaybe ident of
    Just i  -> ById i
    Nothing -> ByName $ T.pack ident


-- Exports
data ExportData =
    ExportData
        { name :: Text
        , email :: Text
        } deriving (Show, Generic)
instance ToNamedRecord ExportData
instance DefaultOrdered ExportData

userToData :: (User, M.Map Text Text) -> ExportData
userToData u@(user, _) =
    ExportData {name = getBestUserName u, email = userEmail user}
