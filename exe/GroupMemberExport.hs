{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-| This script exports the Name & Email of all users belonging it a Group.

Either the group name or id must be passed as an argument.
-}
import           Control.Monad.IO.Class         ( liftIO )
import           Data.Text                      ( Text )
import           Database.Persist               ( Entity(..)
                                                , getBy
                                                )
import           Database.Persist.Sql           ( toSqlKey )
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

import           DB                             ( User
                                                , runDB
                                                , getGroupUsers
                                                )
import           Export                         ( toCsvFile )
import           Export.User                    ( userToNameAndEmail )
import           Schema                         ( Unique(UniqueGroupName) )

import qualified Data.Map                      as M
import qualified Data.Text                     as T


main :: IO ()
main = do
    args <- cmdArgs argSpec
    let groupIdentifier = parseGroupIdentifier $ group args
    users <- groupUsers groupIdentifier
    toCsvFile
            (  "group-membership-export-"
            <> T.replace " " "-" (T.pack (group args))
            <> ".csv"
            )
        $ map userToNameAndEmail users

groupUsers :: GroupIdentifier -> IO [(Entity User, M.Map Text Text)]
groupUsers groupIdentifier = runDB $ do
    groupId <- case groupIdentifier of
        ByName groupName -> getBy (UniqueGroupName groupName) >>= \case
            Nothing -> liftIO $ do
                putStrLn
                    $  "Error: Could not find group `"
                    ++ T.unpack groupName
                    ++ "`."
                exitFailure
            Just (Entity key _) -> return key
        ById rawId -> return . toSqlKey $ fromIntegral rawId
    getGroupUsers groupId


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
