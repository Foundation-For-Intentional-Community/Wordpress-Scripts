{-# LANGUAGE OverloadedStrings #-}
{-| This script diffs the users in the FIC Membership group and those with
Subscriptions to the FIC Membership product, writing two exports containing
the differing users' name & email.
-}
import           Data.Maybe                     ( catMaybes )
import           Database.Persist               ( (==.) )
import           Database.Persist.Sql           ( toSqlKey )
import           System.Console.CmdArgs.Implicit
                                                ( (&=)
                                                , cmdArgs
                                                , program
                                                , help
                                                , summary
                                                )

import           DB                             ( Subscription(..)
                                                , runDB
                                                , getGroupUsers
                                                , getSubscriptions
                                                , orderHasProductOrVariant
                                                , getOrderCustomer
                                                )
import           Export                         ( toCsvFile )
import           Export.User                    ( userToNameAndEmail )
import           Schema                         ( EntityField(PostStatus) )

import qualified Data.List                     as L

main :: IO ()
main = do
    cmdArgs args
    groupUsers  <- runDB . getGroupUsers $ toSqlKey 4
    subscribers <- runDB $ do
        subs <-
            filter
                    ( orderHasProductOrVariant ["14602"] []
                    . subscriptionLineItems
                    )
                <$> getSubscriptions [PostStatus ==. "wc-active"]
        catMaybes <$> mapM (getOrderCustomer . subscriptionPostMetas) subs
    let inGroupNotSubscriber = groupUsers L.\\ subscribers
        subscriberNotInGroup = subscribers L.\\ groupUsers
    toCsvFile "membership-diff-in-group-not-subscriber.csv"
        $ map userToNameAndEmail inGroupNotSubscriber
    toCsvFile "membership-diff-subscriber-not-in-group.csv"
        $ map userToNameAndEmail subscriberNotInGroup

args :: ()
args =
    ()
        &= program "membership-diff-export"
        &= summary "FIC Membership - Group/Subscriptions Difference Exports"
        &= help
               "Export the name & email of FIC members without subscriptions and subscribers not in the Membership group."
