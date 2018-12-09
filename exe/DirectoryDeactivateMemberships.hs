{-# LANGUAGE OverloadedStrings #-}
{-| De-activate the membership fields of Community's who's membership
expiration dates have expired.
-}
import           Control.Applicative            ( (<|>) )
import           Control.Monad                  ( when )
import           Data.Text                      ( Text )
import           Data.Time.Clock                ( UTCTime
                                                , getCurrentTime
                                                )
import           Data.Time.Format               ( defaultTimeLocale
                                                , parseTimeM
                                                )
import           Database.Persist               ( Entity(..) )
import           System.Console.CmdArgs.Implicit
                                                ( (&=)
                                                , cmdArgs
                                                , program
                                                , help
                                                , summary
                                                )

import           DB                             ( FormItem(formItemName)
                                                , Post
                                                , runDB
                                                , getListings
                                                , upsertItemMeta
                                                )

import qualified Data.Map                      as M
import qualified Data.Text                     as T


main :: IO ()
main = cmdArgs args >> runDB getListings >>= mapM_ deactivateIfExpired

-- | Query the FIC Membership status, deactivating if past the expiration
-- date.
deactivateIfExpired :: (Entity FormItem, M.Map Int Text, Maybe Post) -> IO ()
deactivateIfExpired (Entity itemId item, fieldMap, _) =
    case M.lookup isFicMemberFieldId fieldMap of
        Just "Yes" -> do
            currentTime <- getCurrentTime
            when (isExpired currentTime) $ logDeactivation >> runDB
                (upsertItemMeta itemId isFicMemberFieldId "No")
        _ -> return ()
  where
    logDeactivation :: IO ()
    logDeactivation = case M.lookup membershipExpirationDateFieldId fieldMap of
        Just date ->
            putStrLn
                $  "Deactivating membership for "
                ++ T.unpack (formItemName item)
                ++ " with expiration date of "
                ++ T.unpack date
                ++ "."
        Nothing ->
            putStrLn
                $  "Warning, deactivating membership for  "
                ++ T.unpack (formItemName item)
                ++ " with no expiration date!"
    isExpired :: UTCTime -> Bool
    isExpired currentTime = maybe
        False
        (< currentTime)
        (   expirationParser
        .   T.unpack
        =<< M.lookup membershipExpirationDateFieldId fieldMap
        )
    expirationParser :: String -> Maybe UTCTime
    expirationParser s =
        parseTimeM True defaultTimeLocale "%Y-%m-%d" s
            <|> parseTimeM True defaultTimeLocale "%m/%d/%Y" s

isFicMemberFieldId :: Int
isFicMemberFieldId = 933
membershipExpirationDateFieldId :: Int
membershipExpirationDateFieldId = 985


args :: ()
args =
    ()
        &= program "directory-deactivate-memberships"
        &= summary "Directory - Deactivate Expired FIC Memberships"
        &= help
               "Deactivate the FIC Memberships for Communities with Membership Expiration Dates in the past."
