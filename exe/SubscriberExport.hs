{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

{- | Export the Name & Email of Users with Active Subscriptions.

You can pass a list of Product and/or Variation IDs via CLI flags. Any
subscription with an item matching an ID will be included. If no IDs are
passed, users with any active subscriptions are exported.
-}
import           Data.Maybe                     ( catMaybes )
import           Data.Text                      ( Text )
import           Database.Persist               ( (==.) )
import           GHC.Generics                   ( Generic )
import           System.Console.CmdArgs.Implicit
                                                ( (&=)
                                                , Data
                                                , Typeable
                                                , cmdArgs
                                                , def
                                                , typ
                                                , program
                                                , help
                                                , details
                                                , summary
                                                )

import           DB                             ( Subscription(..)
                                                , runDB
                                                , getSubscriptions
                                                , getOrderCustomer
                                                , orderHasProductOrVariant
                                                )
import           Export                         ( toCsvFile
                                                , nameWithProductTags
                                                )
import           Export.User                    ( userToNameAndEmail )
import           Schema                         ( EntityField(..) )

import qualified Data.List                     as L


main :: IO ()
main = do
    args <- cmdArgs argSpec
    let productIds   = products args
        variationIds = variations args
    users <- runDB $ do
        allActiveSubs <- getSubscriptions [PostStatus ==. "wc-active"]
        let
            filteredSubs = if null productIds && null variationIds
                then allActiveSubs
                else filter
                    ( orderHasProductOrVariant productIds variationIds
                    . subscriptionLineItems
                    )
                    allActiveSubs
        rawCustomers <- mapM (getOrderCustomer . subscriptionPostMetas)
                             filteredSubs
        return . L.nub $ catMaybes rawCustomers
    toCsvFile
            (  nameWithProductTags "subscriber-export" productIds variationIds
            <> ".csv"
            )
        $ map userToNameAndEmail users


-- CLI

data Args
    = Args
        { products :: [Text]
        , variations :: [Text]
        } deriving (Generic, Data, Typeable)

argSpec :: Args
argSpec =
    Args
            { products   = def &= typ "PRODUCT_ID,..."
            , variations = def &= typ "VARIATION_ID,..."
            }
        &= program "subscriber-export"
        &= summary "Subscriptions - Subscriber Export"
        &= help "Export the name & email of all active subscribers."
        &= details
               [ "Any subscriptions with a matching product or variation will be included."
               , ""
               , "Passing no product or variation IDs will export all active subscriptions."
               ]
