{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

import           Data.Csv                       ( ToNamedRecord
                                                , DefaultOrdered
                                                )
import           Data.Text                      ( Text )
import           Database.Persist               ( Entity )
import           GHC.Generics                   ( Generic )
import           System.Console.CmdArgs.Implicit
                                                ( (&=)
                                                , cmdArgs
                                                , program
                                                , help
                                                , summary
                                                )

import           DB                             ( AddressMetas(..)
                                                , FormItem
                                                , Post
                                                , runDB
                                                , getListings
                                                , getBestCommunityName
                                                , getAddressMetas
                                                )
import           Export                         ( toCsvFile )

import qualified Data.Map                      as M

main :: IO ()
main = do
    cmdArgs args
    listings <- runDB $ filter filterListings <$> getListings
    let exportRows = map listingToData listings
    toCsvFile "directory-member-export.csv" exportRows
  where
    filterListings (_, metas, _) =
        (== Just "Yes") $ M.lookup isFicMemberFieldId metas
    isFicMemberFieldId = 933


args :: ()
args =
    ()
        &= program "directory-member-export"
        &= summary "Directory - Member Communities Export"
        &= help
               "Export the name, city, state, & country of all FIC Member communities."


data ExportData =
    ExportData
        { communityName :: Text
        , city :: Text
        , state :: Text
        , country :: Text
        } deriving (Show, Generic)
instance ToNamedRecord ExportData
instance DefaultOrdered ExportData


listingToData :: (Entity FormItem, M.Map Int Text, Maybe Post) -> ExportData
listingToData l@(_, metas, _) =
    let AddressMetas {..} = getAddressMetas metas
        communityName     = getBestCommunityName l
    in  ExportData {..}
