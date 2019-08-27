{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-| Export all fields for all Published Listings
-}

import qualified Data.ByteString.Lazy          as LBS
import           Control.Monad                  ( void )
import           Data.Csv                       ( encode )
import qualified Data.List                     as L
import qualified Data.Map.Strict               as M
import           Data.Text                      ( Text )
import           Database.Persist.Sql           ( (==.)
                                                , Entity(..)
                                                , SelectOpt(Asc)
                                                , selectList
                                                , fromSqlKey
                                                )
import           GHC.Generics                   ( Generic )
import           System.Console.CmdArgs.Implicit
                                                ( (&=)
                                                , Data
                                                , Typeable
                                                , cmdArgs
                                                , program
                                                , help
                                                , summary
                                                )

import qualified DB
import           DB                             ( getListings
                                                , getBestCommunityName
                                                , runDB
                                                , FormFieldId
                                                , FormItem
                                                , Post
                                                )


main :: IO ()
main = do
    void $ cmdArgs argSpec
    (listings, fields) <- runDB $ (,) <$> getListings <*> selectList
        [DB.FormFieldFormId ==. 2]
        [Asc DB.FormFieldName]
    let fieldList  = map (\(Entity fId f) -> (fId, DB.formFieldName f)) fields
        headerList = map snd fieldList
        rowList    = map (makeRow fieldList) listings
    LBS.writeFile "directory-full-export.csv" $ encode $ headerList : rowList

data Args
    = Args
    deriving (Generic, Data, Typeable)

argSpec :: Args
argSpec =
    Args
        &= program "directory-full-export"
        &= summary "Directory - Full Data Export"
        &= help "Export all information for every Published Listing"

makeRow
    :: [(FormFieldId, Text)]
    -> (Entity FormItem, M.Map Int Text, Maybe Post)
    -> [Text]
makeRow fields l@(_, metas, _) = metaList
  where
    metaList :: [Text]
    metaList = run fields $ M.toList metas
    run :: [(FormFieldId, Text)] -> [(Int, Text)] -> [Text]
    run fs ms = case fs of
        []                  -> []
        (fId, fName) : rest -> if fName == "Community name"
            then getBestCommunityName l : run rest ms
            else
                case
                    L.partition
                        (\(mId, _) -> fromIntegral mId == fromSqlKey fId)
                        ms
                of
                    ((_, m) : _, otherMetas) -> m : run rest otherMetas
                    ([]        , otherMetas) -> "" : run rest otherMetas
