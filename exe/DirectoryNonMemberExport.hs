{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
import           Control.Monad                  ( forM )
import           Data.Csv                       ( ToNamedRecord
                                                , DefaultOrdered
                                                )
import           Data.Maybe                     ( catMaybes
                                                , fromMaybe
                                                )
import           Data.Text                      ( Text )
import           Database.Persist               ( (==.)
                                                , Entity(..)
                                                , get
                                                , selectList
                                                )
import           GHC.Generics                   ( Generic )
import           System.Console.CmdArgs.Implicit
                                                ( (&=)
                                                , cmdArgs
                                                , program
                                                , help
                                                , summary
                                                )

import           DB                             ( User(..)
                                                , UserMeta(..)
                                                , Post(..)
                                                , FormItem(..)
                                                , runDB
                                                , getBestUserName
                                                , getListings
                                                )
import           Export                         ( toCsvFile )
import           Schema                         ( EntityField(UserMetaUser) )

import qualified Data.List                     as L
import qualified Data.Map                      as M

main :: IO ()
main = do
    cmdArgs args
    listings    <- filter isNonMember <$> runDB getListings
    withEditors <- runDB $ forM listings $ \(e@(Entity _ item), m, mP) ->
        let editorId = case mP of
                Nothing   -> formItemUser item
                Just post -> postAuthor post
        in
            do
                editor <- get editorId >>= \ed -> do
                    metas <- selectList [UserMetaUser ==. editorId] []
                    let userMetaMap = foldl
                            (\mmap (Entity _ meta) -> M.insert
                                (userMetaKey meta)
                                (fromMaybe "" $ userMetaValue meta)
                                mmap
                            )
                            M.empty
                            metas
                    return $ (\u -> (Entity editorId u, userMetaMap)) <$> ed
                return (e, m, mP, editor)
    toCsvFile "directory-non-member-export.csv"
        . L.nubBy (\x y -> contactEmail x == contactEmail y)
        $ concatMap listingToData withEditors
  where
    isNonMember (_, metaMap, _) =
        maybe True ("No" ==) $ M.lookup isFicMemberFieldId metaMap

isFicMemberFieldId :: Int
isFicMemberFieldId = 933
contactEmailFieldId :: Int
contactEmailFieldId = 199
contactNameFieldId :: Int
contactNameFieldId = 202

args :: ()
args =
    ()
        &= program "directory-non-member-export"
        &= summary "Directory - Non-Member Export"
        &= help
               "Export the contact/editor name, email & communtiy of non-FIC Member listings."


data ExportData =
    ExportData
        { contactName :: Text
        , communityName :: Text
        , contactEmail :: Text
        } deriving (Eq, Show, Generic)

instance ToNamedRecord ExportData
instance DefaultOrdered ExportData

listingToData
    :: ( Entity FormItem
       , M.Map Int Text
       , Maybe Post
       , Maybe (Entity User, M.Map Text Text)
       )
    -> [ExportData]
listingToData (Entity _ item, metaMap, post, userAndMetas) =
    let community = case post of
            Just p  -> postTitle p
            Nothing -> formItemName item
        contactData =
            case
                    (,)
                    <$> getMeta contactNameFieldId
                    <*> getMeta contactEmailFieldId
                of
                    Nothing            -> Nothing
                    Just (name, email) -> Just ExportData
                        { contactName   = name
                        , contactEmail  = email
                        , communityName = community
                        }
        editorData = case userAndMetas of
            Nothing                   -> Nothing
            Just u@(Entity _ user, _) -> Just ExportData
                { contactName   = getBestUserName u
                , contactEmail  = userEmail user
                , communityName = community
                }
    in  catMaybes [editorData, contactData]
    where getMeta fieldId = M.lookup fieldId metaMap
