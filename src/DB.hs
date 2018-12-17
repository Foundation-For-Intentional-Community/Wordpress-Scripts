{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-| This module contains functions for querying the database as well as
related helper types & functions.
-}
module DB
    ( -- * Schema
      module Schema
      -- * Database Monad
    , DB
    , DBMonad
    , runDB
    -- * Users
    , getUser
    , getBestUserName
    , getGroupUsers
    , getPostsAndMetas
    -- * Store
    , Order(..)
    , getOrders
    , getOrderCustomer
    , Subscription(..)
    , getSubscriptions
    , orderHasProductOrVariant
    -- * Directory
    , getListings
    , AddressMetas(..)
    , getAddressMetas
    , getBestCommunityName
    , upsertItemMeta
    -- * Misc
    , decodeSerializedOrderItems
    )
where

import           Conduit                        ( (.|)
                                                , ConduitT
                                                , Void
                                                , ResourceT
                                                , runConduit
                                                , runResourceT
                                                , mapMC
                                                , foldlC
                                                , sinkList
                                                )
import           Control.Applicative            ( (<|>) )
import           Control.Arrow                  ( (>>>) )
import           Control.Monad                  ( forM )
import           Control.Monad.IO.Unlift        ( liftIO )
import           Control.Monad.Logger           ( NoLoggingT
                                                , runNoLoggingT
                                                )
import           Control.Monad.Trans.Reader     ( ReaderT )
import           Data.Maybe                     ( fromMaybe
                                                , mapMaybe
                                                , listToMaybe
                                                )
import           Data.Text                      ( Text )
import           Data.Text.Encoding             ( encodeUtf8
                                                , decodeUtf8
                                                )
import           Data.PHPSession                ( PHPSessionValue(..)
                                                , decodePHPSessionValue
                                                )
import           Database.Persist.MySQL
import           Text.Read                      ( readMaybe )
import           System.Environment             ( lookupEnv )

import           Schema

import qualified Data.ByteString.Lazy          as LBS
import qualified Data.List                     as L
import qualified Data.Map.Strict               as M
import qualified Data.Text                     as T

-- Running Queries

type DB a = ReaderT SqlBackend (ResourceT (NoLoggingT IO)) a
type DBMonad = ReaderT SqlBackend (ResourceT (NoLoggingT IO))

runDB :: DB a -> IO a
runDB f = do
    let
        errMsg
            = "You must supply the DB_USER, DB_PASS, & DB_NAME environmental variables."
    (dbUser, dbPassword, dbName) <-
        liftIO
        $   (,,)
        <$> lookupEnv "DB_USER"
        <*> lookupEnv "DB_PASS"
        <*> lookupEnv "DB_NAME"
        >>= \case
                (Just u, Just p, Just n) -> return (u, p, n)
                _                        -> error errMsg
    runNoLoggingT $ runResourceT $ withMySQLConn
        defaultConnectInfo { connectUser     = dbUser
                           , connectPassword = dbPassword
                           , connectDatabase = dbName
                           }
        (runSqlConn f)



-- Users

getUser :: UserId -> DB (Maybe (Entity User, M.Map Text Text))
getUser userId = listToMaybe <$> fetchWithMetaMap
    [UserId ==. userId]
    (\e -> [UserMetaUser ==. entityKey e])
    userMetaKey
    (fromMaybe "" . userMetaValue)


getBestUserName :: (Entity User, M.Map Text Text) -> Text
getBestUserName (Entity _ user, metaMap) =
    fromMaybe (userLogin user)
        $   firstLast ""
        <|> firstLast "shipping_"
        <|> firstLast "billing_"
        <|> nonEmpty (userDisplayName user)
        <|> nonEmpty (userNicename user)
  where
    getMeta k = M.lookup k metaMap >>= nonEmpty
    nonEmpty s = if s == "" then Nothing else Just s
    firstLast prefix =
        (\firstName lastName -> firstName <> " " <> lastName)
            <$> getMeta (prefix <> "first_name")
            <*> getMeta (prefix <> "last_name")

getGroupUsers :: GroupId -> DB [(Entity User, M.Map Text Text)]
getGroupUsers groupId = do
    userIds <-
        map (userGroupUser . entityVal)
            <$> selectList [UserGroupGroup ==. groupId] []
    fetchWithMetaMap [UserId <-. userIds]
                     (\e -> [UserMetaUser ==. entityKey e])
                     userMetaKey
                     (fromMaybe "" . userMetaValue)



-- Posts

getPostsAndMetas :: [Filter Post] -> DB [(Entity Post, M.Map Text (Maybe Text))]
getPostsAndMetas filters = fetchWithMetaMap
    filters
    (\p -> [PostMetaPost ==. entityKey p])
    postMetaKey
    postMetaValue



-- Store

-- Make newtypes?
type OrderMetaMap = M.Map Text Text
type OrderItemMetaMap = M.Map Text Text
type OrderLineItems = [(Entity OrderItem, OrderItemMetaMap)]

data Order =
    Order
        { orderPost :: Entity Post
        , orderPostMetas :: OrderMetaMap
        , orderLineItems :: OrderLineItems
        }

getOrders :: DB [Order]
getOrders = fetchOrders >>= mapM fetchItems
  where
    fetchOrders = fetchWithMetaMap [PostType ==. "shop_order"]
                                   (\e -> [PostMetaPost ==. entityKey e])
                                   postMetaKey
                                   (fromMaybe "" . postMetaValue)
    fetchItems (e@(Entity orderId _), metaMap) = do
        lineItems <- fetchWithMetaMap
            [OrderItemOrder ==. orderId, OrderItemType ==. "line_item"]
            (\item -> [OrderItemMetaItem ==. entityKey item])
            orderItemMetaKey
            orderItemMetaValue
        return Order { orderPost      = e
                     , orderPostMetas = metaMap
                     , orderLineItems = lineItems
                     }

-- A subscription is just a different type of order. E.g., you can use
-- `getOrderCustomer` on the PostMetas.
data Subscription =
    Subscription
        { subscriptionPost :: Entity Post
        , subscriptionPostMetas :: OrderMetaMap
        , subscriptionLineItems :: OrderLineItems
        } deriving (Show)

getSubscriptions :: [Filter Post] -> DB [Subscription]
getSubscriptions filters =
    getPostsAndMetas ((PostType ==. "shop_subscription") : filters)
        >>= mapM withMetas
  where
    withMetas (sub, subMetas) =
        Subscription sub (fmap (fromMaybe "") subMetas)
            <$> fetchWithMetaMap [OrderItemOrder ==. entityKey sub]
                                 (\e -> [OrderItemMetaItem ==. entityKey e])
                                 orderItemMetaKey
                                 orderItemMetaValue

getOrderCustomer :: OrderMetaMap -> DB (Maybe (Entity User, M.Map Text Text))
getOrderCustomer metaMap =
    let maybeUserId =
                M.lookup "_customer_user" metaMap >>= (T.unpack >>> readMaybe)
    in  case maybeUserId of
            Nothing     -> return Nothing
            Just userId -> listToMaybe <$> fetchWithMetaMap
                [UserId ==. toSqlKey userId]
                (\e -> [UserMetaUser ==. entityKey e])
                userMetaKey
                (fromMaybe "" . userMetaValue)

-- Determine whether a Subscription has an OrderItem with a Product or
-- Variation ID matching one given in the respective lists.
orderHasProductOrVariant :: [Text] -> [Text] -> OrderLineItems -> Bool
orderHasProductOrVariant productIds variationIds lineItems =
    let metaMaps = map snd lineItems
    in  oneMetaMatches "_product_id" metaMaps productIds
            || oneMetaMatches "_variation_id" metaMaps variationIds

-- | Return True if one of the MetaMap's value for the key is in the given list.
oneMetaMatches :: Eq c => Text -> [M.Map Text c] -> [c] -> Bool
oneMetaMatches key metaMaps matches =
    not $ null $ mapMaybe (M.lookup key) metaMaps `L.intersect` matches



-- Directory

getListings :: DB [(Entity FormItem, M.Map Int Text, Maybe Post)]
getListings = do
    ls <- selectList [FormItemIsDraft ==. False, FormItemForm ==. 2] []
    forM ls $ \e@(Entity itemId i) -> do
        ms <- selectList [FormItemMetaItem ==. itemId] []
        let
            metaMap = M.fromList $ map
                (\(Entity _ m) ->
                    (formItemMetaField m, fromMaybe "" $ formItemMetaValue m)
                )
                ms
        post <- get $ formItemPost i
        return (e, metaMap, post)

data AddressMetas =
    AddressMetas
        { addressOne :: Text
        , addressTwo :: Text
        , city :: Text
        , state :: Text
        , zipCode :: Text
        , country :: Text
        }
getAddressMetas :: M.Map Int Text -> AddressMetas
getAddressMetas metaMap =
    let country_ = getMeta 424
        state_ =
                if country_ == "United States" then getMeta 815 else getMeta 816
    in  AddressMetas { addressOne = getMeta 425
                     , addressTwo = getMeta 426
                     , city       = getMeta 427
                     , state      = state_
                     , zipCode    = getMeta 429
                     , country    = country_
                     }
    where getMeta fId = fromMaybe "" $ M.lookup fId metaMap

{- | Prefer the post name, falling back to the name meta-field and then
finally to the entry name.
-}
getBestCommunityName :: (Entity FormItem, M.Map Int Text, Maybe Post) -> Text
getBestCommunityName (item, fields, maybePost) =
    let entryName         = formItemName $ entityVal item
        metaFieldName     = M.lookup 9 fields
        directoryPostName = postTitle <$> maybePost
    in  fromMaybe entryName $ directoryPostName <|> metaFieldName

upsertItemMeta :: FormItemId -> Int -> Text -> DB ()
upsertItemMeta itemId fieldId value = do
    let itemMeta = FormItemMeta { formItemMetaField = fieldId
                                , formItemMetaItem  = itemId
                                , formItemMetaValue = Just value
                                }
    selectFirst [FormItemMetaItem ==. itemId, FormItemMetaField ==. fieldId] []
        >>= \case
                Just (Entity metaId _) -> replace metaId itemMeta
                Nothing                -> insert_ itemMeta



-- Utils

{-| Fetch a list of Entities & their Metas. -}
fetchWithMetaMap
    :: forall a b c
     . ( PersistEntity a
       , PersistEntity b
       , PersistEntityBackend a ~ SqlBackend
       , PersistEntityBackend b ~ SqlBackend
       )
    => [Filter a]
    -- ^ Filter the Items
    -> (Entity a -> [Filter b])
    -- ^ Select the Item Metas
    -> (b -> Text)
    -- ^ Get the Key from a Meta
    -> (b -> c)
    -- ^ Get the Value from a Meta
    -> DB [(Entity a, M.Map Text c)]
fetchWithMetaMap filters metaFilters key value =
    runConduit $ selectSource filters [] .| mapMC addMetas .| sinkList
  where
    addMetas :: Entity a -> DB (Entity a, M.Map Text c)
    addMetas item = do
        metas <- runConduit $ selectSource (metaFilters item) [] .| buildMetaMap
        return (item, metas)
    buildMetaMap :: Monad m => ConduitT (Entity b) Void m (M.Map Text c)
    buildMetaMap = flip foldlC M.empty $ \acc meta ->
        M.insert (key $ entityVal meta) (value $ entityVal meta) acc

-- | Decode the old Order Items stored in the PostMeta of an Order.
decodeSerializedOrderItems :: Text -> [M.Map Text Text]
decodeSerializedOrderItems s =
    let phpVals = decodePHPSessionValue $ LBS.fromStrict $ encodeUtf8 s
    in  maybe [] decodeItems phpVals
  where
    decodeItems :: PHPSessionValue -> [M.Map Text Text]
    decodeItems = \case
        PHPSessionValueArray itemsArray -> filter (/= M.empty) $ mapMaybe
            (\case
                (_, PHPSessionValueArray itemArray) ->
                    Just $ decodeItem itemArray
                _ -> Nothing
            )
            itemsArray
        _ -> []
    decodeItem :: [(PHPSessionValue, PHPSessionValue)] -> M.Map Text Text
    decodeItem = foldl decodeItemPair M.empty
    decodeItemPair
        :: M.Map Text Text
        -> (PHPSessionValue, PHPSessionValue)
        -> M.Map Text Text
    decodeItemPair acc = \case
        (PHPSessionValueString key, PHPSessionValueString value) ->
            M.insert (lbsText key) (lbsText value) acc
        _ -> acc
    lbsText :: LBS.ByteString -> Text
    lbsText = decodeUtf8 . LBS.toStrict
