{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
module DB
    ( module Schema
    , getBestUserName
    , getGroupUsers
    , Order(..)
    , getOrders
    , getListings
    , AddressMetas(..)
    , getAddressMetas
    , upsertItemMeta
    , decodePHPStringArray
    )
where

import           Control.Applicative            ( (<|>) )
import           Control.Monad                  ( forM )
import           Data.Maybe                     ( fromMaybe )
import           Data.Text                      ( Text )
import           Data.Text.Encoding             ( encodeUtf8
                                                , decodeUtf8
                                                )
import           Data.PHPSession                ( PHPSessionValue(..)
                                                , decodePHPSessionValue
                                                , convFrom
                                                )
import           Database.Persist.MySQL

import           Schema

import qualified Data.Map.Strict               as M
import qualified Data.ByteString.Lazy          as LBS


-- Users

getBestUserName :: (User, M.Map Text Text) -> Text
getBestUserName (user, metaMap) =
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

getGroupUsers :: GroupId -> DB [(User, M.Map Text Text)]
getGroupUsers groupId = do
    userIds <-
        map (userGroupUser . entityVal)
            <$> selectList [UserGroupGroup ==. groupId] []
    users <- selectList [UserId <-. userIds] []
    forM users $ \(Entity userId user) -> do
        metaMap <-
            M.fromList
            .   map (\(Entity _ meta) -> (userMetaKey meta, userMetaValue meta))
            <$> selectList [UserMetaUser ==. userId] []
        return (user, metaMap)


-- Store

data Order =
    Order
        { orderPost :: Entity Post
        , orderPostMetas :: M.Map Text Text
        , orderLineItems :: [Text]
        }

getOrders :: DB [Order]
getOrders = do
    os <- selectList [PostType ==. "shop_order"] []
    forM os $ \e@(Entity orderId _) -> do
        ms <- selectList [PostMetaPost ==. orderId] []
        let
            metaMap = M.fromList $ map
                (\(Entity _ m) ->
                    (postMetaKey m, fromMaybe "" $ postMetaValue m)
                )
                ms
        lineItems <- selectList
            [OrderItemOrder ==. orderId, OrderItemType ==. "line_item"]
            []
        return Order
            { orderPost      = e
            , orderPostMetas = metaMap
            , orderLineItems = map (orderItemName . entityVal) lineItems
            }


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
    in  AddressMetas
            { addressOne = getMeta 425
            , addressTwo = getMeta 426
            , city       = getMeta 427
            , state      = state_
            , zipCode    = getMeta 429
            , country    = country_
            }
    where getMeta fId = fromMaybe "" $ M.lookup fId metaMap

upsertItemMeta :: FormItemId -> Int -> Text -> DB ()
upsertItemMeta itemId fieldId value = do
    let itemMeta = FormItemMeta
            { formItemMetaField = fieldId
            , formItemMetaItem  = itemId
            , formItemMetaValue = Just value
            }
    selectFirst [FormItemMetaItem ==. itemId, FormItemMetaField ==. fieldId] []
        >>= \case
                Just (Entity metaId _) -> replace metaId itemMeta
                Nothing                -> insert_ itemMeta


-- Utils

decodePHPStringArray :: Text -> [(Text, Text)]
decodePHPStringArray s =
    let phpVals = decodePHPSessionValue $ LBS.fromStrict $ encodeUtf8 s
    in  recursiveDecode $ fromMaybe (PHPSessionValueArray []) phpVals
  where
    both f (x, y) = (f x, f y)
    filterStrings x = case x of
        (PHPSessionValueString _, PHPSessionValueString _) -> True
        _ -> False
    recursiveDecode :: PHPSessionValue -> [(Text, Text)]
    recursiveDecode x = case x of
        PHPSessionValueArray ((_, PHPSessionValueArray vals) : b) ->
            (both (decodeUtf8 . convFrom) <$> filter filterStrings vals)
                ++ recursiveDecode (PHPSessionValueArray b)
        _ -> []
