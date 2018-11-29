{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
module DB
    ( module Schema
    , Order(..)
    , getOrders
    , getListings
    , decodePHPStringArray
    )
where

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
