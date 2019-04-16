{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
module Schema where

import           Data.Text                      ( Text )
import           Data.Time.Clock                ( UTCTime )
import           Database.Persist.MySQL
import           Database.Persist.TH



share [mkPersist sqlSettings] [persistLowerCase|
User sql=3uOgy46w_users
    Id sql=ID
    login Text sql=user_login
    nicename Text sql=user_nicename
    email Text sql=user_email
    displayName Text sql=display_name
    deriving Eq Show

UserMeta sql=3uOgy46w_usermeta
    Id sql=umeta_id
    user UserId sql=user_id
    key Text sql=meta_key
    value Text Maybe sql=meta_value
    deriving Show

UserGroup sql=3uOgy46w_groups_user_group
    user UserId sql=user_id
    group GroupId sql=group_id
    Primary user group
    deriving Show

Group sql=3uOgy46w_groups_group
    Id sql=group_id
    name Text sql=name
    UniqueGroupName name
    deriving Show


Post sql=3uOgy46w_posts
    Id sql=ID
    title Text sql=post_title
    content Text sql=post_content
    author UserId sql=post_author
    status Text sql=post_status
    parent PostId sql=post_parent default=0
    type Text sql=post_type
    mimeType Text sql=post_mime_type
    date UTCTime sql=post_date_gmt
    deriving Show

PostMeta sql=3uOgy46w_postmeta
    Id sql=meta_id
    post PostId sql=post_id
    key Text sql=meta_key
    value Text Maybe sql=meta_value
    deriving Show


OrderItem sql=3uOgy46w_woocommerce_order_items
    Id sql=order_item_id
    name Text sql=order_item_name
    type Text sql=order_item_type
    order PostId sql=order_id
    deriving Show

OrderItemMeta sql=3uOgy46w_woocommerce_order_itemmeta
    Id sql=meta_id
    item OrderItemId sql=order_item_id
    key Text sql=meta_key
    value Text sql=meta_value
    deriving Show


FormItem sql=3uOgy46w_frm_items
    Id sql=id
    name Text sql=name
    form Int sql=form_id
    post PostId Maybe sql=post_id
    user UserId Maybe sql=user_id
    isDraft Bool sql=is_draft
    deriving Show

FormItemMeta sql=3uOgy46w_frm_item_metas
    Id sql=id
    field Int sql=field_id
    item FormItemId sql=item_id
    value Text Maybe sql=meta_value
    deriving Show
|]
