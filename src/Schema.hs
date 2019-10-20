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

import           Data.Int                       ( Int64 )
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
    slug Text sql=post_name
    content Text sql=post_content
    excerpt Text sql=post_excerpt
    author UserId sql=post_author
    status Text sql=post_status
    parent PostId sql=post_parent default=0
    type Text sql=post_type
    mimeType Text sql=post_mime_type
    date UTCTime sql=post_date_gmt
    dateLocal UTCTime sql=post_date
    commentStatus Text sql=comment_status
    pingStatus Text sql=ping_status
    postPassword Text sql=post_password
    toPing Text sql=to_ping
    pinged Text sql=pinged
    modified UTCTime sql=post_modified_gmt
    modifiedLocal UTCTime sql=post_modified
    contentFiltered Text sql=post_content_filtered
    guid Text sql=guid
    menuOrder Int sql=menu_order default=0
    commentCount Int64 sql=comment_count default=0
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
    description Text Maybe sql=description
    form Int sql=form_id
    post PostId Maybe sql=post_id
    user UserId Maybe sql=user_id
    isDraft Bool sql=is_draft
    createdAt UTCTime sql=created_at
    updatedAt UTCTime sql=updated_at
    updatedBy Text Maybe sql=updated_by
    key Text Maybe sql=item_key
    ip Text Maybe sql=ip
    parentItem FormItemId Maybe sql=parent_item_id
    deriving Show

FormItemMeta sql=3uOgy46w_frm_item_metas
    Id sql=id
    field Int sql=field_id
    item FormItemId sql=item_id
    value Text Maybe sql=meta_value
    created UTCTime sql=created_at
    deriving Show

FormField sql=3uOgy46w_frm_fields
    Id sql=id
    fieldKey Text sql=field_key
    name Text sql=name
    description Text sql=description
    formId Int sql=form_id

|]
