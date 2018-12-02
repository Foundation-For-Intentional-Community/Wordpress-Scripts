{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
module Schema where

import           Control.Monad.IO.Unlift        ( MonadUnliftIO
                                                , liftIO
                                                )
import           Control.Monad.Logger           ( LoggingT
                                                , runStderrLoggingT
                                                )
import           Control.Monad.Trans.Reader     ( ReaderT )
import           Data.Text                      ( Text )
import           Data.Time.Clock                ( UTCTime )
import           Database.Persist.MySQL
import           Database.Persist.TH
import           System.Environment             ( lookupEnv )


type DB a = ReaderT SqlBackend (LoggingT IO) a

-- | TODO: pull db config from ENV var or file(that is in .gitignore)
runDB :: (MonadUnliftIO m) => ReaderT SqlBackend (LoggingT m) a -> m a
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
    runStderrLoggingT $ withMySQLConn
        defaultConnectInfo { connectUser     = dbUser
                           , connectPassword = dbPassword
                           , connectDatabase = dbName
                           }
        (runSqlConn f)

share [mkPersist sqlSettings] [persistLowerCase|
User sql=3uOgy46w_users
    Id sql=ID
    login Text sql=user_login
    nicename Text sql=user_nicename
    email Text sql=user_email
    deriving Show

UserMeta sql=3uOgy46w_usermeta
    Id sql=umeta_id
    user UserId sql=user_id
    key Text sql=meta_key
    value Text sql=meta_value
    deriving Show

Post sql=3uOgy46w_posts
    Id sql=ID
    title Text sql=post_title
    author UserId sql=post_author
    status Text sql=post_status
    type Text sql=post_type
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


FormItem sql=3uOgy46w_frm_items
    Id sql=id
    name Text sql=name
    form Int sql=form_id
    post PostId sql=post_id
    user UserId sql=user_id
    isDraft Bool sql=is_draft
    deriving Show

FormItemMeta sql=3uOgy46w_frm_item_metas
    Id sql=id
    field Int sql=field_id
    item FormItemId sql=item_id
    value Text Maybe sql=meta_value
    deriving Show
|]
