{-# LANGUAGE DeriveGeneric #-}
{-| This module contains types & functions to transform Users into
commonly used export rows.
-}
module Export.User where

import           Data.Csv                       ( ToNamedRecord
                                                , DefaultOrdered
                                                )
import           Data.Text                      ( Text )
import           Database.Persist               ( Entity(..) )
import           GHC.Generics                   ( Generic )

import           DB                             ( User(..)
                                                , getBestUserName
                                                )

import qualified Data.Map                      as M

data NameAndEmail
    = NameAndEmail
        { name :: Text
        , email :: Text
        } deriving (Show, Generic)
instance ToNamedRecord NameAndEmail
instance DefaultOrdered NameAndEmail

userToNameAndEmail :: (Entity User, M.Map Text Text) -> NameAndEmail
userToNameAndEmail (Entity _ user, metas) =
    NameAndEmail {name = getBestUserName (user, metas), email = userEmail user}
