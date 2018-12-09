{-# LANGUAGE OverloadedStrings #-}
{-| This module contains functions used to serialize data for exports.
Currently it only supports writing the CSV files with Cassava.
-}
module Export
    ( toCsvFile
    , nameWithProductTags
    )
where

import           Data.Csv                       ( ToNamedRecord
                                                , DefaultOrdered
                                                , encodeDefaultOrderedByNameWith
                                                , defaultEncodeOptions
                                                , EncodeOptions(encUseCrLf)
                                                )
import           Data.Text                      ( Text
                                                , unpack
                                                )

import qualified Data.ByteString.Lazy          as LBS
import qualified Data.List                     as L
import qualified Data.Text                     as T


-- | TODO: Prepend `YYYY-MM-DD-` & append `.csv` extension.
toCsvFile :: (DefaultOrdered a, ToNamedRecord a) => Text -> [a] -> IO ()
toCsvFile fileName recordList =
    LBS.writeFile (unpack fileName) $ encodeDefaultOrderedByNameWith
        (defaultEncodeOptions { encUseCrLf = True })
        recordList

-- | Build a file name encoded with Product & Variation IDs.
--
-- Does not include an extension.
--
-- > nameWithProductTags "base" ["123"] ["456"]
-- "base-p123-v456"
-- > nameWithProductTags "base" [] []
-- "base"
nameWithProductTags :: Text -> [Text] -> [Text] -> Text
nameWithProductTags baseName productIds variationIds
    = let
          prefixedIds =
              L.nub $ map ("p" <>) productIds ++ map ("v" <>) variationIds
          idTags = if null prefixedIds
              then ""
              else "-" <> T.intercalate "-" prefixedIds
      in
          baseName <> idTags
