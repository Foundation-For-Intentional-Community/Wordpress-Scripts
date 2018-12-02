module Export
    ( toCsvFile
    )
where

import           Data.Csv                       ( ToNamedRecord
                                                , DefaultOrdered
                                                , encodeDefaultOrderedByName
                                                )
import           Data.Text                      ( Text
                                                , unpack
                                                )

import qualified Data.ByteString.Lazy          as LBS


toCsvFile :: (DefaultOrdered a, ToNamedRecord a) => Text -> [a] -> IO ()
toCsvFile fileName recordList =
    LBS.writeFile (unpack fileName) $ encodeDefaultOrderedByName recordList
