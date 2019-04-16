{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

import           Control.Monad                  ( forM_ )
import           Data.Function                  ( (&) )
import           Data.List.Split                ( chunksOf )
import           Database.Persist.Sql           ( (==.)
                                                , PersistFilter(..)
                                                , Filter(Filter)
                                                , entityKey
                                                , fromSqlKey
                                                , selectList
                                                )
import           GHC.Generics                   ( Generic )
import           System.Console.CmdArgs.Implicit
                                                ( (&=)
                                                , Data
                                                , Typeable
                                                , cmdArgs
                                                , program
                                                , help
                                                , summary
                                                , def
                                                , argPos
                                                , typ
                                                )
import           System.Process.Typed           ( proc
                                                , runProcess
                                                , setWorkingDir
                                                )

import           DB                             ( runDB )
import           Schema                         ( EntityField
                                                    ( PostType
                                                    , PostMimeType
                                                    )
                                                )

main :: IO ()
main = do
    Args { imageSize, wordpressPath } <- cmdArgs argSpec
    chunkedImageIDs <- chunksOf 1000 . map (fromSqlKey . entityKey) <$> runDB
        (selectList [PostMimeType `like` "image", PostType ==. "attachment"] [])
    forM_ chunkedImageIDs
        $ runProcess
        . regenerateCommand imageSize wordpressPath
  where
    regenerateCommand size path imageIDs =
        let args =
                [ "media"
                    , "regenerate"
                    , "--yes"
                    , "--skip-delete"
                    , "--image_size=" <> size
                    ]
                    <> map show imageIDs
        in  proc "wp" args & setWorkingDir path
    like field val =
        Filter field (Left $ "%" <> val <> "%") (BackendSpecificFilter "like")


data Args
    = Args
        { imageSize :: String
        , wordpressPath :: FilePath
        }
    deriving (Generic, Data, Typeable)

argSpec :: Args
argSpec =
    Args
            { imageSize     = def &= argPos 0 &= typ "IMAGE_SIZE"
            , wordpressPath = def &= argPos 1 &= typ "WORDPRESS_PATH"
            }
        &= program "regenerate-thumbnails"
        &= summary "Wordpress - Regenerate Thumbnails"
        &= help
               "Batch regeneration of thumbnails for a specific image_size using `wp media regenerate`."
