{-# LANGUAGE OverloadedStrings #-}
{- | Publish any posts that have passed their scheduled post date.
-}
import           Data.Time.Clock                ( getCurrentTime )
import           Database.Persist.Sql           ( (==.)
                                                , (<=.)
                                                , (=.)
                                                , selectKeysList
                                                , update
                                                , fromSqlKey
                                                )
import           System.Console.CmdArgs.Implicit
                                                ( (&=)
                                                , cmdArgs
                                                , program
                                                , summary
                                                , help
                                                )

import           DB                             ( DB
                                                , PostId
                                                , runDB
                                                )
import           Schema                         ( EntityField
                                                    ( PostStatus
                                                    , PostDate
                                                    )
                                                )


main :: IO ()
main =
    cmdArgs args >> getMissedPosts >>= logUpdate >>= runDB . mapM_ publishPost

getMissedPosts :: IO [PostId]
getMissedPosts = do
    currentTime <- getCurrentTime
    runDB $ selectKeysList
        [PostStatus ==. "future", PostDate <=. currentTime]
        []

logUpdate :: [PostId] -> IO [PostId]
logUpdate = mapM $ \pId -> do
    putStrLn $ "Publishing post that missed schedule: #" ++ show
        (fromSqlKey pId)
    return pId

publishPost :: PostId -> DB ()
publishPost postId = update postId [PostStatus =. "publish"]

args :: ()
args =
    ()
        &= program "posts-publish-scheduled"
        &= summary "Posts - Publish Scheduled Posts"
        &= help
               "Publish any scheduled Posts that have passed their scheduled date."
