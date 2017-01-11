{-# LANGUAGE OverloadedStrings #-}


module Sites.Actions.WorkPublish where


import           Control.Exception.Base (AssertionFailed (..))
import           Control.Monad          (void)
import qualified Data.Text              as T
import           Data.Time
import           Data.Traversable
import           Shelly                 hiding (FilePath, (<.>), (</>))
import           System.Environment     (unsetEnv)

import           Sites.Actions.Deploy   (deploySite)
import           Sites.Actions.Publish  (DocLocation (..), overLines,
                                         updateDate)
import           Sites.Git
import           Sites.Types
import           Sites.Utils


workPublish :: FilePath -> Maybe BranchMove -> Maybe UTCTime -> Bool -> IO ()
workPublish metaFile branch pubDate deploy = shelly $ verbosely $ do
    now <-  liftIO
        $   T.pack
        .   formatTime defaultTimeLocale (iso8601DateFormat $ Just "%H:%M:%S")
        <$> maybe getCurrentTime return pubDate

    current <- chdir "reading-log" $ 
        throwMaybe (AssertionFailed "cannot find current branch")
            =<< currentBranch
    chdir "reading-log" $ do
        void $ traverse (git_ "checkout" . pure . branchTo) branch
        void $ traverse (merge current) branch

    overLines metaFile (snd . mapAccumL (updateDate now) Pre)

    chdir "reading-log" $ do
        git_ "add"    ["."]
        git_ "commit" ["-m", "Updated date of post."]

    when deploy $ liftIO $ do
        unsetEnv "DEVELOPMENT"
        deploySite True False
