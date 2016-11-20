{-# LANGUAGE OverloadedStrings #-}


module Sites.Actions.Publish where


import           Control.Error
import           Control.Exception.Base (AssertionFailed (..))
import qualified Data.Text              as T
import           Data.Text.Format
import qualified Data.Text.IO           as TIO
import qualified Data.Text.Lazy         as TL
import           Data.Time
import           Data.Traversable
import           Shelly                 hiding (FilePath, (<.>), (</>))
import           System.FilePath

import           Sites.Actions.Deploy   (deploySite)
import           Sites.Git
import           Sites.Utils


data DocLocation = Pre | Meta | Content

publishDraft :: FilePath -> Maybe T.Text -> T.Text -> Maybe UTCTime -> Bool
             -> IO ()
publishDraft metaFile mSrcB destB pubDate deploy = shelly $ verbosely $ do
    srcB <-  maybe (   throwMaybe (AssertionFailed "cannot find current branch")
                   =<< currentBranch) return mSrcB
    now  <-  liftIO
         $   T.pack
         .   formatTime defaultTimeLocale (iso8601DateFormat $ Just "%H:%M:%S")
         <$> maybe getCurrentTime return pubDate

    git_ "checkout" [srcB]
    withTmpDir $ \dirname -> do
        let tmpFile = replaceDirectory metaFile $ fpStr dirname
        mv (strFp metaFile) $ strFp tmpFile
        liftIO
            $   TIO.writeFile metaFile
            .   T.unlines
            .   snd
            .   mapAccumL (updateDate now) Pre
            .   T.lines
            =<< TIO.readFile tmpFile

    git_ "add"      [T.pack metaFile]
    git_ "commit"   ["-m", "Updated date of post."]
    git_ "checkout" [destB]
    git_ "merge"    [srcB]
    git_ "branch"   ["-d", srcB]

    when deploy $
        liftIO $ deploySite False False

-- git branch --list | grep '*' | cut -d ' ' -f 2
currentBranch :: Sh (Maybe T.Text)
currentBranch =   headZ
              .   fmap (T.drop 2)
              .   filter ("* " `T.isPrefixOf`)
              .   T.lines
              <$> git "branch" ["--list"]

updateDate :: T.Text -> DocLocation -> T.Text -> (DocLocation, T.Text)

updateDate _ Pre line@"---" = (Meta, line)
updateDate _ Pre line       = (Pre,  line)

updateDate date Meta line
    | "date: " `T.isPrefixOf` line =
        (Meta, TL.toStrict $ format "date: {}" $ Only date)
    | line == "---" = (Content, line)
    | otherwise = (Meta, line)

updateDate _ Content line = (Content, line)
