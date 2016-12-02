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
import           System.Environment     (unsetEnv)
import           System.FilePath

import           Sites.Actions.Deploy   (deploySite)
import           Sites.Git
import           Sites.Types
import           Sites.Utils


data DocLocation = Pre | Meta | Content

publishDraft :: FilePath -> Maybe BranchMove -> Maybe UTCTime -> Bool -> IO ()
publishDraft metaFile branch pubDate deploy = shelly $ verbosely $ do
    current <-  throwMaybe (AssertionFailed "cannot find current branch")
            =<< currentBranch
    now <-  liftIO
        $   T.pack
        .   formatTime defaultTimeLocale (iso8601DateFormat $ Just "%H:%M:%S")
        <$> maybe getCurrentTime return pubDate

    traverse (git_ "checkout" . pure . branchTo) branch

    overLines metaFile (snd . mapAccumL (updateDate now) Pre)

    git_ "add"      [T.pack metaFile]
    git_ "commit"   ["-m", "Updated date of post."]

    traverse (merge current) branch

    when deploy $ liftIO $ do
        unsetEnv "DEVELOPMENT"
        deploySite True False

overLines :: FilePath -> ([T.Text] -> [T.Text]) -> Sh ()
overLines filename f = withTmpDir $ \dirname -> do
    let tmpFile = replaceDirectory filename $ fpStr dirname
    mv (strFp filename) $ strFp tmpFile
    liftIO
        $   TIO.writeFile filename
        .   T.unlines
        .   f
        .   T.lines
        =<< TIO.readFile tmpFile

merge :: T.Text -> BranchMove -> Sh ()
merge current (BranchMove mSrcB destB) = do
    git_ "checkout" [destB]
    git_ "merge"    [srcB]
    git_ "branch"   ["-d", srcB]
    where
        srcB = fromMaybe current mSrcB

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
