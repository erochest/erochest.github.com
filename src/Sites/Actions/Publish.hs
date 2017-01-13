{-# LANGUAGE OverloadedStrings #-}


module Sites.Actions.Publish where


import           Control.Exception.Base (AssertionFailed (..))
import           Control.Monad          (void, when)
import           Data.Maybe
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
    now <-  liftIO
        $   T.pack
        .   formatTime defaultTimeLocale (iso8601DateFormat $ Just "%H:%M:%S")
        <$> maybe getCurrentTime return pubDate

    chdir dir $ do
        current <-  throwMaybe (AssertionFailed "cannot find current branch")
                =<< currentBranch
        void $ traverse (merge current) branch

    overLines metaFile (snd . mapAccumL (updateDate now) Pre)

    chdir dir $ do
        git_ "add"      [T.pack metaFile]
        git_ "commit"   ["-m", "Updated date of post."]
    when (toTextIgnore dir /= ".") $ do
        git_ "add" [toTextIgnore dir]
        git_ "commit" [ "-m"
                      , TL.toStrict $ format "Published {}." (Only metaFile)
                      ]

    when deploy $ liftIO $ do
        unsetEnv "DEVELOPMENT"
        deploySite True False
    git_ "push" []

    where
        dir = maybe "." strFp
            $ listToMaybe
            $ dropWhile (== "/")
            $ splitPath metaFile

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

updateDate :: T.Text -> DocLocation -> T.Text -> (DocLocation, T.Text)

updateDate _ Pre line@"---" = (Meta, line)
updateDate _ Pre line       = (Pre,  line)

updateDate date Meta line
    | "date: " `T.isPrefixOf` line =
        (Meta, TL.toStrict $ format "date: {}" $ Only date)
    | line == "---" = (Content, line)
    | otherwise = (Meta, line)

updateDate _ Content line = (Content, line)
