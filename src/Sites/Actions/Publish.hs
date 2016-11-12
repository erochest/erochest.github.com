{-# LANGUAGE OverloadedStrings #-}


module Sites.Actions.Publish where


import qualified Data.Text            as T
import           Data.Text.Format
import qualified Data.Text.IO         as TIO
import qualified Data.Text.Lazy       as TL
import           Data.Time
import           Data.Traversable
import           Shelly               hiding (FilePath, (<.>), (</>))
import qualified Shelly               as Sh
import           System.FilePath

import           Sites.Actions.Deploy (deploySite)
import           Sites.Git


data DocLocation = Pre | Meta | Content

publishDraft :: FilePath -> T.Text -> T.Text -> Maybe UTCTime -> Bool -> IO ()
publishDraft metaFile srcB destB pubDate deploy = shelly $ verbosely $ do
    now <-  liftIO
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

fpStr :: Sh.FilePath -> FilePath
fpStr = T.unpack . toTextIgnore

strFp :: FilePath -> Sh.FilePath
strFp = fromText . T.pack

updateDate :: T.Text -> DocLocation -> T.Text -> (DocLocation, T.Text)

updateDate _ Pre line@"---" = (Meta, line)
updateDate _ Pre line       = (Pre,  line)

updateDate date Meta line
    | "date: " `T.isPrefixOf` line =
        (Meta, TL.toStrict $ format "date: {}" $ Only date)
    | line == "---" = (Content, line)
    | otherwise = (Meta, line)

updateDate _ Content line = (Content, line)