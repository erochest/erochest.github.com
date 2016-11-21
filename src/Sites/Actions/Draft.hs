{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}


module Sites.Actions.Draft where


import           Data.Char         (isAlphaNum)
import qualified Data.HashSet      as S
import qualified Data.List         as L
import           Data.Maybe        (fromMaybe)
import           Data.Monoid       ((<>))
import qualified Data.Text         as T
import           Data.Text.Format
import qualified Data.Text.Lazy    as TL
import qualified Data.Text.Lazy.IO as TLIO
import           Data.Time
import           Shelly            hiding (FilePath, path, (<.>), (</>))
import           System.Directory
import           System.FilePath

import           Sites.Git
import           Sites.Types
import           Sites.Utils


newDraft :: FilePath -> S.HashSet T.Text -> T.Text -> Maybe FilePath -> PageType
         -> IO ()
newDraft cat tagSet title mslug pageType = shelly $ verbosely $ do
    now <-  liftIO
        $   formatTime defaultTimeLocale (iso8601DateFormat (Just "%H:%M:%SZ"))
        <$> getCurrentTime
    git_ "checkout" ["-b", brch]
    liftIO $ createDirectoryIfMissing True cdir
    names <- chdir (strFp cdir) $
                newPage pageType title tagSet now
    mapM_ (git_ "add" . pure . T.pack . (cdir </>)) names
    git_ "commit" ["-m", TL.toStrict $ format "Stub for '{}'." $ Only title]
    where
        slug = fromMaybe (T.unpack $ slugify title) mslug
        cdir = "posts" </> cat </> slug
        brch = T.pack $ "draft" </> cat </> slug

newPage :: PageType -> T.Text -> S.HashSet T.Text -> String -> Sh [FilePath]

newPage PureScriptPage title tags timestamp = do
    run_ "pulp" ["init"]
    writefile (strFp index)
        .   (TL.toStrict (yamlHeader title tags timestamp) <>)
        =<< readfile (strFp main)
    touchfile $ strFp keep
    return [ index
           , main
           , keep
           , "bower.json"
           , "test/Main.purs"
           , ".gitignore"
           ]
    where
        main  = "src/Main.purs"
        index = "index.purs"
        keep  = "src/.keep"

newPage _ title tags timestamp = do
    liftIO $ TLIO.writeFile index
           $ yamlHeader title tags timestamp <> "<!--more-->\n"
    return [index]
    where
        index = "index.md"

yamlHeader :: T.Text -> S.HashSet T.Text -> String -> TL.Text
yamlHeader title tags timestamp =
    format "---\ntitle: {}\ndate: {}\ncategories: {}\n---\n\n"
           (title, timestamp, setList tags)

setList :: S.HashSet T.Text -> T.Text
setList = T.intercalate " " . L.sort . S.toList

slugify :: T.Text -> T.Text
slugify = T.map slugChar . T.toLower
    where
        slugChar x = if isAlphaNum x
                        then x
                        else '-'
