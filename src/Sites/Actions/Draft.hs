{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}


module Sites.Actions.Draft where


import           Data.Char         (isAlphaNum)
import qualified Data.HashSet      as S
import qualified Data.List         as L
import           Data.Maybe        (fromMaybe)
import qualified Data.Text         as T
import           Data.Text.Format
import qualified Data.Text.Lazy    as TL
import qualified Data.Text.Lazy.IO as TLIO
import           Data.Time
import           Shelly            hiding (FilePath, path, (<.>), (</>))
import           System.Directory
import           System.FilePath

import           Sites.Git


newDraft :: FilePath -> S.HashSet T.Text -> T.Text -> Maybe FilePath -> IO ()
newDraft cat tagSet title mslug = shelly $ verbosely $ do
    now <-  liftIO
        $   formatTime defaultTimeLocale (iso8601DateFormat (Just "%H:%M:%SZ"))
        <$> getCurrentTime
    git_ "checkout" ["-b", T.pack slug]
    liftIO $ createDirectoryIfMissing True cdir
    liftIO
        $ TLIO.writeFile path
        $ format "---\ntitle: {}\ndate: {}\ncategories: {}\n---\n\n<!--more-->\n"
        ( title, now, tags)
    git_ "add" [T.pack path]
    git_ "commit" ["-m", TL.toStrict $ format "Stub for '{}'." $ Only title]
    where
        slug = fromMaybe (T.unpack $ slugify title) mslug
        cdir = "posts" </> cat </> slug
        path = cdir </> "index.md"
        tags = T.intercalate " " $ L.sort $ S.toList tagSet

slugify :: T.Text -> T.Text
slugify = T.map slugChar . T.toLower
    where
        slugChar x = if isAlphaNum x
                        then x
                        else '-'
