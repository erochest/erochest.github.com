{-# LANGUAGE OverloadedStrings #-}


module Sites.Actions.WorkDraft where


import           Control.Lens         hiding ((.=), (<.>))
import           Data.Aeson           hiding (encode)
import qualified Data.Aeson           as A
import           Data.Aeson.Lens
import           Data.Bifunctor
import qualified Data.ByteString.Lazy as BL
import           Data.Char
import           Data.Foldable
import qualified Data.HashSet         as S
import           Data.Monoid
import qualified Data.Text            as T
import           Data.Text.Encoding
import           Data.Text.Format
import qualified Data.Text.Lazy       as TL
import           Data.Time
import           Data.Yaml            hiding (decode)
import           Network.Wreq
import           Shelly               hiding (trace, (<.>), (</>))
import           System.FilePath
import           Text.HTML.DOM        (parseLBS)
import           Text.XML.Lens        hiding ((.=), (<.>))

import           Sites.Git
import           Sites.Utils


workDraft :: T.Text -> T.Text -> S.HashSet T.Text -> Bool -> IO ()
workDraft author title tags useRange = shelly $ verbosely $ do
    now <-  liftIO
        $   formatTime defaultTimeLocale (iso8601DateFormat (Just "%H:%M:%SZ"))
        <$> getCurrentTime
    hdr <- readingHeader author title tags now useRange
    chdir_p (strFp cdir) $ do
        git_ "checkout" ["-b", branch]
        writefile (strFp draft) $
            TL.toStrict hdr <> "\n<!--more-->\n"
        git_ "add" [T.pack draft]
        git_ "commit" ["-m", message]
    where
        author' = T.unpack $ slugify $ lastFirst author
        title'  = T.unpack $ slugify title
        branch  = T.pack   $ "reading" </> author' </> title'
        cdir    = "reading-log" </> author'
        draft   = title' <.> "md"
        message = TL.toStrict $ format "Stub for '{}'." $ Only title

lastFirst :: T.Text -> T.Text
lastFirst = T.intercalate " "
          . fold
          . fmap (uncurry (flip (:)))
          . lastElem
          . T.split isSpace
    where
        lastElem :: [a] -> Maybe ([a], a)
        lastElem [x]    = Just ([], x)
        lastElem (x:xs) = first (x:) <$> lastElem xs
        lastElem []     = Nothing

readingHeader :: T.Text -> T.Text -> S.HashSet T.Text -> String -> Bool
              -> Sh TL.Text
readingHeader author title tags timestamp useRange = do
    authorInfo <- liftIO $ viaf author
    resource   <- liftIO $ worldcat author title
    return
        $  format "---\n{}---\n"
        $  Only
        $  decodeUtf8
        $  encode
        $  object
        $  [ "title"    .= title
           , "author"   .= authorInfo
           , "date"     .= timestamp
           , "resource" .= resource
           , "typeof"   .= ("Work" :: T.Text)
           , "tags"     .= S.toList tags
           ]
        ++ [ "date_start" .= timestamp | useRange
           ]

viaf :: T.Text -> IO Value
viaf author =
    maybe (error "oh, noes, mr. bill") (vObj author)
        .   preview ( responseBody . jsonl
                    . key "searchRetrieveResponse"
                    . key "records"
                    . _Array
                    . traverse
                    . key "record"
                    . key "recordData"
                    . key "viafID"
                    . key "#text"
                    . _String
                    )
        <$> getWith opts searchUri
    where
        jsonl :: Prism' BL.ByteString Value
        jsonl = prism' A.encode decode

        viafUri   = "http://viaf.org"
        searchUri = viafUri <> "/viaf/search"
        q = "local.personalNames all " <> "\"" <> author <> "\""
        opts = defaults & param "query"        .~ [q]
                        & param "sortKeys"     .~ ["holdingscount"]
                        & param "recordSchema" .~ ["BriefVIAF"]
                        & header "Accept"      .~ ["application/json"]
        vObj a p = object [ "name"     .= a
                          , "resource" .= (T.pack viafUri <> "/viaf/" <> p)
                          , "typeof"   .= ("Person" :: T.Text)
                          ]

worldcat :: T.Text -> T.Text -> IO Value
worldcat author title =
    maybe Null (String . (wcUri <>) . ("/oclc/" <>))
        .   preview ( responseBody . to parseLBS . root . entire
                    . attributeIs "class" "result details" . entire
                    . attributeIs "class" "oclc_number"
                    . text
                    )
        <$> getWith searchOpts searchUri
    where
        wcUri      = "http://www.worldcat.org"
        searchUri  = T.unpack $ wcUri <> "/search"
        searchOpts = defaults & param "q"      .~ [q]
                              & param "qt"     .~ ["advanced"]
                              & param "dblist" .~ ["638"]
        q = TL.toStrict $ format "ti:{} au:{}" (title, author)
