{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}


module Sites.Actions.Draft where


import           Control.Lens            hiding ((.=), (<.>))
import           Control.Monad
import           Data.Aeson              hiding (encode)
import qualified Data.Aeson              as A
import           Data.Aeson.Lens
import           Data.Bifunctor
import qualified Data.ByteString.Lazy    as BL
import           Data.Char               (isSpace)
import           Data.Foldable
import qualified Data.HashMap.Strict     as M
import qualified Data.HashSet            as S
import           Data.Maybe              (fromMaybe)
import           Data.Monoid             ((<>))
import qualified Data.Text               as T
import           Data.Text.Format
import qualified Data.Text.Lazy          as TL
import           Data.Text.Lazy.Encoding (decodeUtf8)
import           Data.Time
import           Data.Yaml               hiding (decode)
import           Network.Wreq
import           Shelly                  hiding (FilePath, path, (<.>), (</>))
import           System.FilePath
import           Text.HTML.DOM           (parseLBS)
import           Text.XML.Lens           hiding ((.=), (<.>))

import           Sites.Git
import           Sites.Types
import           Sites.Utils


newDraft :: FilePath
         -> S.HashSet T.Text
         -> T.Text
         -> Maybe T.Text
         -> Maybe FilePath
         -> Bool
         -> PageType
         -> IO ()
newDraft cat tagSet title author mslug useRange pageType =
    shelly $ verbosely $
        yamlHeader title tagSet
            >>= makeDraftInfo pageType title author cat (makeSlug title mslug)
                              useRange
            >>= makeDraft

makeDraft :: DraftInfo -> Sh ()
makeDraft draft@DraftInfo{..} = chdir_p (strFp draftRepo) $ do
    void $ git "checkout" ["-b", draftBranch]
    chdir_p (strFp draftDirectory) $
        git_ "add" . map T.pack =<< draftBuilder draft
    git_ "commit" [ "-m"
                  , TL.toStrict
                  $ format "Stub draft for '{}'"
                  $ Only draftTitle
                  ]

makeDraftInfo :: PageType
              -> T.Text
              -> Maybe T.Text
              -> FilePath
              -> FilePath
              -> Bool
              -> Value
              -> Sh DraftInfo

makeDraftInfo MarkdownPage   title _ cat slug _ hdr =
    return $ singlePage title cat slug hdr $ pageBuilder "index.md"

makeDraftInfo ClojurePage    title _ cat slug _ hdr =
    return $ singlePage title cat slug hdr $ pageBuilder "index.clj"

makeDraftInfo PureScriptPage title _ cat slug _ hdr =
    return $ singlePage title cat slug hdr pursBuilder

makeDraftInfo ReadingLogPage title author _ slug useRange hdr = do
    authorInfo <- liftIO $ maybe (return Null) viaf author
    resource   <- liftIO $ worldcat (fold author) title
    let hdr' = mergeHeader hdr authorInfo resource "Work"
    return $ DraftInfo title "reading-log" branch dir hdr' readingBuilder
    where
        author' = foldMap (T.unpack . slugify . lastFirst) author
        branch  = T.pack $ "reading" </> author' </> slug
        dir     = author'

        mergeHeader :: Value -> Value -> Value -> T.Text -> Value
        mergeHeader (Object h) a r t =
            Object
                $ M.unions
                [ h
                , [ "author"   .= a
                  , "resource" .= r
                  , "typeof"   .= t
                  ]
                , if useRange
                     then maybe M.empty
                                (M.singleton "date_start")
                                (M.lookup "date" h)
                     else M.empty
                ]
        mergeHeader h _ _ _ = h

pageBuilder :: FilePath -> DraftInfo -> Sh [FilePath]
pageBuilder base DraftInfo{..} = do
    writefile (strFp base) $
        renderHeader draftHeader <> "\n<!--more-->"
    return [base]

pursBuilder :: DraftInfo -> Sh [FilePath]
pursBuilder DraftInfo{..} = do
    run_ "pulp" ["init"]
    writefile (strFp idx) . (renderHeader draftHeader <>)
        =<< readfile (strFp main)
    touchfile $ strFp keep
    return [ idx
           , main
           , keep
           , "bower.json"
           , "test/Main.purs"
           , ".gitignore"
           ]
    where
        main = "src/Main.purs"
        idx  = "index.purs"
        keep = "src/.keep"

readingBuilder :: DraftInfo -> Sh [FilePath]
readingBuilder DraftInfo{..} = do
    writefile (strFp title) $
        renderHeader draftHeader
    return [title]
    where
        title = T.unpack (slugify draftTitle) <.> "md"

renderHeader :: Value -> T.Text
renderHeader = TL.toStrict
             . format "---\n{}---\n\n"
             . Only
             . decodeUtf8
             . BL.fromStrict
             . encode

yamlHeader :: T.Text -> S.HashSet T.Text -> Sh Value
yamlHeader title tags = do
    now <-  liftIO
        $   formatTime defaultTimeLocale (iso8601DateFormat (Just "%H:%M:%SZ"))
        <$> getCurrentTime
    return $ object [ "title"       .= title
                    , "date"        .= now
                    , "categories"  .= S.toList tags
                    ]

singlePage :: T.Text
           -> FilePath
           -> FilePath
           -> Value
           -> (DraftInfo -> Sh [FilePath])
           -> DraftInfo
singlePage title cat slug = DraftInfo title "." branch dir
    where
        dir = "posts" </> cat </> slug
        branch = T.pack $ "draft" </> cat </> slug

makeSlug :: T.Text -> Maybe FilePath -> FilePath
makeSlug title = fromMaybe (T.unpack $ slugify title)

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
                              & param "fq"     .~ ["x0:book"]
        q = TL.toStrict $ format "ti:{} au:{}" (title, author)
