{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}


-- TODO: clean this up, order it, and add headings
module Sites.Utils
    ( baseUrl
    , parallaxScript
    , cleanRoute
    , createIndexRoute
    , createBaseIndexRoute
    , cleanIndexUrls
    , cleanIndexHtmls
    , cleanIndex
    , pandocHtml
    , pandocClean
    , style
    , sassCompiler
    , compilePage
    , compilePage'
    , compilePost
    , compilePost'
    , profileContext
    , siteContext
    , foldMetadataField
    , errMetadataField
    , reformatDate
    , parseDateLax
    , formatTime'
    , extraHeaderContext
    , openGraph
    , openGraphContext
    , loadAndApplyDefault
    , postTemplate
    , pursTemplate
    , indexTemplate
    , livereload
    , analytics
    , developmentField
    , indexFileName
    , meta
    , guessUrl
    , iso8601
    , getCategory
    , getTags'
    , throwMaybe
    , fpStr
    , strFp
    , mnot
    , lookupDefined
    ) where


import           Control.Applicative
import           Control.Exception.Safe
import           Control.Monad
import           Data.Aeson.Types       (Value (..))
import           Data.Foldable
import qualified Data.HashMap.Strict    as M
import           Data.List              (isSuffixOf)
import           Data.Maybe             (fromMaybe)
import           Data.Monoid
import qualified Data.Text              as T
import           Data.Text.Format
import qualified Data.Text.Lazy         as TL
import           Data.Time.Clock        (UTCTime)
import           Data.Time.Format       (defaultTimeLocale, formatTime,
                                         parseTimeM)
import           Hakyll
import           Lucid
import           Lucid.Base             (makeAttribute)
import qualified Shelly                 as Sh
import           System.Environment
import           System.FilePath


parallaxScript :: String
parallaxScript = "$(document).ready(function() {\
                 \  $('.parallax').parallax();\
                 \});"

cleanRoute :: Routes
cleanRoute = customRoute createIndexRoute

createIndexRoute :: Identifier -> FilePath
createIndexRoute ident =
    takeDirectory p </> takeBaseName p </> "index.html"
    where p = toFilePath ident

createBaseIndexRoute :: Identifier -> FilePath
createBaseIndexRoute ident = takeBaseName (toFilePath ident) </> "index.html"

cleanIndexUrls :: Item String -> Compiler (Item String)
cleanIndexUrls = return . fmap (withUrls cleanIndex)

cleanIndexHtmls :: Item String -> Compiler (Item String)
cleanIndexHtmls = return . fmap (replaceAll "/index.html" replacement)
    where
        replacement = const "/"

cleanIndex :: String -> String
cleanIndex url
    | idx `isSuffixOf` url = take (length url - length idx) url
    | otherwise            = url
    where
        idx = "index.html"

pandocHtml :: Rules ()
pandocHtml = do
    route   $   setExtension "html"
    compile $   pandocCompiler
            >>= loadAndApplyDefault (siteContext Nothing Nothing)
            >>= relativizeUrls
            >>= cleanIndexUrls

pandocClean :: Rules ()
pandocClean = do
    route       cleanRoute
    compile $   pandocCompiler
            >>= loadAndApplyDefault (siteContext Nothing Nothing)
            >>= relativizeUrls
            >>= cleanIndexUrls

sassCompiler :: Compiler (Item String)
sassCompiler =
        getResourceString >>=
        withItemBody (unixFilter "sass" [ "--scss"
                                        , "--stdin"
                                        , "--load-path", "sass/"
                                        ])

style :: String -> String
style url =  "<link rel=\"stylesheet\" href=\"" <> url <> "\">"

compilePage :: Compiler (Item String)
compilePage =  compilePage' $ siteContext Nothing Nothing

compilePage' :: Context String -> Compiler (Item String)
compilePage' c =   pandocCompiler
               >>= saveSnapshot "content"
               >>= loadAndApplyDefault c
               >>= relativizeUrls
               >>= cleanIndexUrls

compilePost :: Compiler (Item String)
compilePost = compilePost' $ siteContext Nothing Nothing

baseUrl :: T.Text
baseUrl = "http://www.ericrochester.com/"

guessUrl :: Identifier -> Compiler T.Text
guessUrl = fmap (mappend baseUrl . T.pack . fold) . getRoute

iso8601 :: Identifier -> Compiler T.Text
iso8601 = fmap (T.pack . formatTime defaultTimeLocale "%FT%TZ")
        . getItemUTC defaultTimeLocale

getCategory :: Identifier -> T.Text
getCategory ident
    | "reading-log/" `T.isPrefixOf` identT = "reading"
    | otherwise = T.pack . takeFileName . takeDirectory $ takeDirectory identS
    where
        identS = toFilePath ident
        identT = T.pack identS

getTags' :: Identifier -> Compiler [T.Text]
getTags' ident = do
    m <- getMetadata ident
    return $ case M.lookup "tags" m <|> M.lookup "categories" m of
                  Just (Object _) -> mzero
                  Just (Array  a) -> toList . fold $ mapM string a
                  Just (String s) -> filter (not . T.null)
                                  .  map T.strip
                                  $  T.split splitter s
                  Just (Number _) -> mzero
                  Just (Bool   _) -> mzero
                  Just Null       -> mzero
                  Nothing         -> mzero
    where
        splitter ',' = True
        splitter ' ' = True
        splitter _   = False

string :: Value -> Maybe T.Text
string (Object _) = Nothing
string (Array  _) = Nothing
string (String s) = Just s
string (Number _) = Nothing
string (Bool   _) = Nothing
string Null       = Nothing

openGraphContext :: Compiler (Context String)
openGraphContext = do
    ident <-  getUnderlying
    m     <-  getMetadata ident
    url   <-  guessUrl    ident
    pub   <-  iso8601     ident
    tags  <-  getTags'    ident
    let title = lookupm "<Untitled>" "title" m
        image = lookupm (baseUrl <> "img/about.jpg") "banner-image" m
        sect  = getCategory ident
    return
        . openGraph
        $ [ ("og:title",               title)
          , ("og:type",                "article")
          , ("og:image",               image)
          , ("og:url",                 url)
          , ("og:site_name",           "e.")
          , ("article:published_time", pub)
          , ("article:author",         baseUrl <> "about/")
          , ("article:section",        sect)
          ] ++ map ("article:tag", ) tags
    where
        lookupm :: T.Text -> T.Text -> Metadata -> T.Text
        lookupm d k m = fromMaybe d
                      $ case M.lookup k m of
                             Just v  -> string v
                             Nothing -> Nothing

compilePost' :: Context String -> Compiler (Item String)
compilePost' c = do
    og <- openGraphContext
    let c' =  constField "extraScript" parallaxScript
           <> og
           <> boolField "noContainer"  (const True)
           <> c
    pandocCompiler
        >>= saveSnapshot "content"
        >>= postTemplate c'
        >>= relativizeUrls
        >>= cleanIndexUrls

openGraph :: [(T.Text, T.Text)] -> Context String
openGraph = constField "open-graph"
          . TL.unpack
          . renderText
          . foldMap (\(p, v) -> meta_ [property_ p, content_ v])

profileContext :: Context String
profileContext = openGraph
               [ ("profile:first_name", "Eric")
               , ("profile:last_name",  "Rochester")
               , ("profile:username",   "erochest")
               , ("profile:username",   "erochester")
               ]

property_ :: T.Text -> Attribute
property_ = makeAttribute "property"

siteContext :: Maybe String -> Maybe String -> Context String
siteContext title extraHeader =
           dateField "date" "%e %B %Y"
        <> dateField "datetime" "%Y-%m-%dT%H:%M:%SZ"
        <> mconcat [ field "coverImage"  (errMetadataField "cover-image" )
                   , field "bannerImage" (errMetadataField "banner-image")
                   ]
        <> extraHeaderContext extraHeader
        <> foldMap (constField "title") title
        <> constField "open-graph" ""
        <> livereload
        <> analytics
        <> defaultContext

foldMetadataField :: String -> Item a -> Compiler String
foldMetadataField f = fmap fold . (`getMetadataField` f) . itemIdentifier

errMetadataField :: String -> Item a -> Compiler String
errMetadataField f = (`getMetadataField'` f) . itemIdentifier

extraHeaderContext :: Maybe String -> Context String
extraHeaderContext = constField "extra-header" . fromMaybe ""

reformatDate :: String -> String
reformatDate dateStamp = maybe dateStamp formatTime' $ parseDateLax dateStamp

parseDateLax :: String -> Maybe UTCTime
parseDateLax dateStamp = msum $ map (`parse` dateStamp) formats
    where parse   = parseTimeM True defaultTimeLocale
          formats = [ "%a, %d %b %Y %H:%M:%S UT"
                    , "%Y-%m-%dT%H:%M:%SZ"
                    , "%Y-%m-%d %H:%M:%S"
                    , "%Y-%m-%d"
                    , "%B %e, %Y %l:%M %p"
                    , "%B %e, %Y"
                    ]

formatTime' :: UTCTime -> String
formatTime' = formatTime defaultTimeLocale "%e %b %Y"

loadAndApplyDefault :: Context a -> Item a -> Compiler (Item String)
loadAndApplyDefault = loadAndApplyTemplate "templates/default.html"

postTemplate :: Context String -> Item String -> Compiler (Item String)
postTemplate c =
    loadAndApplyTemplate "templates/default.html" c
        <=< loadAndApplyTemplate "templates/post.html" c

pursTemplate :: Context String -> Item String -> Compiler (Item String)
pursTemplate c =
    loadAndApplyTemplate "templates/default.html" c
        <=< loadAndApplyTemplate "templates/purescript.html" c

indexTemplate :: Context String -> Item String -> Compiler (Item String)
indexTemplate c =
    loadAndApplyTemplate "templates/default.html" c
        <=< loadAndApplyTemplate "templates/post-list.html" c

livereload :: Context a
livereload = developmentField "livereload" livereload' ""
    where
        livereload' :: String
        livereload' = "\
            \<script>\n\
            \  document.write('<script src=\"http://' + (location.host || 'localhost').split(':')[0] +\n\
            \    ':35729/livereload.js?snipver=1\"></' + 'script>')\n\
            \</script>"

analytics :: Context a
analytics = field "analytics" $ \_ -> do
    code <-  unsafeCompiler
         $   (*>)
         <$> fmap mnot (lookupEnv "DEVELOPMENT")
         <*> lookupEnv "ANALYTICS_CODE"
    return $ foldMap google code
    where
        google c = "\
        \<script>\n\
        \    (function(b,o,i,l,e,r){b.GoogleAnalyticsObject=l;b[l]||(b[l]=\n\
        \    function(){(b[l].q=b[l].q||[]).push(arguments)});b[l].l=+new Date;\n\
        \    e=o.createElement(i);r=o.getElementsByTagName(i)[0];\n\
        \    e.src='https://www.google-analytics.com/analytics.js';\n\
        \    r.parentNode.insertBefore(e,r)}(window,document,'script','ga'));\n\
        \    ga('create','" ++ c ++ "','auto');ga('send','pageview');\n\
        \</script>\n\
        \ "

developmentField :: String -> String -> String -> Context a
developmentField name dev prod = field name $ \_ ->
    maybe prod (const dev) <$> unsafeCompiler (lookupDefined "DEVELOPMENT")

indexFileName :: FilePath -> PageNumber -> Identifier
indexFileName root 1 = fromFilePath $ root </> "index.html"
indexFileName root n = fromFilePath
                     . (root </>)
                     . TL.unpack
                     . format "index-{}.html"
                     . Only
                     $ left 3 '0' n

meta :: String -> Item a -> Compiler String
meta k = (`getMetadataField'` k) . itemIdentifier

throwMaybe :: (MonadThrow m, Exception e) => e -> Maybe a -> m a
throwMaybe _ (Just a) = return a
throwMaybe e Nothing  = throwM e

fpStr :: Sh.FilePath -> FilePath
fpStr = T.unpack . Sh.toTextIgnore

strFp :: FilePath -> Sh.FilePath
strFp = Sh.fromText . T.pack

lookupDefined :: String -> IO (Maybe String)
lookupDefined key = do
    value <- lookupEnv key
    return $ case value of
                Just "" -> Nothing
                _       -> value

mnot :: (Eq m, Monoid m) => Maybe m -> Maybe m
mnot = maybe (Just mempty) $ \case
                                m | m == mempty -> Just mempty
                                  | otherwise   -> Nothing
