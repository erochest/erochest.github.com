{-# LANGUAGE OverloadedStrings #-}


import           Control.Arrow
import           Control.Monad
import           Data.Time
import qualified Data.List as L
import           Data.Monoid
import           Hakyll
import           System.FilePath (takeFileName)
import           System.Locale


dateFormat :: String
dateFormat = "%B %e, %Y %l:%M %p"


main :: IO ()
main = hakyll $ do

    match  "index.html" $ route idRoute
    create "index.html" $ constA mempty
        >>> arr (setField "title" "Home")
        >>> requireAllA "articles/*"
                (  arr id *** (onlyPublished >>> arr ( L.take 3
                                                     . L.reverse
                                                     . L.sortBy comparePagesByDate
                                                     ))
                >>> addPostList "articles"
                                "templates/articleitem.html"
                                "templates/articlediv.html"
                )

        -- requireAllA "notes/*.md"
        >>> applyTemplateCompiler "templates/index.html"
        >>> applyTemplateCompiler "templates/default.html"
        >>> relativizeUrlsCompiler

    match "articles/*" $ do
        route   $ setExtension "html"
        compile $   pageCompiler
                >>> arr (renderDateField "published" dateFormat "unknown")
                >>> applyTemplateCompiler "templates/article.html"
                >>> applyTemplateCompiler "templates/default.html"
                >>> relativizeUrlsCompiler

    match "*.md" $ do
        route   $ setExtension "html"
        compile $   pageCompiler
                >>> applyTemplateCompiler "templates/default.html"
                >>> relativizeUrlsCompiler

    match "css/*" $ do
        route   $ setExtension "css"
        compile $ byExtension (error "Not a (S)CSS file.")
                              [ (".css",  compressCssCompiler)
                              , (".scss", compassCompiler)
                              ]

    match "templates/*" $ compile templateCompiler

    match "*.png" $ do
        route   idRoute
        compile copyFileCompiler
    match "*.ico" $ do
        route   idRoute
        compile copyFileCompiler
    match "*.txt" $ do
        route   idRoute
        compile copyFileCompiler
    match "img/*" $ do
        route   idRoute
        compile copyFileCompiler
    match "js/*" $ do
        route   idRoute
        compile copyFileCompiler

addPostList :: String -> Identifier Template -> Identifier Template
            -> Compiler (Page String, [Page String]) (Page String)
addPostList field itemTemplate wrapperTemplate = setFieldA field $
        require itemTemplate renderPost
    >>> arr mconcat
    >>> applyTemplateCompiler wrapperTemplate
    >>> arr pageBody
    where
        renderPost :: [Page String] -> Template -> [Page String]
        renderPost p t = map (applyTemplate t) p

compassCompiler :: Compiler Resource String
compassCompiler =   getResourceString
                >>> unixFilter "sass" [ "-s", "--scss", "--compass"
                                      , "--load-path", "./sass"
                                      ]
                >>> arr compressCss

-- | This compiler filters out the pages that are missing a published date for
-- whose published date is in the future.
onlyPublished :: Compiler [Page a] [Page a]
onlyPublished = unsafeCompiler $ \ps ->
    (\now -> filterM (op now) ps) =<< getCurrentTime
    where
        op :: UTCTime -> Page a -> IO Bool
        op now page =
            return . maybe False (<= now) $ getUTCMaybe defaultTimeLocale page

-- | Copied and pasted here, since it's not exported from
-- Hakyll.Web.Page.Metadata. Grr.
getUTCMaybe :: TimeLocale     -- ^ Output time locale
            -> Page a         -- ^ Input page
            -> Maybe UTCTime  -- ^ Parsed UTCTime
getUTCMaybe locale page = msum
    [ fromPublished "%a, %d %b %Y %H:%M:%S UT"
    , fromPublished "%Y-%m-%dT%H:%M:%SZ"
    , fromPublished "%B %e, %Y %l:%M %p"
    , fromPublished "%B %e, %Y"
    , getFieldMaybe "path" page >>= parseTime' "%Y-%m-%d" .
        L.intercalate "-" . take 3 . splitAll "-" . takeFileName
    ]
  where
    fromPublished f  = getFieldMaybe "published" page >>= parseTime' f
    parseTime' f str = parseTime locale f str
