{-# LANGUAGE OverloadedStrings #-}


import           Control.Arrow
import           Control.Monad
import           Data.Monoid
import           Data.Time
import qualified Data.List as L
import           Data.Monoid
import           Hakyll
import           System.FilePath (takeFileName)
import           System.Locale


dateFormat :: String
dateFormat = "%B %e, %Y %l:%M %p"

-- | This takes a list of published or draft resources and filters out the
-- drafts, sorts them, and slices them. It then feeds each on to an item
-- template, concatenates the output, and feeds that to a wrapper template. The
-- output of the wrapper template is assigned to a field in the page.
getPublishedList :: String
                 -- ^ The field on the page to assign the output to.
                 --
                 -- By convention, this also determines several other values.
                 -- For the field FIELD:
                 --
                 -- * FIELD/* is the pattern for the resources;
                 -- * template/FIELD-item.html is the template for the items;
                 -- * template/FIELD-div.html is the template for the wrapper.
                 -> ([Page String] -> [Page String])
                 -- ^ A transformation function that slices out and returns the
                 -- resources to display.
                 -> Compiler (Page String) (Page String)
getPublishedList field = getPublishedList' field inputs itemt divt
    where
        inputs = parseGlob       $ field ++ "/*"
        itemt  = parseIdentifier $ "templates/" ++ field ++ "-item.html"
        divt   = parseIdentifier $ "templates/" ++ field ++ "-div.html"

-- | This is a version of getPublishedList that eschews convention and insists
-- that you specify the parameters.
getPublishedList' :: String
                  -> Pattern (Page String)
                  -> Identifier Template
                  -> Identifier Template
                  -> ([Page String] -> [Page String])
                  -> Compiler (Page String) (Page String)
getPublishedList' field inputs itemt divt transform =
    requireAllA inputs
        (arr id *** sliceA transform >>> addPostList field itemt divt)

-- | This takes a list transformation function and wraps it in an arrow that
-- filters out the unpublished pages, reverse sorts it, and transforms the list
-- with the function.
sliceA :: ([Page a] -> [Page a]) -> Compiler [Page a] [Page a]
sliceA transform =   onlyPublished
                 >>> arr (transform . L.reverse . L.sortBy comparePagesByDate)

main :: IO ()
main = hakyll $ do

    match  "index.html" $ route idRoute
    create "index.html" $ constA mempty
        >>> arr (setField "title" "Home")

        >>> getPublishedList "articles" (L.take 3)
        >>> getPublishedList "notes"    (L.take 5)

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
    
    match  "articles/index.html" $ route idRoute
    create "articles/index.html" $
        indexPage "Articles" 10 ("articles/*" <> complement "articles/index.html")
                  "templates/articles-item.html" "templates/articles-div.html"

    match  "notes/index.html" $ route idRoute
    create "notes/index.html" $
        indexPage "Notes" 10 ("notes/*" <> complement "notes/index.html")
                  "templates/notes-item.html" "templates/notes-div.html"

    match "notes/*" $ do
        route   $ setExtension "html"
        compile $   pageCompiler
                >>> arr (renderDateField "published" dateFormat "unknown")
                >>> applyTemplateCompiler "templates/note.html"
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

-- | This compiler takes a list of items and creates an index page for the
-- first N.
indexPage :: String
          -- ^ Title
          -> Int
          -- ^ The number of items to index.
          -> Pattern (Page String)
          -- ^ The resources to index.
          -> Identifier Template
          -- ^ The item template.
          -> Identifier Template
          -- ^ The wrapper template.
          -> Compiler () (Page String)
indexPage title n resources itemt divt = constA mempty
    >>> arr (setField "title" title)
    >>> getPublishedList' "items" resources itemt divt (take n)
    >>> applyTemplateCompiler "templates/list.html"
    >>> applyTemplateCompiler "templates/default.html"
    >>> relativizeUrlsCompiler

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
