{-# LANGUAGE OverloadedStrings #-}

module Sites.Erochest
    ( erochestSite
    ) where


import           Control.Applicative
import           Control.Monad
import           Data.Char (toLower)
import qualified Data.List as L
import           Data.Maybe (fromMaybe, mapMaybe)
import           Data.Monoid
import qualified Data.Text.Lazy as TL
import           Data.Time.Clock (UTCTime)
import           Data.Time.Format (formatTime, parseTime)
import           Hakyll
import           Prelude hiding (FilePath)
import           Shelly
import           Sites.Base
import           Sites.Pager
import           Sites.Types
import           System.FilePath (replaceExtension, takeBaseName, takeDirectory)
import           System.Locale
import           Text.Blaze.Html.Renderer.String (renderHtml)


erochestSite :: IO SiteInfo
erochestSite =
        Site "erochest" "erochest.github.com" "." `fmap` rules

pageLength :: Int
pageLength = 25

openIdHeaders :: String
openIdHeaders = "<link rel=\"openid.server\" href=\"http://www.myopenid.com/server\" /> \
    \ <link rel=\"openid.delegate\" href=\"http://erochest.myopenid.com/\" /> \
    \ <link rel=\"openid2.local_id\" href=\"http://erochest.myopenid.com\" /> \
    \ <link rel=\"openid2.provider\" href=\"http://www.myopenid.com/server\" /> \
    \ <meta http-equiv=\"X-XRDS-Location\" content=\"http://www.myopenid.com/xrds?username=erochest.myopenid.com\" />"

-- | Borrowed heavily from applyJoinTemplateList
renderPanes :: Template -> Context c -> [Item c] -> Compiler (Item String)
renderPanes template baseContext items =
    zipWithM (applyTemplate template) (zipWith context ([0..] :: [Int]) items) items >>=
    makeItem . concatMap itemBody
    where paneClasses 0 = " main"
          paneClasses 1 = " third"
          paneClasses 2 = " second"
          paneClasses 4 = " second third"
          paneClasses 6 = " second"
          paneClasses 7 = " second third"
          paneClasses _ = ""
          context pos item =  constField "pos-class" (paneClasses pos)
                           <> constField "url" (url item)
                           <> baseContext
          url = toUrl . (`replaceExtension` ".html") . show . itemIdentifier

loadPageContent :: Compiler [Item String]
loadPageContent =
        loadAllSnapshots ("pages/**/*.md" .&&. hasNoVersion) "content"

loadSnippets :: Int -> Compiler [Item String]
loadSnippets lineCount =
            loadAll ("pages/**/*.md" .&&. hasVersion "raw")
        >>= mapM (withItemBody shorten)
        >>= mapM (return . renderPandoc)
    where shorten = return . unlines . take lineCount . lines

compileIndex :: Context String -> Template -> Compiler (Item String)
compileIndex context template =
        take 8 <$> loadSnippets 19 >>=
        renderPanes template context >>=
        loadAndApplyTemplate "templates/default.html" context >>=
        relativizeUrls

listCategoryPagesT :: FilePath -> Sh [TL.Text]
listCategoryPagesT rootDir = do
        children <- ls rootDir
        map toTextIgnore <$> ((++) <$> filterM test_f children
                                   <*> (fmap concat <$> mapM ls =<< filterM test_d children))

indexPageInfo :: IO (Int, [String])
indexPageInfo = do
    pages <- shelly $ map TL.unpack . filter (TL.isSuffixOf ".md") <$> listCategoryPagesT "pages/"
    let (d, m)    = length pages `divMod` pageLength
        indexPageCount = d + if m == 0 then 0 else 1
        indexPages     = "pages/index.html" : [ "pages/index-" <> show n <> ".html"
                                              | n <- take (indexPageCount - 1) [1..]
                                              ]
    return (indexPageCount, indexPages)

-- include the pagination links: << < (n-2) (n-1) n (n+1) (n+2) > >>
compilePageIndex :: Int -> Compiler (Item String)
compilePageIndex pageCount = do
        pageN        <- getPageNumber . toFilePath <$> getUnderlying
        itemTemplate <- loadBody "templates/pages-index-item.html"
        let context =  (constField "pager" . renderHtml . pager pageCount pageN $ getPage "/pages/index")
                    <> siteContext Nothing
        take pageLength . drop (pageN * pageLength) <$> loadPageContent
            >>= applyTemplateList itemTemplate context
            >>= makeItem
            >>= loadAndApplyTemplate "templates/pages-index.html" context
            >>= loadAndApplyTemplate "templates/default.html" context

getCategoryPage :: String -> Identifier
getCategoryPage cat = fromFilePath $ "categories/" ++ cat' ++ ".html"
    where cat' = map toLower cat

getCategory' :: MonadMetadata m => Identifier -> m [String]
getCategory' =
    return . return . takeBaseName . takeDirectory . takeDirectory . toFilePath

renderCategory :: String -> String -> Int -> Int -> Int -> String
renderCategory tag url count minCount maxCount = lia url tag $ show count

joinCategories :: [String] -> String
joinCategories = L.concat

lia :: String -> String -> String -> String
lia url title parens =
        "<li><a href='" <> url <> "'>" <> title <> "</a> (" <> parens <> ")</li>"

reformatDate :: String -> String
reformatDate dateStamp =
          maybe dateStamp formatTime'
        . msum
        $ map (`parseTime'` dateStamp) formats
    where
        -- Have to use type declarations so that parseTime and formatTime know
        -- what to convert to/from.

        parseTime' :: String -> String -> Maybe UTCTime
        parseTime'  = parseTime defaultTimeLocale

        formatTime' :: UTCTime -> String
        formatTime' = formatTime defaultTimeLocale "%e %b %Y"

        formats     = [ "%a, %d %b %Y %H:%M:%S UT"
                      , "%Y-%m-%dT%H:%M:%SZ"
                      , "%Y-%m-%d %H:%M:%S"
                      , "%Y-%m-%d"
                      , "%B %e, %Y %l:%M %p"
                      , "%B %e, %Y"
                      ]

renderIds :: (MonadMetadata m, Applicative m) => String -> [Identifier] -> m String
renderIds name ids = L.concat <$> mapM renderId ids

renderId :: (MonadMetadata m, Applicative m) => Identifier -> m String
renderId id =
        lia url <$> getMetadataField' id "title"
                <*> fmap reformatDate (getMetadataField' id "date")
    where url = '/' : replaceExtension (show id) ".html"


rules :: IO (Rules ())
rules = do
    (indexPageCount, indexPages) <- indexPageInfo
    return $ do
    create ["index.html"] $ do
        let headers = Just $ style "css/index.css" ++ openIdHeaders
        route       idRoute
        compile $   loadBody "templates/index-pane.html"
                >>= compileIndex (siteContext headers)

    create (map fromFilePath indexPages) $ do
        route     idRoute
        compile $ compilePageIndex indexPageCount

    create ["atom.xml"] $ do
        route idRoute
        compile $
            let context = bodyField "description" <> siteContext Nothing
                config  = FeedConfiguration "Eric Rochester"
                                            "Feed for my site."
                                            "Eric Rochester"
                                            "erochest@gmail.com"
                                            "http://www.ericrochester.com/"
            in  take 10 <$> loadPageContent >>= renderAtom config context

    match "pages/**/*.md" $ do
        route   $ setExtension "html"
        compile   compilePage

    match "pages/**/*.md" $ version "raw" $ do
        route   idRoute
        compile getResourceBody

    match "sass/index.scss" $ do
        route   $ constRoute "css/index.css"
        compile   sassCompiler

    -- This section creates the categories.
    categories <- buildTagsWith getCategory' "pages/**/*.md" getCategoryPage

    create ["categories/index.html"] $ do
        route idRoute
        compile $
            let context =  constField "title" "Categories"
                        <> siteContext Nothing
            in     renderTags renderCategory joinCategories categories
               >>= makeItem
               >>= loadAndApplyTemplate "templates/category-index.html" context
               >>= loadAndApplyTemplate "templates/default.html" context
               >>= relativizeUrls

    forM_ (tagsMap categories) $ \(catName, catIds) ->
        create [tagsMakeId categories catName] $ do
            route idRoute
            compile $
                let context =  constField "title" ("Category: " ++ catName)
                            <> siteContext Nothing
                in     renderIds catName catIds
                   >>= makeItem
                   >>= loadAndApplyTemplate "templates/category-index.html" context
                   >>= loadAndApplyTemplate "templates/default.html" context
                   >>= relativizeUrls

