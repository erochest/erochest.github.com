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
import           Hakyll
import           Prelude hiding (FilePath)
import           Shelly
import           Sites.Base
import           Sites.Literate
import           Sites.Pager
import           Sites.Types
import           System.FilePath (
                   replaceExtension, takeBaseName, takeDirectory, takeExtension)
import           Text.Blaze.Html.Renderer.String (renderHtml)
import           Text.Regex.TDFA hiding (match)
import qualified Text.Regex.TDFA as RE
import qualified Text.Regex.TDFA.String as RES


erochestSite :: IO SiteInfo
erochestSite = Site "erochest" "erochest.github.com" "." `fmap` rules

postPattern :: Pattern
postPattern = "pages/**/*.md" .||. "pages/**/*.clj"

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
        recentFirst =<< loadAllSnapshots (postPattern .&&. hasNoVersion) "content"

loadSnippets :: Int -> Compiler [Item String]
loadSnippets lineCount =
            loadAll (postPattern .&&. hasVersion "raw")
        >>= recentFirst
        >>= mapM (withItemBody shorten)
        >>= mapM renderSnippet
    where shorten               = return . unlines . firstAndLinks lineCount . lines
          Right linkRegex       = RES.compile defaultCompOpt (ExecOption False) "^\\[[[:word:]-]+\\]: "
          firstAndLinks n lines =
              let body  = take n lines
                  links = filter (RE.match linkRegex) lines
              in  body ++ ["\n"] ++ links ++ ["\n"]
          renderSnippet item =
              return $ case itemExt of
                           ".clj" -> clojure `fmap` item
                           _      -> renderPandoc item
              where itemExt = takeExtension . toFilePath $ itemIdentifier item

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
    pages <- shelly $ map TL.unpack . filter isPost <$> listCategoryPagesT "pages/"
    let (d, m)    = length pages `divMod` pageLength
        indexPageCount = d + if m == 0 then 0 else 1
        indexPages     = "pages/index.html" : [ "pages/index-" <> show n <> ".html"
                                              | n <- take (indexPageCount - 1) [1..]
                                              ]
    return (indexPageCount, indexPages)
    where isPost fn | ".md" `TL.isSuffixOf` fn  = True
                    | ".clj" `TL.isSuffixOf` fn = True
                    | otherwise                 = False

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

    match "pages/**/*.clj" $ do
        route   $   setExtension "html"
        compile $   do
            rsc <- getResourceBody
            saveSnapshot "content" (itemSetBody (clojure $ itemBody rsc) rsc)
                >>= postTemplate (siteContext Nothing)
                >>= relativizeUrls

    match "pages/**/*.clj" $ version "raw" $ do
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

