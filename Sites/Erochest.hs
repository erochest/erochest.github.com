{-# LANGUAGE OverloadedStrings #-}

module Sites.Erochest
    ( erochestSite
    ) where


import           Control.Applicative
import           Control.Monad
import           Data.Monoid
import qualified Data.Text.Lazy as TL
import           Hakyll
import           Prelude hiding (FilePath)
import           Shelly
import           Sites.Base
import           Sites.Pager
import           Sites.Types
import           Text.Blaze.Html.Renderer.String (renderHtml)


erochestSite :: IO SiteInfo
erochestSite =
        Site "erochest" "erochest.github.com" "." `fmap` rules

pageLength :: Int
pageLength = 25

-- Borrowed heavily from applyJoinTemplateList
renderPanes :: Template -> Context c -> [Item c] -> Compiler (Item String)
renderPanes template baseContext items =
    zipWithM (applyTemplate template) (map context ([0..] :: [Int])) items >>=
    makeItem . concatMap itemBody
    where paneClasses 0 = " main"
          paneClasses 1 = " third"
          paneClasses 2 = " second"
          paneClasses 4 = " second third"
          paneClasses 6 = " second"
          paneClasses 7 = " second third"
          paneClasses _ = ""
          context pos = constField "pos-class" (paneClasses pos) <> baseContext

loadPageContent :: Compiler [Item String]
loadPageContent =
        recentFirst <$> loadAllSnapshots ("pages/*.md" .&&. hasNoVersion) "content"

compileIndex :: Context String -> Template -> Compiler (Item String)
compileIndex context template =
        take 8 <$> loadPageContent >>=
        renderPanes template context >>=
        loadAndApplyTemplate "templates/default.html" context >>=
        relativizeUrls

indexPageInfo :: IO (Int, [String])
indexPageInfo = do
    pages <- shelly $ map TL.unpack . filter (TL.isSuffixOf ".md") <$> lsT "pages/"
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


rules :: IO (Rules ())
rules = do
    (indexPageCount, indexPages) <- indexPageInfo
    return $ do
    create ["index.html"] $ do
        route       idRoute
        compile $   loadBody "templates/index-pane.html"
                >>= (compileIndex . siteContext . Just $ style "css/index.css")

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

    match "pages/*.md" $ do
        route   $ setExtension "html"
        compile   compilePage

    match "pages/*.md" $ version "raw" $ do
        route   idRoute
        compile getResourceBody

    match "sass/index.scss" $ do
        route   $ constRoute "css/index.css"
        compile   sassCompiler

