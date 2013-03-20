{-# LANGUAGE OverloadedStrings #-}


-- import           Control.Applicative
import           Control.Monad
-- import           Control.Monad.IO.Class
import           Data.Char
import qualified Data.List as L
import           Data.Maybe (fromMaybe)
import           Data.Monoid
import qualified Data.Text.Lazy as TL
import           Hakyll
import           Shelly
import           Text.Blaze.Html.Renderer.String (renderHtml)
import           Text.Blaze.Html5 hiding (div, map, span, style)
import           Text.Blaze.Html5.Attributes hiding (style)
import qualified Text.Blaze.Html5 as H5
import qualified Text.Blaze.Html5.Attributes as H5A
import qualified Debug.Trace as Debug
import           Sites.Base


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

{-
 - traceWatch :: Show a => String -> a -> a
 - traceWatch msg x = Debug.trace (msg <> show x) x
 - 
 - traceMapWatch :: (Functor f, Show (f b)) => String -> (a -> b) -> f a -> f a
 - traceMapWatch msg f x = Debug.trace (msg <> show (f `fmap` x)) x
 -}

loadPageContent :: Compiler [Item String]
loadPageContent =
        recentFirst <$> loadAllSnapshots ("pages/*.md" .&&. hasNoVersion) "content"

recentPages :: Int -> Compiler [Item String]
recentPages n = liftM (take n) loadPageContent

compileIndex :: Context String -> Template -> Compiler (Item String)
compileIndex context template =
        recentPages 8 >>=
        renderPanes template context >>=
        loadAndApplyTemplate "templates/default.html" context >>=
        relativizeUrls

getPageNumber :: Prelude.FilePath -> Int
getPageNumber fp
    | "index.html" `L.isSuffixOf` fp = 0
    | otherwise =
        read . takeWhile isDigit $ dropWhile (not . isDigit) fp

pager :: Int -> Int -> (Int -> String) -> Html
pager 1 _ _ = H5.span $ toHtml ("" :: String)
pager pageCount page getPageUrl =
        H5.div ! class_ "page-nav" $ do
            when (page > 0)         $ pageLink 0    "«"
            when (prev > 0)         $ pageLink prev "‹"
            when (page - 2 >= 0)    $ pl (page - 2)
            when (page - 1 >= 0)    $ pl (page - 1)
            toHtml (succ page)
            toHtml (" " :: String)
            when (page + 1 <= last) $ pl (page + 1)
            when (page + 2 <= last) $ pl (page + 2)
            when (next < last)      $ pageLink next "›"
            when (last > page)      $ pageLink last "»"
    where prev = pred page
          next = succ page
          last = pred pageCount

          pl n = pageLink n . show $ succ n

          pageLink :: Int -> String -> Html
          pageLink n text = do
              H5.span $ a ! href (toValue (getPageUrl n)) $ toHtml text
              toHtml (" " :: String)

getPage :: String -> Int -> String
getPage p 0 = p <> ".html"
getPage p n = p <> "-" <> show n <> ".html"

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

main :: IO ()
main = do
    pages <- shelly $ map TL.unpack . filter (TL.isSuffixOf ".md") <$> lsT "pages/"
    let (d, m)    = length pages `divMod` pageLength
        indexPageCount = d + if m == 0 then 0 else 1
        indexPages     = "pages/index.html" : [ "pages/index-" <> show n <> ".html"
                                              | n <- take (indexPageCount - 1) [1..]
                                              ]

    hakyll $ do
    match "templates/**" $ compile templateCompiler

    create ["index.html"] $ do
        route       idRoute
        compile $   loadBody "templates/index-pane.html"
                >>= (compileIndex . siteContext . Just $ style "css/index.css")

    create ["atom.xml"] $ do
        route idRoute
        compile $ do
            let context = bodyField "description" <> siteContext Nothing
                config  = FeedConfiguration "Eric Rochester"
                                            "Feed for my site."
                                            "Eric Rochester"
                                            "erochest@gmail.com"
                                            "http://www.ericrochester.com/"
            recentPages 10 >>= renderAtom config context

    create (map fromFilePath indexPages) $ do
        route     idRoute
        compile $ compilePageIndex indexPageCount

    match "pages/*.md" $ do
        route   $ setExtension "html"
        compile   compilePage

    match "pages/*.md" $ version "raw" $ do
        route   idRoute
        compile getResourceBody

    match "sass/main.scss" $ do
        route   $ constRoute "css/main.css"
        compile   sassCompiler
    match "sass/index.scss" $ do
        route   $ constRoute "css/index.css"
        compile   sassCompiler

    match "clj-data-analysis/index.md" $ do
        route   $   setExtension "html"
        compile $   pandocCompiler
                >>= loadAndApplyTemplate "templates/default.html" (siteContext Nothing)
                >>= relativizeUrls

    match "clj-data-analysis/data/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "clj-data-analysis/data/UCI/*" $ do
        route   idRoute
        compile copyFileCompiler

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
    match "js/**" $ do
        route   idRoute
        compile copyFileCompiler
    match "css/*.css" $ do
        route   idRoute
        compile copyFileCompiler
    match "font/*" $ do
        route   idRoute
        compile copyFileCompiler

