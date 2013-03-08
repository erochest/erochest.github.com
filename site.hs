{-# LANGUAGE OverloadedStrings #-}

-- TODO: color scheme
-- TODO: aria
-- TODO: move over to clj-data-analysis subsite
-- TODO: RSS
-- TODO: compress all CSS.
-- TODO: notes


-- import           Control.Applicative
import           Control.Monad
import           Control.Monad.IO.Class
-- import qualified Data.List as L
import           Data.Monoid
import           Hakyll
-- import Debug.Trace


sassCompiler :: Compiler (Item String)
sassCompiler =
        getResourceString >>=
        withItemBody (unixFilter "sass" [ "--scss"
                                        , "--stdin"
                                        , "--load-path", "sass/errstyle/"
                                        ])

postTemplate :: Context String -> Item String -> Compiler (Item String)
postTemplate context item =
            loadAndApplyTemplate "templates/errstyle/post.html" context item
        >>= loadAndApplyTemplate "templates/errstyle/default.html" context

style :: String -> String
style url =  "<link rel=\"stylesheet\" href=\"" <> url <> "\">"

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
 - traceWatch msg x = trace (msg <> show x) x
 - 
 - traceMapWatch :: (Functor f, Show (f b)) => String -> (a -> b) -> f a -> f a
 - traceMapWatch msg f x = trace (msg <> show (f `fmap` x)) x
 -}

compileIndex :: Context String -> Template -> Compiler (Item String)
compileIndex context template =
        loadAllSnapshots "pages/*.md" "content" >>=
        recentFirst >>=
        return . take 8 >>=
        renderPanes template context >>=
        loadAndApplyTemplate "templates/errstyle/default.html" context >>=
        relativizeUrls

compilePage :: Compiler (Item String)
compilePage =   pandocCompiler
            >>= saveSnapshot "content"
            >>= loadAndApplyTemplate "templates/errstyle/post.html" context
            >>= loadAndApplyTemplate "templates/errstyle/default.html" context
            >>= relativizeUrls
    where context =  dateField "date" "%e %B %Y"
                  <> constField "extra-header" ""
                  <> defaultContext

main :: IO ()
main = hakyll $ do

    match "templates/**" $ compile templateCompiler

    create ["index.html"] $ do
        route       idRoute
        compile $   loadBody "templates/index-pane.html"
                >>= compileIndex (  dateField "date" "%e %B %Y"
                                 <> constField "extra-header" (style "css/index.css")
                                 <> defaultContext)

    match "pages/*.md" $ do
        route   $ setExtension "html"
        compile   compilePage

    match "sass/main.scss" $ do
        route   $ constRoute "css/main.css"
        compile   sassCompiler
    match "sass/index.scss" $ do
        route   $ constRoute "css/index.css"
        compile   sassCompiler

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

