{-# LANGUAGE OverloadedStrings #-}

-- TODO: favicon
-- TODO: index.html
-- TODO: http://www.ericrochester.com/blog/2011/07/21/linked-open-data-at-the-rare-book-school/
-- TODO: move over to clj-data-analysis subsite
-- TODO: compress all CSS.


-- import           Control.Applicative
import           Data.Monoid
import           Hakyll


sassCompiler :: Compiler (Item String)
sassCompiler =
        getResourceString >>=
        withItemBody (unixFilter "sass" [ "--scss"
                                        , "--stdin"
                                        , "--load-path", "sass/errstyle/"
                                        ])


main :: IO ()
main = hakyll $ do

    match "templates/*" $ compile templateCompiler
    match "templates/errstyle/*" $ compile templateCompiler

    match "index.md" $ do
        let context = dateField "date" "%e %B %Y" <> defaultContext
        route   $   setExtension "html"
        compile $   pandocCompiler
                >>= loadAndApplyTemplate "templates/errstyle/post.html" context
                >>= loadAndApplyTemplate "templates/errstyle/default.html" context
                >>= relativizeUrls

    match "sass/*.scss" $ do
        route   $ constRoute "css/main.css"
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

