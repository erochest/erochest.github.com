{-# LANGUAGE OverloadedStrings #-}

-- TODO: index.html
-- TODO: aria
-- TODO: http://www.ericrochester.com/blog/2011/07/21/linked-open-data-at-the-rare-book-school/
-- TODO: move over to clj-data-analysis subsite
-- TODO: RSS
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

postTemplate :: Context String -> Item String -> Compiler (Item String)
postTemplate context item =
            loadAndApplyTemplate "templates/errstyle/post.html" context item
        >>= loadAndApplyTemplate "templates/errstyle/default.html" context

style :: String -> String
style url =  "<link rel=\"stylesheet\" href=\"" <> url <> "\">"


main :: IO ()
main = hakyll $ do

    match "templates/**" $ compile templateCompiler

    match "index.html" $ do
        route       idRoute
        compile $   getResourceBody
                >>= loadAndApplyTemplate
                        "templates/errstyle/default.html"
                        (  dateField "date" "%e %B %Y"
                        <> constField "extra-header" (style "css/index.css")
                        <> defaultContext
                        )
                >>= relativizeUrls

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

