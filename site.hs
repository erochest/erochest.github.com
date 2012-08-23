{-# LANGUAGE OverloadedStrings #-}


import Control.Arrow
import Data.Monoid
import Hakyll
import qualified Data.List as L


main :: IO ()
main = hakyll $ do

    match "index.html" $ route idRoute
    create "index.html" $ constA mempty
        >>> arr (setField "title" "Eric Rochester &loz; Home")
        >>> requireAllA "articles/*" (arr id *** arr ( L.take 3
                                                     . L.reverse
                                                     . L.sortBy comparePagesByDate
                                                     )
                                     >>> addPostList "articles"
                                     )

        -- requireAllA "notes/*.md"
        >>> applyTemplateCompiler "templates/index.html"
        >>> applyTemplateCompiler "templates/default.html"
        >>> relativizeUrlsCompiler

    match "articles/*" $ do
        route   $ setExtension "html"
        compile $   pageCompiler
                >>> arr (renderDateField "published" "%B %e, %Y, at %H:%M" "unknown")
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

addPostList :: String -> Compiler (Page String, [Page String]) (Page String)
addPostList field = setFieldA field $
        require "templates/articleitem.html" renderPost
    >>> arr mconcat
    >>> applyTemplateCompiler "templates/articlediv.html"
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


