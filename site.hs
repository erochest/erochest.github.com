{-# LANGUAGE OverloadedStrings #-}


import Control.Arrow
import Hakyll


main :: IO ()
main = hakyll $ do

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

compassCompiler :: Compiler Resource String
compassCompiler =   getResourceString
                >>> unixFilter "sass" [ "-s", "--scss", "--compass"
                                      , "--load-path", "./sass"
                                      ]
                >>> arr compressCss


