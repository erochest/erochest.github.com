{-# LANGUAGE OverloadedStrings #-}


import           Control.Applicative
import           Data.Monoid
import           Hakyll


main :: IO ()
main = hakyll $ do

    match "templates/*" $ compile templateCompiler

    match "index.md" $ do
        route   $   setExtension "html"
        compile $   pandocCompiler
                >>= loadAndApplyTemplate "templates/default.html" defaultContext
                >>= relativizeUrls

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

