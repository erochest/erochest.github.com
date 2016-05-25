{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}


import           Hakyll
import           Sites


main :: IO ()
main = do
    Root{..} <- site
    hakyll $ do
    match "templates/**" $ compile templateCompiler

    siteRules rootSite
    mapM_ siteRules subsites

    match "sass/main.scss" $ do
        route   $ constRoute "css/main.css"
        compile   sassCompiler

    match "CNAME" $ do
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

