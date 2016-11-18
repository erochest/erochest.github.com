{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}


module Sites.Actions.Hakyll where


import qualified Data.Text          as T
import           System.Environment
import           System.FilePath

import           Hakyll
import           Sites


callHakyll :: [T.Text] -> IO ()
callHakyll args = do
    Root{..} <- site
    withArgs (map T.unpack args) $ hakyllWith config $ do
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
        match "fonts/**" $ do
            route   idRoute
            compile copyFileCompiler
        match "*.xml" $ do
            route   idRoute
            compile copyFileCompiler
    where
        config = defaultConfiguration { ignoreFile = ignoreBower }
        ignoreBower fp =  ignoreFile defaultConfiguration fp
                       || "bower_components" `elem` splitDirectories fp
