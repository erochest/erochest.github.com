{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}


-- import           Control.Applicative
import           Control.Monad
-- import           Control.Monad.IO.Class
import           Data.Char
import qualified Data.List as L
import           Data.Maybe (fromMaybe)
import           Data.Monoid
import qualified Data.Text.Lazy as TL
import           Hakyll
import           Sites
import qualified Debug.Trace as Debug


{-
 - traceWatch :: Show a => String -> a -> a
 - traceWatch msg x = Debug.trace (msg <> show x) x
 - 
 - traceMapWatch :: (Functor f, Show (f b)) => String -> (a -> b) -> f a -> f a
 - traceMapWatch msg f x = Debug.trace (msg <> show (f `fmap` x)) x
 -}

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

