{-# LANGUAGE OverloadedStrings #-}


module Sites.CljDataAnalysis
    ( cljDataAnalysisSite
    ) where


import           Hakyll
import           Sites.Base
import           Sites.Types


cljDataAnalysisSite :: IO SiteInfo
cljDataAnalysisSite =
        return $ Site "clj-data-analysis" "clj-data-analysis" "clj-data-analysis" rules

rules :: Rules ()
rules = do
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
