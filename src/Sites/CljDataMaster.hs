{-# LANGUAGE OverloadedStrings #-}



module Sites.CljDataMaster
  ( cljDataMasterSite
  ) where


import           Hakyll

import           Sites.Base
import           Sites.Types
import           Sites.Utils


cljDataMasterSite :: IO SiteInfo
cljDataMasterSite = return $ Site "clj-data-master"
                                  "clj-data-master"
                                  "clj-data-master"
                                  rules

-- temp-diffs.csv
-- data/ufo.json
-- data/post-sample-100000.json.gz
-- /mastering-clj-data => /clj-data-master

rules :: Rules ()
rules = do
    match "clj-data-master/index.md" $ do
        route   $   setExtension "html"
        compile $   pandocCompiler
                >>= loadAndApplyTemplate "templates/default.html" (siteContext Nothing)
                >>= relativizeUrls
                >>= cleanIndexUrls

    match "clj-data-master/*.csv" $ do
        route   idRoute
        compile copyFileCompiler

    match "clj-data-master/data/*" $ do
        route   idRoute
        compile copyFileCompiler

