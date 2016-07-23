{-# LANGUAGE OverloadedStrings #-}


module Sites.Reading
    ( readingSite
    ) where


import           Hakyll

import           Sites.Base
import           Sites.Erochest (indexPageSize)
import           Sites.Pager
import           Sites.Types


readingPattern :: Pattern
readingPattern = "reading-log/**/*.md"

readingSite :: IO SiteInfo
readingSite = return . Site "reading-journal" "reading" "reading-log" $ do
    paginate indexPageSize "reading-log" readingPattern True

    match readingPattern $ do
        route   cleanRoute
        compile compilePost
