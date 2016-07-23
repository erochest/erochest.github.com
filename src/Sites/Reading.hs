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
readingPattern = "reading/**/*.md"

readingSite :: IO SiteInfo
readingSite = return . Site "reading-journal" "reading" "reading" $ do
    paginate indexPageSize "reading" readingPattern True

    match readingPattern $ do
        route   cleanRoute
        compile compilePost
