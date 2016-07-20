{-# LANGUAGE OverloadedStrings #-}


module Sites.Reading
    ( readingSite
    ) where


import           Hakyll
import           Sites.Base
import           Sites.Types


readingSite :: IO SiteInfo
readingSite = return . Site "reading-journal" "reading" "reading" $ do
    match "reading/index.md"
        pandocHtml

    match "reading/**/*.md"
        pandocHtml
