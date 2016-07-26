{-# LANGUAGE OverloadedStrings #-}


module Sites.Reading
    ( readingSite
    ) where


import           Data.Monoid
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

    create ["posts.json"] $ do
        route idRoute
        compile $
            makeItem
                =<< unixFilter "localRDFa.py" ["-g", "output", "-j"]
                .   foldMap itemBody
                =<< loadAllSnapshots readingPattern "content"

    match readingPattern $ do
        route   cleanRoute
        compile compileReading

compileReading :: Compiler (Item String)
compileReading = compileReading' $ siteContext Nothing

compileReading' :: Context String -> Compiler (Item String)
compileReading' c =   pandocCompiler
                  >>= loadAndApplyTemplate "templates/reading.html" c'
                  >>= saveSnapshot "content"
                  >>= loadAndApplyDefault c'
                  >>= relativizeUrls
                  >>= cleanIndexUrls
    where
        c' =  constField "extraScript" parallaxScript
           <> boolField "noContainer" (const True)
           <> field "typeof"   (meta "typeof"  )
           <> field "resource" (meta "resource")
           <> c

