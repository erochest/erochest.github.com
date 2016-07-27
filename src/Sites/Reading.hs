{-# LANGUAGE OverloadedStrings #-}


module Sites.Reading
    ( readingSite
    ) where


import           Control.Applicative ((<|>))
import           Control.Lens        hiding (Context)
import           Data.Aeson
import           Data.Aeson.Lens
import           Data.Char           (toLower)
import           Data.Maybe          (fromMaybe)
import           Data.Monoid
import qualified Data.Text           as T
import           Hakyll

import           Sites.Base
import           Sites.Erochest      (indexPageSize)
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
compileReading' c = do
    typeof <- meta "typeof" =<< getResourceString
    tpl <-  loadBody ( fromFilePath
                     $ "templates/" ++ map toLower typeof ++ ".html"
                     )
        <|> loadBody "templates/reading.html"
    let c'' = secondaryMeta typeof <> c'
    pandocCompiler
        >>= applyTemplate tpl c''
        >>= saveSnapshot "content"
        >>= loadAndApplyDefault c''
        >>= relativizeUrls
        >>= cleanIndexUrls
    where
        c' =  constField "extraScript" parallaxScript
           <> boolField "noContainer" (const True)
           <> field "typeof"   (meta "typeof"  )
           <> field "resource" (meta "resource")
           <> c

secondaryMeta :: String -> Context String
secondaryMeta "Work" =
       field "authorName"     (dig "author" "name")
    <> field "authorTypeOf"   (dig "author" "typeof")
    <> field "authorResource" (dig "author" "resource")
secondaryMeta _      = mempty

dig :: T.Text -> T.Text -> Item String -> Compiler String
dig k1 k2 = fmap ( fromMaybe (fail errMsg)
                 . preview (key k1 . key k2 . _String . to T.unpack)
                 . Object
                 )
          . getMetadata
          . itemIdentifier
    where
        errMsg = T.unpack $ mconcat ["No ", k1, ".", k2, " in post metadata."]
