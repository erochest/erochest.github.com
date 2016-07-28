{-# LANGUAGE OverloadedStrings #-}


module Sites.Reading
    ( readingSite
    ) where


import           Control.Applicative ((<|>))
import           Control.Lens        hiding (Context)
import           Control.Monad
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

    create ["posts.ttl"] $ do
        route idRoute
        compile $ do
            pages     <-  map itemBody
                      <$> loadAll readingPattern
            tmpFiles  <-  replicateM (length pages) (newTmpFile "-page.html")
            tmpFiles' <-  unsafeCompiler . forM (zip tmpFiles pages)
                      $   \(TmpFile filename, page) -> do
                            writeFile filename page
                            return filename
            let args = "serialize"
                     : "--format" : "rdfa"
                     : "--output-format" : "turtle"
                     : "--uri" : "uri:reading-journal"
                     : tmpFiles'
            makeItem . unlines . drop 1 . lines =<< unixFilter "rdf" args ""

    create ["posts.json"] $ do
        route idRoute
        compile $ do
            ttl <- itemBody <$> load "posts.ttl"
            TmpFile tmpFile <- newTmpFile "-page.ttl"
            unsafeCompiler $ writeFile tmpFile ttl
            let args = [ "serialize"
                       , "--format", "turtle"
                       , "--output-format", "jsonld"
                       , tmpFile
                       ]
            makeItem . unlines . drop 1 . lines =<< unixFilter "rdf" args ""

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
        >>= saveSnapshot "content"
        >>= applyTemplate tpl c''
        >>= saveSnapshot "rdfa"
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
