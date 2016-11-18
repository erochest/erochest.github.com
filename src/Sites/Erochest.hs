{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}


module Sites.Erochest
    ( erochestSite
    , indexPageSize
    ) where


import           Control.Exception
import           Control.Monad.Error.Class
import           Data.Monoid
import           Hakyll

import           Sites.Base
import           Sites.Literate
import           Sites.Pager
import           Sites.Projects
import           Sites.Types


indexPageSize :: Int
indexPageSize = 10


erochestSite :: IO SiteInfo
erochestSite = Site "erochest" "erochest.github.com" "." `fmap` rules

-- Post utilities: To add a filetype for posts, everything you need to change
-- should be in the next few definitions.

postPattern :: Pattern
postPattern =    "posts/**/*.md"
            .||. "posts/**/src/main.purs"
            .||. "posts/**/index.clj"

loadPageContent :: Compiler [Item String]
loadPageContent =   recentFirst
                =<< loadAllSnapshots (postPattern .&&. hasNoVersion) "content"

rules :: IO (Rules ())
rules =
    return $ do
        paginate indexPageSize "posts" postPattern "Blog" False
        compileProjectIndex

        create ["atom.xml"] $ do
            route idRoute
            compile $
                let context =  bodyField "description"
                            <> siteContext Nothing Nothing
                    config  =  FeedConfiguration "Eric Rochester"
                                                 "Feed for my site."
                                                 "Eric Rochester"
                                                 "erochest@gmail.com"
                                                 "http://www.ericrochester.com/"
                in  take 10 <$> loadPageContent >>= renderAtom config context

        match "index.html" $ do
            let context = siteContext Nothing . Just $ style "/css/index.css"
            route       idRoute
            compile $   getResourceBody
                    >>= loadAndApplyDefault context
                    >>= relativizeUrls
                    >>= cleanIndexUrls

        match "pages/about.md" $ do
            route   $   customRoute createBaseIndexRoute
            compile .   compilePage'
                    .   mappend profileContext
                    $   siteContext Nothing
                    .   Just
                    $   style "/css/index.css"

        match "pages/*.html" $ do
            let context = siteContext Nothing Nothing
            route   $   customRoute createBaseIndexRoute
            compile $   getResourceBody
                    >>= loadAndApplyDefault context

        match "posts/**/*.md" $ do
            route   $ setExtension "html"
            compile   compilePost

        match "posts/**/*.md" $ version "raw" $ do
            route   idRoute
            compile getResourceBody

        match "posts/**/index.clj" $ do
            route   $   setExtension "html"
            compile $   do
                rsc <- getResourceBody
                case clojure $ itemBody rsc of
                     Right body ->  saveSnapshot "content"
                                        (itemSetBody body rsc)
                                >>= postTemplate (siteContext Nothing Nothing)
                                >>= relativizeUrls
                                >>= cleanIndexUrls
                     Left e     ->  throwError . pure $ displayException e

        match "posts/**/*.clj" $ version "raw" $ do
            route   idRoute
            compile getResourceBody

        match "posts/**/src/main.purs" $ do
            route   $ gsubRoute "src/main.purs" (const "index.html")
            compile $ do
                rsc <- getResourceBody
                -- TODO: compile purescript
                -- TOOD: embed/load JS (purescript) in webpage
                -- TODO: inject code to run script
                -- TODO: metadata to create a div?
                case purescript $ itemBody rsc of
                     Left e     ->  throwError . pure $ displayException e
                     Right body ->  saveSnapshot "content"
                                        (itemSetBody body rsc)
                                >>= postTemplate (siteContext Nothing Nothing)
                                >>= relativizeUrls
                                >>= cleanIndexUrls

        match "posts/**/main.purs" $ version "raw" $ do
            route   idRoute
            compile getResourceBody

        match "sass/index.scss" $ do
            route   $ constRoute "css/index.css"
            compile   sassCompiler

        match "sass/materialize.scss" $ do
            route   $ constRoute "css/materialize.css"
            compile   sassCompiler
