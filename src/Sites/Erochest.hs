{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}

module Sites.Erochest
    ( erochestSite
    ) where


import           Control.Applicative
import           Control.Exception
import           Control.Monad
import           Control.Monad.Error.Class
import           Data.Monoid
import           Hakyll
import           Prelude                   hiding (FilePath)
import           Sites.Base
import           Sites.Literate
import           Sites.Types


erochestSite :: IO SiteInfo
erochestSite = Site "erochest" "erochest.github.com" "." `fmap` rules

-- Post utilities: To add a filetype for posts, everything you need to change
-- should be in the next few definitions.

postPattern :: Pattern
postPattern = "posts/**/*.md" .||. "posts/**/index.clj"

loadPageContent :: Compiler [Item String]
loadPageContent =
        recentFirst =<< loadAllSnapshots (postPattern .&&. hasNoVersion) "content"


rules :: IO (Rules ())
rules =
    return $ do
        create ["posts/index.html"] $
            return ()

        create ["atom.xml"] $ do
            route idRoute
            compile $
                let context = bodyField "description" <> siteContext Nothing
                    config  = FeedConfiguration "Eric Rochester"
                                                "Feed for my site."
                                                "Eric Rochester"
                                                "erochest@gmail.com"
                                                "http://www.ericrochester.com/"
                in  take 10 <$> loadPageContent >>= renderAtom config context

        match "index.html" $ do
            let context = siteContext . Just $ style "/css/index.css"
            route       idRoute
            compile $   getResourceBody
                    >>= loadAndApplyDefault context
                    >>= relativizeUrls
                    >>= cleanIndexUrls

        match "pages/*.md" $ do
            route   $   customRoute createBaseIndexRoute
            compile .   compilePage'
                    $   siteContext
                    .   Just
                    $   style "/css/index.css"

        match "pages/*.html" $ do
            let context = siteContext . Just $ style "/css/index.css"
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
                                >>= loadAndApplyTemplate' "templates/post.html"
                                        (siteContext Nothing)
                                >>= relativizeUrls
                                >>= cleanIndexUrls
                     Left e     ->  throwError . pure $ displayException e

        match "posts/**/*.clj" $ version "raw" $ do
            route   idRoute
            compile getResourceBody

        match "sass/index.scss" $ do
            route   $ constRoute "css/index.css"
            compile   sassCompiler

        match "sass/materialize.scss" $ do
            route   $ constRoute "css/materialize.css"
            compile   sassCompiler
