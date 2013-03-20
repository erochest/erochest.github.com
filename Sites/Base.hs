{-# LANGUAGE OverloadedStrings #-}

module Sites.Base
    ( sassCompiler
    , postTemplate
    , style
    , compilePage
    , siteContext
    ) where

import           Control.Applicative
import           Data.Maybe (fromMaybe)
import           Data.Monoid
import           Hakyll
import           Sites.Types

sassCompiler :: Compiler (Item String)
sassCompiler =
        getResourceString >>=
        withItemBody (unixFilter "sass" [ "--scss"
                                        , "--stdin"
                                        , "--load-path", "sass/"
                                        ])

postTemplate :: Context String -> Item String -> Compiler (Item String)
postTemplate context item =
            loadAndApplyTemplate "templates/post.html" context item
        >>= loadAndApplyTemplate "templates/default.html" context

style :: String -> String
style url =  "<link rel=\"stylesheet\" href=\"" <> url <> "\">"

compilePage :: Compiler (Item String)
compilePage =   pandocCompiler
            >>= saveSnapshot "content"
            >>= postTemplate context
            >>= relativizeUrls
    where context = siteContext Nothing

siteContext :: Maybe String -> Context String
siteContext extraHeader =
           dateField "date" "%e %B %Y"
        <> dateField "datetime" "%Y-%m-%dT%H:%M:%SZ"
        <> constField "extra-header" (fromMaybe "" extraHeader)
        <> defaultContext

