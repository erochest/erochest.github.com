{-# LANGUAGE OverloadedStrings #-}

module Sites.Base
    ( sassCompiler
    , postTemplate
    , style
    , compilePage
    , siteContext
    , reformatDate
    , parseDateLax
    , formatTime'
    , extraHeaderContext
    ) where

import           Control.Applicative
import           Control.Monad
import           Data.Maybe (fromMaybe)
import           Data.Monoid
import           Data.Time.Clock (UTCTime)
import           Data.Time.Format (defaultTimeLocale, formatTime, parseTimeM)
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
        <> extraHeaderContext extraHeader
        <> defaultContext

extraHeaderContext :: Maybe String -> Context String
extraHeaderContext = constField "extra-header" . fromMaybe ""

reformatDate :: String -> String
reformatDate dateStamp = maybe dateStamp formatTime' $ parseDateLax dateStamp

parseDateLax :: String -> Maybe UTCTime
parseDateLax dateStamp = msum $ map (`parse` dateStamp) formats
    where parse   = parseTimeM True defaultTimeLocale
          formats = [ "%a, %d %b %Y %H:%M:%S UT"
                    , "%Y-%m-%dT%H:%M:%SZ"
                    , "%Y-%m-%d %H:%M:%S"
                    , "%Y-%m-%d"
                    , "%B %e, %Y %l:%M %p"
                    , "%B %e, %Y"
                    ]

formatTime' :: UTCTime -> String
formatTime' = formatTime defaultTimeLocale "%e %b %Y"

