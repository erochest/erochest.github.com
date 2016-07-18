{-# LANGUAGE OverloadedStrings #-}


module Sites.Utils
    ( cleanRoute
    , cleanIndexUrls
    , cleanIndexHtmls
    , cleanIndex
    , pandocHtml
    , pandocClean
    , style
    , sassCompiler
    , compilePage
    , siteContext
    , reformatDate
    , parseDateLax
    , formatTime'
    , extraHeaderContext
    , loadAndApplyDefault
    ) where


import           Control.Monad
import           Data.List          (isSuffixOf)
import           Data.Maybe         (fromMaybe)
import           Data.Monoid
import           Data.Time.Clock    (UTCTime)
import           Data.Time.Format   (defaultTimeLocale, formatTime, parseTimeM)
import           Hakyll
import           System.Environment
import           System.FilePath


cleanRoute :: Routes
cleanRoute = customRoute createIndexRoute
    where
        createIndexRoute ident =
            takeDirectory p </> takeBaseName p </> "index.html"
            where p = toFilePath ident

cleanIndexUrls :: Item String -> Compiler (Item String)
cleanIndexUrls = return . fmap (withUrls cleanIndex)

cleanIndexHtmls :: Item String -> Compiler (Item String)
cleanIndexHtmls = return . fmap (replaceAll "/index.html" replacement)
    where
        replacement = const "/"

cleanIndex :: String -> String
cleanIndex url
    | idx `isSuffixOf` url = take (length url - length idx) url
    | otherwise            = url
    where
        idx = "index.html"

pandocHtml :: Rules ()
pandocHtml = do
    route   $   setExtension "html"
    compile $   pandocCompiler
            >>= loadAndApplyDefault (siteContext Nothing)
            >>= relativizeUrls
            >>= cleanIndexUrls

pandocClean :: Rules ()
pandocClean = do
    route       cleanRoute
    compile $   pandocCompiler
            >>= loadAndApplyDefault (siteContext Nothing)
            >>= relativizeUrls
            >>= cleanIndexUrls

sassCompiler :: Compiler (Item String)
sassCompiler =
        getResourceString >>=
        withItemBody (unixFilter "sass" [ "--scss"
                                        , "--stdin"
                                        , "--load-path", "sass/"
                                        ])

style :: String -> String
style url =  "<link rel=\"stylesheet\" href=\"" <> url <> "\">"

compilePage :: Compiler (Item String)
compilePage =   pandocCompiler
            >>= saveSnapshot "content"
            >>= relativizeUrls
            >>= cleanIndexUrls

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

loadAndApplyDefault :: Context a -> Item a -> Compiler (Item String)
loadAndApplyDefault c i = do
    debug <- unsafeCompiler $ lookupEnv "DEBUG"
    let c' = c <> constField "livereload" (maybe "" (const livereload) debug)
    loadAndApplyTemplate "templates/default.html" c' i
    where
        livereload :: String
        livereload = "\
            \<script>\n\
            \  document.write('<script src=\"http://' + (location.host || 'localhost').split(':')[0] +\n\
            \    ':35729/livereload.js?snipver=1\"></' + 'script>')\n\
            \</script>"

