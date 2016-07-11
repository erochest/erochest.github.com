module Sites.Utils where


import Hakyll
import System.FilePath
import Data.List (isSuffixOf)


cleanRoute :: Routes
cleanRoute = customRoute createIndexRoute
    where
        createIndexRoute ident =
            takeDirectory p </> takeBaseName p </> "index.html"
            where p = toFilePath ident

cleanIndexUrls :: Item String -> Compiler (Item String)
cleanIndexUrls = return . fmap (withUrls cleanIndex)

cleanIndexHtmls :: Item String -> Compiler (Item String)
cleanIndexHtmls = return . fmap (replaceAll pattern replacement)
    where
        pattern = "/index.html"
        replacement = const "/"

cleanIndex :: String -> String
cleanIndex url
    | idx `isSuffixOf` url = take (length url - length idx) url
    | otherwise            = url
    where
        idx = "index.html"

