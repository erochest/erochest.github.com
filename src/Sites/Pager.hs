{-# LANGUAGE OverloadedStrings #-}

-- | A collection of functions about pagination.

module Sites.Pager
    ( getPageNumber
    , pager
    , getPage
    ) where


import           Control.Monad
import           Data.Char
import qualified Data.List                   as L
import           Data.Monoid
import           Text.Blaze.Html5            hiding (div, map, span, style)
import qualified Text.Blaze.Html5            as H5
import           Text.Blaze.Html5.Attributes hiding (style)


getPageNumber :: FilePath -> Int
getPageNumber fp
    | "index.html" `L.isSuffixOf` fp = 0
    | otherwise =
        read . takeWhile isDigit $ dropWhile (not . isDigit) fp

pager :: Int -> Int -> (Int -> String) -> Html
pager 1 _ _ = H5.span $ toHtml ("" :: String)
pager pageCount page getPageUrl =
        H5.div ! class_ "page-nav" $ do
            when (page > 0)         $ pageLink 0    "«"
            when (prev > 0)         $ pageLink prev "‹"
            when (page - 2 >= 0)    $ pl (page - 2)
            when (page - 1 >= 0)    $ pl (page - 1)
            toHtml (succ page)
            toHtml (" " :: String)
            when (page + 1 <= lst)  $ pl (page + 1)
            when (page + 2 <= lst)  $ pl (page + 2)
            when (next < lst)       $ pageLink next "›"
            when (lst > page)       $ pageLink lst "»"
    where prev = pred page
          next = succ page
          lst  = pred pageCount

          pl n = pageLink n . show $ succ n

          pageLink :: Int -> String -> Html
          pageLink n txt = do
              H5.span $ a ! href (toValue (getPageUrl n)) $ toHtml txt
              toHtml (" " :: String)

getPage :: String -> Int -> String
getPage basename 0 = basename <> ".html"
getPage basename n = basename <> "-" <> show n <> ".html"

