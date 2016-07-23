{-# LANGUAGE OverloadedStrings #-}

-- | A collection of functions about pagination.

module Sites.Pager
    ( paginate
    ) where


import           Data.Monoid
import           Hakyll

import           Sites.Utils


postsByPage :: MonadMetadata m => Int -> [Identifier] -> m [[Identifier]]
postsByPage n = fmap (paginateEvery n) . sortRecentFirst

paginate :: Int -> FilePath -> Pattern -> Bool -> Rules ()
paginate pageSize root pat cleanUrl = do
    pag <- buildPaginateWith (postsByPage pageSize) pat $ indexFileName root
    paginateRules pag $ \pageNum p -> do
        route idRoute
        compile $ do
            posts     <- recentFirst =<< loadAllSnapshots p "content"
            let c'    =  siteContext Nothing
                pageC =  paginateContext pag pageNum
                postC =  teaserField "teaser" "content" <> c'
                c     =  listField "posts" postC (return posts) <> pageC <> c'
                clean = if cleanUrl then cleanIndexUrls else return
            makeItem "" >>= indexTemplate c >>= relativizeUrls >>= clean
