{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}


module Sites.Projects
    ( projectList
    , projectMap
    , compileProjectIndex
    ) where


import           Control.Arrow       ((&&&))
import qualified Data.HashMap.Strict as M
import           Data.Monoid
import           Data.Time
import           Hakyll

import           Sites.Types
import           Sites.Utils


compileProjectIndex :: Rules ()
compileProjectIndex = create ["projects/index.html"] $ do
    route idRoute
    compile $ do
        debugCompiler $ show projectList
        let c  =  listField "posts" c' items
               <> boolField "onePage" (const True)
               <> siteContext Nothing
            c' =  field "title"      (pLookup projectTitle)
               <> field "teaser"     (pLookup projectTeaser)
               <> field "coverImage" (pLookup projectCoverImage)
               <> field "url"        (pLookup projectId)
               <> field "date"       (pLookup pDate)
               <> siteContext Nothing
        makeItem "" >>= indexTemplate c
    where
        items :: Compiler [Item String]
        items = return . map ((`Item` "") . fromFilePath) $ M.keys projectMap

        pLookup :: (ProjectInfo -> String) -> Item String -> Compiler String
        pLookup f = return
                  . maybe "" f
                  . (`M.lookup` projectMap)
                  . toFilePath
                  . itemIdentifier

        pDate :: ProjectInfo -> String
        pDate = formatTime defaultTimeLocale "%e %B %Y" . projectDate

{-
 - p :: String -> String -> String -> FilePath -> String -> ProjectInfo
 - p ident title tease cover =
 -     Project (ident ++ "/index.html") title tease cover .
 -             parseTimeOrError False defaultTimeLocale "%Y-%m-%dT%H:%M"
 -}

projectList :: [ProjectInfo]
projectList =
    [
    {-
     - [ p "retag" "retag" "A library and utility to fix implicitly nested tags."
     -     "/img/about.jpg" "2016-07-24T10:56"
     -}
    ]
-- <li><a href="/jekyll-rdfa/"><code>jekyll-rdfa</code></a></li> -->
-- <li><a href="/frank/">Frankensystem</a></li> -->
-- <li><a href="/text-regex-replace/"><code>text-regex-replace</code></a></li> -->
-- <li><a href="/books/">Books!</a></li> -->
-- <li><a href="/data-server/"><code>data-server</code></a></li> -->
-- <li><a href="/schwa/"><code>schwa</code></a></li> -->
-- <li><a href="/pygis/"><code>pygis</code></a></li> -->
-- <li><a href="/intelligent-design/"><code>intelligent-design</code></a></li> -->
-- <li><a href="/life-cast/"><code>life-cast</code></a></li> -->
-- <li><a href="/wa-tor/"><code>wa-tor</code></a></li> -->
-- <li><a href="/lod/"><code>lod</code></a></li> -->
-- <li><a href="/git-workshop/"><code>git-workshop</code></a></li> -->
-- <li><a href="/sigil/"><code>sigil</code></a></li> -->

projectMap :: M.HashMap FilePath ProjectInfo
projectMap = M.fromList $ (projectId &&& id) <$> projectList
