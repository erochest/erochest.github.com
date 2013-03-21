{-# LANGUAGE OverloadedStrings #-}


module Sites.CljDataAnalysis
    ( cljDataAnalysisSite
    ) where


import qualified Data.Set as S
import           Hakyll
import           Sites.Base
import           Sites.Types


cljDataAnalysisSite :: IO SiteInfo
cljDataAnalysisSite =
        return $ Site "clj-data-analysis" "clj-data-analysis" "." rules

rules :: Rules ()
rules = undefined

