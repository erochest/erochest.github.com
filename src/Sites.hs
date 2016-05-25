
module Sites
    ( module X
    , site
    ) where


import           Sites.Base            as X
import           Sites.CljDataAnalysis as X
import           Sites.CljDataMaster   as X
import           Sites.Erochest        as X
import           Sites.Pager           as X
import           Sites.Types           as X

import           Control.Applicative


site :: IO X.RootSite
site = X.Root <$> X.erochestSite
              <*> sequence [ X.cljDataAnalysisSite
                           , X.cljDataMasterSite
                           ]

