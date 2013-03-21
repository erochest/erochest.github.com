
module Sites
    ( module X
    , site
    ) where


import Sites.Base     as X
import Sites.Erochest as X
import Sites.Pager    as X
import Sites.Types    as X

import Control.Applicative
import Control.Monad

site :: IO X.RootSite
site = X.Root <$> X.erochestSite <*> sequence []

