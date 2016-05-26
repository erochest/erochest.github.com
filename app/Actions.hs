{-# LANGUAGE RecordWildCards #-}


module Actions where


import           Types

import           Sites.Actions.Build
import           Sites.Actions.Deploy


action :: Actions -> IO ()
action (Build args) = buildSite args
action Deploy{..}   = deploySite scratch bail
