{-# LANGUAGE RecordWildCards #-}


module Actions where


import           Types

import           Sites.Actions.Hakyll
import           Sites.Actions.Deploy
import           Sites.Actions.Illiterate


action :: Actions -> IO ()
action (Hakyll args) = callHakyll args
action Deploy{..}    = deploySite scratch bail
action Illiterate    = illiterateClojure
