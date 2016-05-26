module Actions where


import           Types

import           Sites.Actions.Build


action :: Actions -> IO ()
action (Build args) = buildSite args
