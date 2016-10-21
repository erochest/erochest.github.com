{-# LANGUAGE RecordWildCards #-}


module Actions where


import           Types

import           Sites.Actions.Hakyll
import           Sites.Actions.Draft
import           Sites.Actions.Deploy
import           Sites.Actions.Illiterate


action :: Actions -> IO ()
action (Hakyll args) = callHakyll args
action Draft{..}     = newDraft draftCategory draftTags draftTitle draftSlug
action Deploy{..}    = deploySite scratch bail
action Illiterate    = illiterateClojure
