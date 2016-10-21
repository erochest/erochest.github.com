{-# LANGUAGE RecordWildCards #-}


module Actions where


import           Types

import           Sites.Actions.Deploy
import           Sites.Actions.Draft
import           Sites.Actions.Hakyll
import           Sites.Actions.Illiterate
import           Sites.Actions.Publish


action :: Actions -> IO ()
action (Hakyll args) = callHakyll args
action Draft{..}     = newDraft draftCategory draftTags draftTitle draftSlug
action Deploy{..}    = deploySite scratch bail
action Publish{..}   = publishDraft publishMetaFile publishBranch publishTo
                                    publishOn publishDeploy
action Illiterate    = illiterateClojure
