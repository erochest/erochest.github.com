{-# LANGUAGE RecordWildCards #-}


module Actions where


import           Types

import           Sites.Actions.Deploy
import           Sites.Actions.Draft
import           Sites.Actions.Hakyll
import           Sites.Actions.Illiterate
import           Sites.Actions.Publish
import           Sites.Actions.WorkDraft
import           Sites.Actions.WorkPublish


action :: Actions -> IO ()
action (Hakyll args) = callHakyll args
action Draft{..}     = newDraft draftCategory draftTags draftTitle draftSlug
                                draftType
action Deploy{..}    = deploySite scratch bail
action Publish{..}   = publishDraft publishMetaFile publishBranch publishOn
                                    publishDeploy
action Illiterate    = illiterateClojure
action WorkStart{..} = workDraft workStartAuthor workStartTitle workStartTag
                                 workStartRange
action WorkDone{..}  = workPublish workDoneFile workDoneBranch workDoneOn
                                   workDoneDeploy
