module Types where


import qualified Data.HashSet as S
import qualified Data.Text    as T
import           Data.Time

import           Sites.Types


data Actions
    = Hakyll  { hakyllArgs    :: ![T.Text]
              }
    | Draft   { draftCategory :: !FilePath
              , draftTags     :: !(S.HashSet T.Text)
              , draftTitle    :: !T.Text
              , draftSlug     :: !(Maybe FilePath)
              , draftType     :: !PageType
              }
    | Publish { publishMetaFile :: !FilePath
              , publishBranch   :: !(Maybe BranchMove)
              , publishOn       :: !(Maybe UTCTime)
              , publishDeploy   :: !Bool
              }
    | WorkStart
              { workStartAuthor :: !T.Text
              , workStartTitle  :: !T.Text
              , workStartTag    :: !(S.HashSet T.Text)
              , workStartRange  :: !Bool
              }
    | WorkDone
              { workDoneFile   :: !FilePath
              , workDoneBranch :: !(Maybe BranchMove)
              , workDoneOn     :: !(Maybe UTCTime)
              , workDoneDeploy :: !Bool
              }
    | Deploy  { scratch :: !Bool
              , bail    :: !Bool
              }
    | Illiterate
    deriving (Show, Eq)
