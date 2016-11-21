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
              , publishBranch   :: !(Maybe T.Text)
              , publishTo       :: !T.Text
              , publishOn       :: !(Maybe UTCTime)
              , publishDeploy   :: !Bool
              }
    | Deploy  { scratch :: !Bool
              , bail    :: !Bool
              }
    | Illiterate
    deriving (Show, Eq)
