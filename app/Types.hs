module Types where


import qualified Data.HashSet as S
import qualified Data.Text    as T
import           Data.Time


data Actions
    = Hakyll  { hakyllArgs    :: ![T.Text]
              }
    | Draft   { draftCategory :: !FilePath
              , draftTags     :: !(S.HashSet T.Text)
              , draftTitle    :: !T.Text
              , draftSlug     :: !(Maybe FilePath)
              }
    | Publish { publishMetaFile :: !FilePath
              , publishBranch   :: !T.Text
              , publishTo       :: !T.Text
              , publishOn       :: !(Maybe UTCTime)
              , publishDeploy   :: !Bool
              }
    | Deploy  { scratch :: !Bool
              , bail    :: !Bool
              }
    | Illiterate
    deriving (Show, Eq)
