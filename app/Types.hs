module Types where


import qualified Data.HashSet as S
import qualified Data.Text    as T


data Actions
    = Hakyll  { hakyllArgs    :: ![T.Text]
              }
    | Draft   { draftCategory :: !FilePath
              , draftTags     :: !(S.HashSet T.Text)
              , draftTitle    :: !T.Text
              , draftSlug     :: !(Maybe FilePath)
              }
    | Deploy  { scratch :: !Bool
              , bail    :: !Bool
              }
    | Illiterate
    deriving (Show, Eq)
