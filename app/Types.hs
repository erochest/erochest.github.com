module Types where


import qualified Data.Text as T


data Actions
    = Hakyll  { hakyllArgs :: ![T.Text] }
    | Deploy  { scratch    :: !Bool
              , bail       :: !Bool
              }
    | Illiterate
    deriving (Show, Eq)
