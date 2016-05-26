module Types where


import qualified Data.Text as T


data Actions
    = Build  { buildArgs :: ![T.Text] }
    | Deploy { scratch   :: !Bool
             , bail      :: !Bool
             }
    deriving (Show, Eq)
