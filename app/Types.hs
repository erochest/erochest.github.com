module Types where


import qualified Data.Text as T


data Actions
    = Build { buildArgs :: ![T.Text] }
    deriving (Show, Eq)
