{-# LANGUAGE OverloadedStrings #-}


module Sites.Git where


import qualified Data.Text as T
import           Shelly    (Sh, command1, command1_)


git_ :: T.Text -> [T.Text] -> Sh ()
git_ = command1_ "git" []

git :: T.Text -> [T.Text] -> Sh T.Text
git = command1 "git" []
