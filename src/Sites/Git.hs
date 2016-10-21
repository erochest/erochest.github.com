{-# LANGUAGE OverloadedStrings #-}


module Sites.Git where


import qualified Data.Text as T
import           Shelly    (Sh, command1_)


git_ :: T.Text -> [T.Text] -> Sh ()
git_ = command1_ "git" []
