{-# LANGUAGE OverloadedStrings #-}


module Sites.Git where


import           Control.Error
import qualified Data.Text     as T
import           Shelly        (Sh, command1, command1_)

import           Sites.Types


git_ :: T.Text -> [T.Text] -> Sh ()
git_ = command1_ "git" []

git :: T.Text -> [T.Text] -> Sh T.Text
git = command1 "git" []

-- git branch --list | grep '*' | cut -d ' ' -f 2
currentBranch :: Sh (Maybe T.Text)
currentBranch =   headZ
              .   fmap (T.drop 2)
              .   filter ("* " `T.isPrefixOf`)
              .   T.lines
              <$> git "branch" ["--list"]

merge :: T.Text -> BranchMove -> Sh ()
merge current (BranchMove mSrcB destB) = do
    git_ "checkout" [destB]
    git_ "merge"    [srcB]
    git_ "branch"   ["-d", srcB]
    where
        srcB = fromMaybe current mSrcB
