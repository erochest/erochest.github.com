{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Main where


import           ClassyPrelude
import           Data.Time
import qualified Data.Set as S
import qualified Data.Text.Lazy as TL
import           Shelly
import           System.Locale

default (TL.Text)

toSave :: S.Set FilePath
toSave = S.fromList [ "_deploy/.git"
                    , "_deploy/CNAME"
                    ]


ghc_ :: [TL.Text] -> Sh ()
ghc_ = command_ "ghc" [ "--make"
                      , "-package-conf"
                      , "./cabal-dev/packages-7.4.2.conf"
                      ]

git_ :: TL.Text -> [TL.Text] -> Sh ()
git_ = command1_ "git" []

gitCommit :: TL.Text -> Sh ()
gitCommit msg = git_ "commit" ["-m", msg]

clearDeploy :: Sh ()
clearDeploy = mapM_ rm_rf =<< filter (`S.notMember` toSave) <$> ls "_deploy"

copySite :: Sh ()
copySite = mapM_ (`cp_r` "_deploy") =<< ls "_site"

commitDeploy :: Sh ()
commitDeploy = undefined


main :: IO ()
main = shelly $ verbosely $ do
    now <- TL.pack . formatTime defaultTimeLocale rfc822DateFormat <$> liftIO getCurrentTime
    ghc_ ["site.hs"]
    run_ "site" []
    clearDeploy
    copySite
    chdir "_deploy" $ do
        git_ "add" ["--all"]
        gitCommit $ "Deployed on " ++ now ++ "."
        git_ "push" ["origin", "master"]
    git_ "add" ["_deploy"]
    gitCommit $ "Deployed on " ++ now ++ "."

