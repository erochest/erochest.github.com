{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RecordWildCards   #-}


-- | Passing in the option `--bail` will cause it to skip commiting and pushing
-- the changes.


module Main where


import           ClassyPrelude
import           Data.Time
import qualified Data.List as L
import qualified Data.Set as S
import qualified Data.Text.Lazy as TL
import           Shelly hiding ((</>))
import           Sites (site, SiteInfo(..), RootSite(..))
import           System.Locale

default (TL.Text)

binDir :: FilePath
binDir = "./dist/build/site"

cabalDev_ :: TL.Text -> [TL.Text] -> Sh ()
cabalDev_ = command1_ "cabal-dev" []

git_ :: TL.Text -> [TL.Text] -> Sh ()
git_ = command1_ "git" []

gitCommit :: TL.Text -> Sh ()
gitCommit msg = git_ "commit" ["-m", msg]

clearDeploy :: Sh ()
clearDeploy =
        ls "_deploy/" >>=
        fmap (filter ((".git" /=) . filename) . concat) . mapM ls >>=
        mapM_ rm_rf

copySite :: FilePath -> Sh ()
copySite dest = mapM_ (`cp_r` dest) =<< ls "_site"

deploySite :: TL.Text -> TL.Text -> FilePath -> Sh ()
deploySite branch msg dir =
        chdir dir $ do
            git_ "checkout" [branch]
            git_ "add" ["--all"]
            gitCommit msg
            git_ "push" ["origin", "master"]


main :: IO ()
main = shelly $ verbosely $ do
    args <-  getArgs
    now  <-  TL.pack . formatTime defaultTimeLocale rfc822DateFormat
         <$> liftIO getCurrentTime
    Root{..} <- liftIO site
    let rootDeploy = ("_deploy" </>) $ siteTarget rootSite

    cabalDev_ "clean"     []
    cabalDev_ "configure" []
    cabalDev_ "build"     []

    run_ (binDir </> "site") ["rebuild"]
    clearDeploy
    copySite rootDeploy
    forM_ subsites $ \site ->
        let src  = "_deploy" </> siteTarget rootSite </> siteRoot site
            dest = "_deploy" </> siteTarget site
        in  ls src >>=
            mapM_ (`mv` dest) >>
            rm_rf src

    unless ("--bail" `elem` args) $ do
        let msg = "Deployed on " ++ now ++ "."
        deploySite "master" msg rootDeploy
        forM_ subsites (deploySite "gh-pages" msg . ("_deploy" </>) . siteTarget)
        git_ "add" ["_deploy"]
        gitCommit msg

