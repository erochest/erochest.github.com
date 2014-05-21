{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE NoImplicitPrelude  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE DeriveDataTypeable #-}


-- | Passing in the option `--scratch` will cause the `site` executable to be
-- rebuilt from scratch.
--
-- Passing in the option `--bail` will cause it to skip commiting and pushing
-- the changes.


module Main where


import           ClassyPrelude
import           Data.Data
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import           Shelly hiding ((</>))
import           Sites (site, SiteInfo(..), RootSite(..))
import           System.Locale
import           System.Console.CmdArgs

default (TL.Text)


cabal_ :: Text -> [Text] -> Sh ()
cabal_ = command1_ "cabal" []

git_ :: Text -> [Text] -> Sh ()
git_ = command1_ "git" []

gitCommit :: Text -> Sh ()
gitCommit msg = git_ "commit" ["-m", msg]

clearDeploy :: Sh ()
clearDeploy =
        ls "_deploy/" >>=
        fmap (filter ((".git" /=) . filename) . concat) . mapM ls >>=
        mapM_ rm_rf

copySite :: FilePath -> Sh ()
copySite dest = mapM_ (`cp_r` dest) =<< ls "_site"

deploySite :: Text -> Text -> FilePath -> Sh ()
deploySite branch msg dir =
        chdir dir $ do
            git_ "checkout" [branch]
            git_ "add" ["--all"]
            errExit False $ do
                gitCommit msg
                whenM ((== 0) <$> lastExitCode) $
                    git_ "push" ["origin", "master"]


main :: IO ()
main = shelly $ verbosely $ do
    Deploy{..} <- liftIO $ cmdArgs deployArgs
    now  <-  T.pack . formatTime defaultTimeLocale rfc822DateFormat
         <$> liftIO getCurrentTime
    Root{..} <- liftIO site
    let rootDeploy = ("_deploy" </>) $ siteTarget rootSite

    when scratch $ do
        cabal_ "clean"     []
        cabal_ "configure" []
        cabal_ "build"     []

    cabal_ "run" ["site", "rebuild"]
    clearDeploy
    copySite rootDeploy
    forM_ subsites $ \s ->
        let src  = "_deploy" </> siteTarget rootSite </> siteRoot s
            dest = "_deploy" </> siteTarget s
        in  ls src >>=
            mapM_ (`mv` dest) >>
            rm_rf src

    unless bail $ do
        let msg = "Deployed on " ++ now ++ "."
        deploySite "master" msg rootDeploy
        forM_ subsites (deploySite "gh-pages" msg . ("_deploy" </>) . siteTarget)
        git_ "add" ["_deploy"]
        errExit False $
            gitCommit msg

-- command-line parsing

data DeployArgs = Deploy
                { scratch :: Bool
                , bail    :: Bool
                } deriving (Show, Data, Typeable)

deployArgs :: DeployArgs
deployArgs
    = Deploy
    { scratch = False &= help "If given, this cleans the project and \
                              \rebuilds everything from scratch. (Default \
                              \is false.)"
    , bail    = False &= help "If given, this will bail before actually \
                              \deploying the site."
    }

