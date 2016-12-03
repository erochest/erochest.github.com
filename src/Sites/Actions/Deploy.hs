{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}


module Sites.Actions.Deploy where


import           Data.Foldable
import           Data.Monoid
import           Data.Text       (Text)
import qualified Data.Text       as T
import           Data.Time
import           Shelly          hiding (FilePath, (</>))
import qualified Shelly          as S
import           System.FilePath
import qualified System.FilePath as FP
import Git.Types
import Git.Libgit2

import           Sites           (RootSite (..), SiteInfo (..), site)


deploySite :: Bool -> Bool -> IO ()
deploySite scratch bail = shelly $ verbosely $ do
    repo <- openLgRepository repoOpts
    now  <-  T.pack . formatTime defaultTimeLocale rfc822DateFormat
         <$> liftIO getCurrentTime
    Root{..} <- liftIO site
    let rootDeploy = ("_deploy" </>) $ siteTarget rootSite

    when scratch $ do
        stack_ "clean" []
        stack_ "build" []

    stack_ "exec" ["--", "errsite", "hakyll", "clean"]
    stack_ "exec" ["--", "errsite", "hakyll", "build"]
    clearDeploy
    copySite rootDeploy
    forM_ subsites $ \s ->
        let src  = filePathString $ "_deploy" </> siteTarget rootSite </> siteRoot s
            dest = filePathString $ "_deploy" </> siteTarget s
        in  ls src >>=
            mapM_ (`mv` dest) >>
            rm_rf src

    unless bail $ do
        let msg = "Deployed on " <> now <> "."
        deploySite' "master" msg rootDeploy
        forM_ subsites ( deploySite' "gh-pages" msg
                       . ("_deploy" </>)
                       . siteTarget)
        git_ "add" ["_deploy"]
        errExit False $
            gitCommit msg

    where
        repoOpts = RepositoryOptions "./.git" (Just ".") False False

stack_ :: Text -> [Text] -> Sh ()
stack_ = command1_ "stack" []

clearDeploy :: Sh ()
clearDeploy =
        ls "_deploy/"
    >>= mapM ls
    >>= mapM_ rm_rf
    .   filter ((".git" /=) . FP.takeFileName . T.unpack . toTextIgnore)
    .   fold

copySite :: FilePath -> Sh ()
copySite dest = mapM_ (`cp_r` dest') =<< ls "_site"
    where
        dest' = filePathString dest

deploySite' :: Text -> Text -> FilePath -> Sh ()
deploySite' branch msg dir =
    chdir (filePathString dir) $ do
        git_ "checkout" [branch]
        git_ "add" ["--all"]
        errExit False $ do
            gitCommit msg
            whenM ((== 0) <$> lastExitCode) $
                git_ "push" []

filePathString :: String -> S.FilePath
filePathString = fromText . T.pack
