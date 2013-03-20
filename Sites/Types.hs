
module Sites.Types where


import qualified Data.Set                  as S
import qualified Data.Text                 as T
import qualified Filesystem.Path.CurrentOS as FP
import           Hakyll


data SiteInfo = Site
              { siteName   :: T.Text                -- ^ An identifier.
              , siteTarget :: FP.FilePath           -- ^ Where to put the site (under `_deploy`).
              , siteRoot   :: FP.FilePath           -- ^ The root of the site under `_site`.
              , siteSticky :: S.Set FP.FilePath     -- ^ Which files in the site's target should not
                                                    -- be deleted between runs?
              , siteRules  :: Rules ()              -- ^ These are the Hakyll rules for building this site.
              }

data RootSite = Root
              { rootSite :: SiteInfo
              , subsites :: [SiteInfo]
              }

