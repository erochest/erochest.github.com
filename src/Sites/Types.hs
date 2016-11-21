
module Sites.Types where


import qualified Data.Text as T
import           Data.Time
import           Hakyll


data SiteInfo
    = Site
    { siteName   :: T.Text     -- ^ An identifier.
    , siteTarget :: FilePath   -- ^ Where to put the site (under `_deploy`).
    , siteRoot   :: FilePath   -- ^ The root of the site under `_site`.
    , siteRules  :: Rules ()   -- ^ These are the Hakyll rules for building
                               -- this site.
    }

data RootSite
    = Root
    { rootSite :: SiteInfo
    , subsites :: [SiteInfo]
    }

data ProjectInfo
    = Project
    { projectId         :: !String
    , projectTitle      :: !String
    , projectTeaser     :: !String
    , projectCoverImage :: !FilePath
    , projectDate       :: !UTCTime
    } deriving (Show)

data PageType
    = MarkdownPage
    | ClojurePage
    | PureScriptPage
    deriving (Show, Eq)
