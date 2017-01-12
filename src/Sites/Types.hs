{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}


module Sites.Types where


import           Data.Data
import qualified Data.Text    as T
import           Data.Time
import           Data.Yaml    (Value)
import           GHC.Generics
import           Hakyll
import           Shelly       (Sh)


data SiteInfo
    = Site
    { siteName   :: T.Text     -- ^ An identifier.
    , siteTarget :: FilePath   -- ^ Where to put the site (under `_deploy`).
    , siteRoot   :: FilePath   -- ^ The root of the site under `_site`.
    , siteRules  :: Rules ()   -- ^ These are the Hakyll rules for building
                               -- this site.
    } deriving (Generic, Typeable)

data RootSite
    = Root
    { rootSite :: SiteInfo
    , subsites :: [SiteInfo]
    } deriving (Generic, Typeable)

data ProjectInfo
    = Project
    { projectId         :: !String
    , projectTitle      :: !String
    , projectTeaser     :: !String
    , projectCoverImage :: !FilePath
    , projectDate       :: !UTCTime
    } deriving (Eq, Show, Generic, Data, Typeable)

data PageType
    = MarkdownPage
    | ClojurePage
    | PureScriptPage
    | ReadingLogPage
    deriving (Show, Eq, Generic, Data, Typeable)

data BranchMove
    = BranchMove
    { branchFrom :: !(Maybe T.Text)
    , branchTo   :: !T.Text
    } deriving (Show, Eq, Generic, Data, Typeable)

data DraftInfo
    = DraftInfo
    { draftTitle     :: !T.Text
    , draftRepo      :: !FilePath
    , draftBranch    :: !T.Text
    , draftDirectory :: !FilePath
    , draftHeader    :: !Value
    , draftBuilder   :: !(DraftInfo -> Sh [FilePath])
    } deriving (Generic, Typeable)

