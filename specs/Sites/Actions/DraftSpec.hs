{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}


module Sites.Actions.DraftSpec where


import           Data.Monoid
import qualified Data.Text           as T
import           Data.Time
import           Shelly              (shelly)
import           Test.Hspec

import           Sites.Actions.Draft (renderHeader, yamlHeader)


spec :: Spec
spec = describe "yamlHeader" $ do
    it "should escape titles with colons." $ do
      now <- formatTime defaultTimeLocale
             (iso8601DateFormat (Just "%H:%M:%SZ"))
             <$> getCurrentTime
      header <- shelly $ yamlHeader "title: hi" ["tag"]

      renderHeader header `shouldBe`
        "\
        \---\n\
        \categories:\n\
        \- tag\n\
        \date: " <> T.pack now <> "\n\
        \title: ! 'title: hi'\n\
        \---\n\
        \\n"
