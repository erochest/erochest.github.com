{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}


module Sites.Actions.DraftSpec where


import           Test.Hspec

import           Sites.Actions.Draft (yamlHeader)


spec :: Spec
spec = describe "yamlHeader" $ do
    it "should escape titles with colons." $
        yamlHeader "title: hi" ["tag"] "timestamp" `shouldBe` "\
            \---\n\
            \categories:\n\
            \- tag\n\
            \date: timestamp\n\
            \title: ! 'title: hi'\n\
            \---\n\
            \"
