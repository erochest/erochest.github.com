{-# LANGUAGE OverloadedStrings #-}


module Sites.UtilsSpec where


import           Test.Hspec

import           Sites.Utils (slugify)


spec :: Spec
spec = do
  describe "slugify" $ do
    it "should remove subtitles." $ do
      slugify "This is a title: This is a subtitle" `shouldBe`
        "this-is-a-title"
