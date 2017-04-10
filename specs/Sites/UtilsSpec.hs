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

    it "should normalize accents." $ do
      slugify "China Miéville" `shouldBe` "china-mieville"

    it "should fold together runs of dashes." $ do
      slugify "\"This is a quote!\" she said" `shouldBe`
        "this-is-a-quote-she-said"
