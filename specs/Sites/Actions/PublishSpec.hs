module Sites.Actions.PublishSpec where


import Sites.Actions.Publish (upDir)

import Test.Hspec


spec :: Spec
spec =
  describe "upDir" $ do
    it "should assume the current directory for bare names." $
      upDir "filename" `shouldBe` ("./", "filename")
    it "should handle an empty input string without problems." $
      upDir "" `shouldBe` ("./", "")
    it "should return the parent directory." $
      upDir "something/else" `shouldBe` ("something/", "else")
    it "should identify the top-most given parent directory only." $
      upDir "something/else/other" `shouldBe` ("something/", "else/other")
    it "should identify the current directory even when given." $
      upDir "./something" `shouldBe` ("./", "something")
    it "should always use slashes after the parent directory for consistency." $
      upDir "something" `shouldBe` ("./", "something")
