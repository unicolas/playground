module Training.SectionSpec (spec) where

import Test.Hspec (describe, it, shouldBe, Spec)
import Training.Section (section)

spec :: Spec
spec = do
  describe "Section tests" $ do
    let inGivenSections = [300, 200, 400, 600, 100] :: [Int]
    
    it "Returns first section" $ do
      section 299 inGivenSections `shouldBe` Just 0

    it "Returns second section" $ do
      section 300 inGivenSections `shouldBe` Just 1

    it "Does not return a section" $ do
      section 1600 inGivenSections `shouldBe` Nothing

    it "Returns first section for lower bound" $ do
      section 1 inGivenSections `shouldBe` Just 0

    it "Returns last section for higher bound" $ do
      section 1599 inGivenSections `shouldBe` Just 4

