module Training.BreakingChocolateSpec (spec) where

import Test.Hspec (describe, it, shouldBe, Spec)
import Training.BreakingChocolate (breakChocolate)

spec :: Spec
spec = do
  describe "Breaking Chocolate tests" $ do
    it "Calculates the correct number of breaks" $ do
      breakChocolate 5 5 `shouldBe` 24
      breakChocolate 7 4 `shouldBe` 27
      breakChocolate 1 1 `shouldBe` 0
      breakChocolate 6 1 `shouldBe` 5
      
    it "Works if there is no chocolate" $ do
      breakChocolate 0 5 `shouldBe` 0
      breakChocolate 1 0 `shouldBe` 0
