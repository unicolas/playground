module Training.WhichSpec (spec) where

import Test.Hspec (describe, it, Spec, shouldBe)
import Training.Which (areIn)

spec :: Spec
spec = do
  describe "Which are in? tests" $ do
    it "finds the overlapping words" $ 
      let a1 = ["arp", "live", "strong"]
          a2 = ["lively", "alive", "harp", "sharp", "armstrong"]
      in areIn a1 a2 `shouldBe` ["arp", "live", "strong"]
     
    it "returns the empty list if there's no overlapping" $ 
      let a1 = ["tarp", "mice", "bull"]
          a2 = ["lively", "alive", "harp", "sharp", "armstrong"]
      in areIn a1 a2 `shouldBe` []
      
    it "removes duplicates" $ 
      let a1 = ["duplicates", "duplicates"]
          a2 = a1
      in areIn a1 a2 `shouldBe` ["duplicates"]
    
    it "returns sorted elements" $ 
      let a1 = map return ['z','y'..'a']
          a2 = a1
      in areIn a1 a2 `shouldBe` map return ['a'..'z']

    it "returns the empty list if either list is empty" $ do
      let a1 = ["tarp", "mice"]
          a2 = []
      areIn a1 a2 `shouldBe` []
      areIn a2 a1 `shouldBe` []
      areIn a2 a2 `shouldBe` []
