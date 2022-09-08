module Training.CompareVersionsSpec (spec) where

import Test.Hspec (describe, it, shouldBe, Spec)
import Training.CompareVersions (compareVersions)

spec :: Spec
spec = do
  describe "compareVersions" $ do
    it "returns GT if version1 is greater than version2" $ do
      compareVersions "11" "10" `shouldBe` GT
      compareVersions "10.4.6" "10.4" `shouldBe` GT
      compareVersions "10.10" "10.9"  `shouldBe` GT
    
    it "returns EQ if version1 is equal to version2" $ do
      compareVersions "10" "10" `shouldBe` EQ
      compareVersions "10.9" "10.9" `shouldBe` EQ
      compareVersions "10.4.6" "10.4.6" `shouldBe` EQ
      
    it "returns LT if version1 is less than version2" $ do
      compareVersions "10" "11" `shouldBe` LT
      compareVersions "10.4" "10.4.6" `shouldBe` LT
      compareVersions "10.99" "10.100" `shouldBe` LT
