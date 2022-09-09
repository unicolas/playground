module Training.StringMixSpec (spec) where

import Test.Hspec (describe, it, shouldBe, Spec)
import Training.StringsMix (mix)

spec :: Spec
spec = do
  describe "String Mix tests" $ do
    it "Returns mix" $ do
      mix "Are they here" "yes, they are here" `shouldBe` "2:eeeee/2:yy/=:hh/=:rr"
      mix "looping is fun but dangerous" "less dangerous than coding" `shouldBe` "1:ooo/1:uuu/2:sss/=:nnn/1:ii/2:aa/2:dd/2:ee/=:gg"
      mix " In many languages" " there's a pair of functions" `shouldBe` "1:aaa/1:nnn/1:gg/2:ee/2:ff/2:ii/2:oo/2:rr/2:ss/2:tt"        
      mix "Lords of the Fallen" "gamekult" `shouldBe` "1:ee/1:ll/1:oo"
      mix "codewars" "codewars" `shouldBe` ""
      mix "A generation must confront the looming " "codewarrs" `shouldBe` "1:nnnnn/1:ooooo/1:tttt/1:eee/1:gg/1:ii/1:mm/=:rr"
