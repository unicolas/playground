module Training.XOSpec (spec) where

import Test.Hspec (describe, it, Spec, shouldBe)
import Training.XO (xo)

spec :: Spec
spec = do
  describe "XO tests" $ do
    it "Has the same amount of Xs and Os" $ do
      xo "xo" `shouldBe` True
      xo "Xo" `shouldBe` True
      xo "xxOo" `shouldBe` True
      xo "a" `shouldBe` True
      xo "abcvyz" `shouldBe` True
      xo "abXOyz" `shouldBe` True

    it "Has not the same amount of Xs and Os" $ do
      xo "X" `shouldBe` False
      xo "O" `shouldBe` False
      xo "x" `shouldBe` False
      xo "o" `shouldBe` False
      xo "xxxm" `shouldBe` False
      xo "Xxo" `shouldBe` False
      xo "Oox" `shouldBe` False
      xo "Oo" `shouldBe` False
      xo "ooom" `shouldBe` False
