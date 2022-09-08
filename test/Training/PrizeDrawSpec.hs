module Training.PrizeDrawSpec (spec) where

import Test.Hspec (describe, it, shouldBe, Spec)
import Training.PrizeDraw (rank)

spec :: Spec
spec = do
  describe "Rank: Basic Tests" $ do
    it "Returns rank" $ do
      rank "Addison,Jayden,Sofia,Michael,Andrew,Lily,Benjamin" [4,2,1,4,3,1,2] 4 `shouldBe` "Benjamin"
      rank "Elijah,Chloe,Elizabeth,Matthew,Natalie,Jayden" [1,3,5,5,3,6] 2 `shouldBe` "Matthew"
      rank "Aubrey,Olivai,Abigail,Chloe,Andrew,Elizabeth" [3,1,4,4,3,2] 4 `shouldBe` "Abigail"
      rank "Lagon,Lily" [1,5] 2 `shouldBe` "Lagon"
      rank "Elijah,Michael,Avery,Sophia,Samantha" [2,1,5,2,2] 3 `shouldBe` "Sophia"
      rank "William,Willaim,Olivia,Olivai,Lily,Lyli" [1,1,1,1,1,1] 1 `shouldBe` "Willaim"
      rank "Addison,William,Jayden" [3,5,6] 1 `shouldBe` "William"
      rank "Joshua,Grace,Isabella" [1,5,4] 1 `shouldBe` "Isabella"
      rank "Elijah,Addison" [3,6] 2 `shouldBe` "Elijah"
      rank "Willaim,Liam,Daniel,Alexander" [6,4,6,2] 2 `shouldBe` "Daniel"
      rank "Avery,Olivai,Sophia,Michael,Elizabeth,Willaim,Liam" [5,5,3,2,1,3,6] 5 `shouldBe` "Sophia"
      rank "Liam,Madison,Lyli,Jacob,Matthew,Michael" [2,6,5,5,3,4] 6 `shouldBe` "Liam"
      rank "Sophia,Robert,Abigail,Grace,Lagon" [1,2,2,6,4] 5 `shouldBe` "Sophia"
      rank "Samantha,Ella" [5,6] 1 `shouldBe` "Samantha"
      rank "Aubrey,Jayden" [3,4] 2 `shouldBe` "Aubrey"
      rank "Jacob,Elijah" [4,3] 1 `shouldBe` "Elijah"
      rank "Addison,Jayden,Sofia,Michael,Andrew,Lily,Benjamin" [4,2,1,4,3,1,2] 1 `shouldBe` "Addison"
      rank "Avery,Olivai,Olivia,Sophia,Michael,Elizabeth,Willaim,Liam" [5,5,3,2,1,3,6,6] 2 `shouldBe` "Avery"
      rank "Sophia,Jacob,Isabella,Mason,Emma,William,Willaim,Olivia,Olivai,Jayden,Ava" [5,5,3,2,1,3,6,6,1,1,1] 2 `shouldBe` "Olivia"
      rank "Sophia,Jacob,Isabella,Mason,Emma,William,Willaim,Olivia,Olivai,Jayden,Ava" [5,5,3,2,1,3,6,6,1,1,1] 5 `shouldBe` "Isabella"
      rank "Sophia,Robert,Abigail,Grace,Lagon" [1,2,2,6,4] 3 `shouldBe` "Robert"

    it "Has no enough participants" $ do
      rank "Addison,Jayden,Sofia,Michael,Andrew,Lily,Benjamin" [4,2,1,4,3,1,2] 8 `shouldBe` "Not enough participants"

    it "Has no participants" $ do
      rank "" [4,2,1,4,3,1,2] 6 `shouldBe` "No participants"
