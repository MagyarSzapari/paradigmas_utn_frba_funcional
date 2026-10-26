module Guides.TuplesSpec (spec) where

import Guides.Tuples
import Test.Hspec
import Test.QuickCheck

spec :: Spec
spec = do
  describe "triple projections" $
    it "rebuild the triple" $
      property $ \t -> (fst3 t, snd3 t, thd3 t) == (t :: (Int, Char, Bool))

  describe "applyBoth" $
    it "applies both functions" $ applyBoth (succ, show) (3 :: Int) `shouldBe` (4, "3")

  describe "oddCalculation" $ do
    it "first greater: adds" $ oddCalculation (10, 3) `shouldBe` 13
    it "gap above 10: subtracts" $ oddCalculation (2, 20) `shouldBe` 18
    it "small gap: multiplies" $ oddCalculation (2, 5) `shouldBe` 10
    it "equal values no longer crash" $ oddCalculation (5, 5) `shouldBe` 25
    it "gap of exactly 10 no longer crashes" $ oddCalculation (0, 10) `shouldBe` 0

  describe "grades" $ do
    it "5 is a failing grade" $ isFailingGrade 5 `shouldBe` True
    it "passed both" $ passedBoth (6, 8) `shouldBe` True
    it "did not pass both" $ passedBoth (6, 4) `shouldBe` False
    it "promoted" $ promoted (8, 7) `shouldBe` True
    it "7 + 7 does not promote" $ promoted (7, 7) `shouldBe` False
    it "passed the first partial" $ passedFirstPartial (6, 2) `shouldBe` True

  describe "retakes" $ do
    let record = ((2, 7), (Just 6, Nothing)) :: StudentRecord
    it "final grades keep the best of each" $ finalGrades record `shouldBe` (6, 7)
    it "does not have to retake the course" $ mustRetakeCourse record `shouldBe` False
    it "has to retake when a final grade fails" $
      mustRetakeCourse ((2, 7), (Just 4, Nothing)) `shouldBe` True
    it "retook the first partial" $ retookFirstPartial record `shouldBe` True
    it "did not retake the first partial" $
      retookFirstPartial ((8, 2), (Nothing, Just 6)) `shouldBe` False
    it "retook for fun" $ retookForFun ((8, 8), (Nothing, Just 9)) `shouldBe` True
    it "did not retake for fun if not promoted" $
      retookForFun ((6, 6), (Just 8, Nothing)) `shouldBe` False
    it "did not retake for fun if no retakes" $
      retookForFun ((8, 8), (Nothing, Nothing)) `shouldBe` False

  describe "isOfAge" $ do
    it "22 is of age" $ isOfAge ("Ana", 22) `shouldBe` True
    it "21 is not" $ isOfAge ("Juan", 21) `shouldBe` False

  describe "calculate" $ do
    it "doubles an even first, increments an odd second" $ calculate (4, 5) `shouldBe` (8, 6)
    it "leaves odd first and even second untouched" $ calculate (3, 6) `shouldBe` (3, 6)
