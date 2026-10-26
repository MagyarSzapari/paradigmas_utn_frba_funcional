module Guides.HigherOrderSpec (spec) where

import Guides.HigherOrder
import Test.Hspec
import Test.QuickCheck

spec :: Spec
spec = do
  describe "higher order" $ do
    it "existsInTriple finds an even" $ existsInTriple even (1, 3, 4 :: Int) `shouldBe` True
    it "existsInTriple finds nothing" $ existsInTriple even (1, 3, 5 :: Int) `shouldBe` False
    it "better picks the greatest result" $ better (* 2) (+ 10) (3 :: Int) `shouldBe` 13
    it "applyToPair" $ applyToPair even (2, 3 :: Int) `shouldBe` (True, False)
    it "pairOfFunctions" $ pairOfFunctions even show (4 :: Int) `shouldBe` (True, "4")

  describe "higher order + lists" $ do
    it "isMultipleOfAny" $ isMultipleOfAny 15 [2, 3, 4] `shouldBe` True
    it "isMultipleOfAny with no divisor" $ isMultipleOfAny 7 [2, 3, 4] `shouldBe` False
    it "averages" $ averages [[2, 4], [6, 8, 10]] `shouldBe` [3, 8]
    it "averagesWithoutFails" $ averagesWithoutFails [[2, 6], [4, 8]] `shouldBe` [6, 6]
    it "bestGrades" $ bestGrades [[2, 9], [7, 3]] `shouldBe` [9, 7]
    it "passed" $ (passed [6, 7], passed [6, 5]) `shouldBe` (True, False)
    it "passedStudents" $ passedStudents [[8, 6], [2, 10]] `shouldBe` [[8, 6]]
    it "divisors of 12" $ divisors 12 `shouldBe` [1, 2, 3, 4, 6, 12]
    it "every divisor divides" $
      property $ \(Positive k) -> all (\d -> k `mod` d == 0) (divisors k)
    it "exists behaves like any" $
      property $ \xs -> exists even xs == any even (xs :: [Int])
    it "hasAnyNegative" $ (hasAnyNegative [1, -2], hasAnyNegative [1, 2]) `shouldBe` (True, False)
    it "applyFunctions" $ applyFunctions [(+ 1), (* 2)] (5 :: Int) `shouldBe` [6, 10]
    it "applyFunctions preserves length" $
      property $ \n x -> length (applyFunctions (replicate n (+ (1 :: Int))) x) == max 0 n
    it "sumOfFunctions" $ sumOfFunctions [(+ 1), (* 2)] (5 :: Int) `shouldBe` 16

  describe "skills" $ do
    it "raiseSkills caps at 12" $ raiseSkills 3 [2, 10, 12] `shouldBe` [5, 12, 12]
    it "raiseSkills never exceeds the cap" $
      property $ \n xs -> all (<= maxSkill) (raiseSkills n xs)
    it "clamped stays within [0, 12]" $
      property $ \n x -> let r = clamped (+ n) x in r >= 0 && r <= maxSkill
    it "changeSkills" $ changeSkills (* 2) [-1, 3, 7] `shouldBe` [0, 6, 12]
    it "raiseLowSkillsToFour" $ raiseLowSkillsToFour [1, 5, 13] `shouldBe` [4, 5, 12]

  describe "takeWhile family" $ do
    it "leadingEvens" $ leadingEvens [2, 4, 5, 6] `shouldBe` [2, 4]
    it "leadingEvens works on infinite lists (laziness)" $
      leadingEvens ([2, 4, 1] ++ [0 ..]) `shouldBe` [2, 4]
    it "leadingDivisorsOf" $ leadingDivisorsOf 12 [1, 2, 5, 6] `shouldBe` [1, 2]
    it "leadingNonDivisorsOf" $ leadingNonDivisorsOf 12 [5, 7, 3, 8] `shouldBe` [5, 7]

  describe "folds" $ do
    it "sumWithFold agrees with sum" $ property $ \xs -> sumWithFold xs == sum xs
    it "productWithFold agrees with product" $ property $ \xs -> productWithFold xs == product xs
    it "listDispersion" $ listDispersion [3, 9, 1, 4] `shouldBe` 8
    it "listDispersion of [] is 0" $ listDispersion [] `shouldBe` 0
    it "listDispersion is maximum - minimum" $
      property $ \(NonEmpty xs) -> listDispersion xs == maximum xs - minimum xs
