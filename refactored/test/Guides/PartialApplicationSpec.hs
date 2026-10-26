module Guides.PartialApplicationSpec (spec) where

import Guides.Basics (isLeapYear, isMultipleOf)
import Guides.PartialApplication
import Test.Hspec
import Test.QuickCheck

spec :: Spec
spec = do
  describe "sections" $ do
    it "successor" $ successor 4 `shouldBe` 5
    it "half" $ half 5 `shouldBe` 2.5
    it "inverse" $ inverse 4 `shouldBe` 0.25
    it "triple" $ triple 4 `shouldBe` 12
    it "isPositive" $ map isPositive [-1, 0, 1] `shouldBe` [False, False, True]

  describe "divides" $ do
    it "3 divides 12" $ divides 3 12 `shouldBe` True
    it "5 does not divide 12" $ divides 5 12 `shouldBe` False
    it "is isMultipleOf with flipped arguments" $
      property $ \(NonZero d) n -> divides d n == n `isMultipleOf` d

  describe "isLeapYearPointFree" $
    it "agrees with isLeapYear" $
      property $ \year -> isLeapYearPointFree year == isLeapYear year

  describe "compositions" $ do
    it "inverseSquareRoot 4 is 0.5" $ inverseSquareRoot 4 `shouldBe` 0.5
    it "addSquareOf 3 1 is 10" $ addSquareOf 3 1 `shouldBe` 10
    it "2^3 is even" $ isPowerEven 2 3 `shouldBe` True
    it "3^2 is odd" $ isPowerEven 3 2 `shouldBe` False
