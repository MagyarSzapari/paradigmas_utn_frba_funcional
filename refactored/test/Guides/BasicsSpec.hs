module Guides.BasicsSpec (spec) where

import Guides.Basics
import Test.Hspec
import Test.QuickCheck

approxEq :: Double -> Double -> Bool
approxEq a b = abs (a - b) < 1e-9

spec :: Spec
spec = do
  describe "double" $
    it "doubles a number (original smoke test)" $
      double 1 `shouldBe` 2

  describe "isMultipleOfThree / isMultipleOf" $ do
    it "9 is a multiple of 3" $ isMultipleOfThree 9 `shouldBe` True
    it "10 is not a multiple of 3" $ isMultipleOfThree 10 `shouldBe` False
    it "12 is a multiple of 4" $ (12 `isMultipleOf` 4) `shouldBe` True
    it "x * k is always a multiple of k (k /= 0)" $
      property $ \x (NonZero k) -> (x * k) `isMultipleOf` k

  describe "cube" $ do
    it "cube of 3 is 27" $ cube (3 :: Int) `shouldBe` 27
    it "works for doubles too" $ cube (1.5 :: Double) `shouldBe` 3.375

  describe "triangleArea" $
    it "base 4 and height 3 gives 6" $ triangleArea 4 3 `shouldBe` 6

  describe "isLeapYear" $ do
    it "2024 is a leap year (the original said False)" $ isLeapYear 2024 `shouldBe` True
    it "2000 is a leap year" $ isLeapYear 2000 `shouldBe` True
    it "1900 is not a leap year" $ isLeapYear 1900 `shouldBe` False
    it "2023 is not a leap year" $ isLeapYear 2023 `shouldBe` False
    it "agrees with the guards version" $
      property $ \year -> isLeapYear year == isLeapYearGuards year

  describe "temperatures" $ do
    it "100 C is 212 F" $ celsiusToFahrenheit 100 `shouldSatisfy` approxEq 212
    it "32 F is 0 C" $ fahrenheitToCelsius 32 `shouldBe` 0
    it "round trip is (almost) the identity" $
      property $ \c ->
        abs (fahrenheitToCelsius (celsiusToFahrenheit c) - c) <= 1e-9 * max 1 (abs c)
    it "40 F is cold" $ isColdFahrenheit 40 `shouldBe` True
    it "50 F is not cold" $ isColdFahrenheit 50 `shouldBe` False

  describe "leastCommonMultiple" $ do
    it "lcm of 4 and 6 is 12" $ leastCommonMultiple 4 6 `shouldBe` 12
    it "lcm with 0 is 0 (the original divided by zero)" $ leastCommonMultiple 0 0 `shouldBe` 0
    it "is a multiple of both arguments" $
      property $ \(NonZero a) (NonZero b) ->
        let m = leastCommonMultiple a b in m `isMultipleOf` a && m `isMultipleOf` b

  describe "dispersion and days" $ do
    it "dispersion of 10 50 30 is 40" $ dispersion 10 50 30 `shouldBe` 40
    it "is invariant under permutations" $
      property $ \x y z -> dispersion x y z == dispersion z x y && dispersion x y z == dispersion y x z
    it "is never negative" $ property $ \x y z -> dispersion x y z >= 0
    it "even days" $ areEvenDays 10 20 30 `shouldBe` True
    it "crazy days" $ areCrazyDays 0 50 150 `shouldBe` True
    it "normal days" $ areNormalDays 0 50 80 `shouldBe` True
    it "exactly one category always holds" $
      property $ \x y z ->
        length (filter id [areEvenDays x y z, areCrazyDays x y z, areNormalDays x y z]) == 1

  describe "pines" $ do
    it "200 cm weighs 600 kg" $ pineWeight 200 `shouldBe` 600
    it "400 cm weighs 1100 kg" $ pineWeight 400 `shouldBe` 1100
    it "500 kg is useful, 1000 kg is not" $
      (isUsefulWeight 500, isUsefulWeight 1000) `shouldBe` (True, False)
    it "a 200 cm pine is useful" $ isUsefulPine 200 `shouldBe` True
    it "a 400 cm pine is not useful" $ isUsefulPine 400 `shouldBe` False

  describe "isPerfectSquare" $ do
    it "49 is a perfect square" $ isPerfectSquare 49 `shouldBe` True
    it "50 is not" $ isPerfectSquare 50 `shouldBe` False
    it "every n^2 is a perfect square" $
      property $ \(NonNegative n) -> n < 10000 ==> isPerfectSquare (n * n)
