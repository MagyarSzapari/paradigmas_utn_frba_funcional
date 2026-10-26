module Guides.UtilsSpec (spec) where

import Guides.Utils
import Test.Hspec
import Test.QuickCheck

spec :: Spec
spec = do
  describe "maximumOn" $ do
    it "returns the element with the greatest image" $
      maximumOn length ["a", "abc", "ab"] `shouldBe` Just "abc"
    it "last element wins on ties (like the original maximoSegun)" $
      maximumOn length ["ab", "cd"] `shouldBe` Just "cd"
    it "Nothing for an empty list" $ maximumOn id ([] :: [Int]) `shouldBe` Nothing
    it "agrees with maximum when f is id" $
      property $ \(NonEmpty xs) -> maximumOn id xs == Just (maximum (xs :: [Int]))

  describe "deltaBy" $ do
    it "absolute change after a transformation" $ deltaBy negate (+ 3) (10 :: Int) `shouldBe` 3
    it "is never negative" $
      property $ \n x -> deltaBy id (+ n) (x :: Int) >= 0

  describe "average" $ do
    it "mean of 2 and 4 is 3" $ average [2, 4] `shouldBe` 3
    it "mean of [] is 0 by convention" $ average [] `shouldBe` 0
