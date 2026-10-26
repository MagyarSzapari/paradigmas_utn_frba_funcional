module Guides.ListsSpec (spec) where

import Guides.Lists
import Test.Hspec
import Test.QuickCheck

spec :: Spec
spec = do
  describe "sumList" $
    it "agrees with sum" $ property $ \xs -> sumList xs == sum xs

  describe "heart rates" $ do
    it "average heart rate" $ averageHeartRate heartRates `shouldBe` 806 / 7
    it "average of an empty list is 0" $ averageHeartRate [] `shouldBe` 0
    it "rate at minute 30" $ heartRateAt 30 `shouldBe` Just 128
    it "rate out of range is Nothing" $ heartRateAt 100 `shouldBe` Nothing
    it "negative minutes are Nothing" $ heartRateAt (-10) `shouldBe` Nothing
    it "rates until minute 30" $ heartRatesUntil 30 `shouldBe` [80, 100, 120, 128]

  describe "isPalindrome" $ do
    it "detects a palindrome split in words" $ isPalindrome ["ne", "uqu", "en"] `shouldBe` True
    it "rejects a non palindrome" $ isPalindrome ["hola", "mundo"] `shouldBe` False
    it "s ++ reverse s is always a palindrome" $
      property $ \s -> isPalindrome [s, reverse s]

  describe "call logs" $ do
    it "reduced band has more minutes (70 vs 44)" $
      bandWithMoreMinutes callLogs `shouldBe` "horarioReducido"
    it "normal band has more calls (6 vs 4)" $
      bandWithMoreCalls callLogs `shouldBe` "horarioNormal"
