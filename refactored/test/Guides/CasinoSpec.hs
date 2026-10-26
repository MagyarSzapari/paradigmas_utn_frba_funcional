module Guides.CasinoSpec (spec) where

import Data.List (nub)
import Guides.Casino
import Test.Hspec
import Test.QuickCheck

spec :: Spec
spec = do
  describe "suits" $ do
    it "there are four legal suit names" $ length validSuitNames `shouldBe` 4
    it "names of the suits" $ suitName Clubs `shouldBe` "Tréboles"

  describe "given helpers" $ do
    it "occurrencesOf" $ occurrencesOf 'a' "banana" `shouldBe` 3
    it "concatenate agrees with concat" $
      property $ \xss -> concatenate xss == concat (xss :: [[Int]])
    it "concatenate is lazy enough for infinite lists" $
      take 5 (concatenate (repeat [1, 2 :: Int])) `shouldBe` [1, 2, 1, 2, 1]

  describe "largerBy" $ do
    it "returns the element, not its image" $ largerBy length "ab" "abc" `shouldBe` "abc"
    it "keeps the first on ties" $ largerBy length "ab" "cd" `shouldBe` "ab"

  describe "maximumBy'" $ do
    it "returns the element that maximises f" $
      maximumBy' (negate . abs) [-5, 2, -1, 7 :: Int] `shouldBe` Just (-1)
    it "returns Nothing for []" $ maximumBy' id ([] :: [Int]) `shouldBe` Nothing
    it "players: who drinks the longest drink" $
      fmap name (maximumBy' (length . drink) table) `shouldBe` Just "jamesBond"

  describe "withoutDuplicates" $ do
    it "removes repeated elements" $ withoutDuplicates [1, 2, 1, 3, 2 :: Int] `shouldBe` [1, 2, 3]
    it "agrees with nub" $ property $ \xs -> withoutDuplicates xs == nub (xs :: [Int])

  describe "isInvalidCard" $ do
    it "a legal card is valid" $ isInvalidCard (Card 1 "Picas") `shouldBe` False
    it "rank out of range" $ isInvalidCard (Card 14 "Picas") `shouldBe` True
    it "unknown suit (Copas)" $ isInvalidCard (Card 4 "Copas") `shouldBe` True
    it "James Bond's hand is clean" $ any isInvalidCard (hand jamesBond) `shouldBe` False
    it "Felix Leiter has a fake card" $ any isInvalidCard (hand felixLeiter) `shouldBe` True
