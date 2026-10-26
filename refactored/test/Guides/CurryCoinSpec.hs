module Guides.CurryCoinSpec (spec) where

import Guides.CurryCoin
import Test.Hspec
import Test.QuickCheck

alice, bob, carol :: Account
alice = Account "alice" 100
bob = Account "bob" 10
carol = Account "carol" 0

accounts :: [Account]
accounts = [alice, bob, carol]

spec :: Spec
spec = do
  describe "transactions" $ do
    it "payment subtracts" $ balance (payment 30 alice) `shouldBe` 70
    it "collection adds" $ balance (collection 30 alice) `shouldBe` 130
    it "mining adds 25" $ balance (mining carol) `shouldBe` 25
    it "payment undoes collection" $
      property $ \amount b ->
        let account = Account "x" (fromIntegral (b :: Int))
            restored = payment (fromIntegral (amount :: Int)) (collection (fromIntegral amount) account)
         in restored == account

  describe "lookups" $ do
    it "hasId" $ hasId "bob" bob `shouldBe` True
    it "firstMatching" $ firstMatching ((> 5) . balance) accounts `shouldBe` Just alice
    it "firstMatching with no match" $ firstMatching ((> 500) . balance) accounts `shouldBe` Nothing
    it "withoutFirstMatching removes only the first" $
      withoutFirstMatching even [1, 2, 3, 4 :: Int] `shouldBe` [1, 3, 4]
    it "withoutFirstMatching removes at most one element" $
      property $ \xs ->
        let ys = withoutFirstMatching even (xs :: [Int])
         in length xs - length ys == (if any even xs then 1 else 0)

  describe "updateAccount" $ do
    it "applies the transaction keeping the position" $
      updateAccount "bob" accounts mining `shouldBe` [alice, Account "bob" 35, carol]
    it "unknown id leaves the accounts unchanged (the original crashed)" $
      updateAccount "dave" accounts mining `shouldBe` accounts

  describe "blocks" $ do
    let block = [("alice", payment 50), ("bob", collection 50), ("carol", mining)]
    it "runBlock applies every transaction" $
      runBlock block accounts `shouldBe` [Account "alice" 50, Account "bob" 60, Account "carol" 25]
    it "areStable" $ areStable accounts `shouldBe` True
    it "not stable with a negative balance" $ areStable [Account "x" (-1)] `shouldBe` False

  describe "isStableBlockchain" $ do
    let spendForty = [("alice", payment 40)]
    it "two blocks of -40 keep alice at 20: stable" $
      isStableBlockchain [spendForty, spendForty] accounts `shouldBe` True
    it "three blocks accumulate to -20: unstable (the original said stable)" $
      isStableBlockchain [spendForty, spendForty, spendForty] accounts `shouldBe` False
    it "the empty chain is stable" $ isStableBlockchain [] accounts `shouldBe` True

  describe "shamelessFunction" $ do
    it "returns id when more than 10 evens" $
      shamelessFunction [[1 .. 30 :: Int]] (id, (+ 1)) (5 :: Int) `shouldBe` 5
    it "returns the second function otherwise" $
      shamelessFunction [[1 .. 3 :: Int]] (id, (+ 1)) (5 :: Int) `shouldBe` 6
    it "does not crash on an empty list" $
      shamelessFunction ([] :: [[Int]]) (id, (+ 1)) (5 :: Int) `shouldBe` 6
