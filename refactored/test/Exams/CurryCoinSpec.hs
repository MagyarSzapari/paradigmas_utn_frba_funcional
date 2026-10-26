module Exams.CurryCoinSpec (spec) where

import Exams.CurryCoin
import Test.Hspec
import Test.QuickCheck

genAccount :: Gen Account
genAccount = Account <$> elements ["alice", "bob", "carol"] <*> chooseInt (-1000, 1000)

spec :: Spec
spec = do
  describe "Transactions" $ do
    it "a payment lowers the balance" $
      payment 30 alice `shouldBe` Account "alice" 70
    it "a collection raises the balance" $
      collection 30 bob `shouldBe` Account "bob" 80
    it "mining rewards 25 coins" $
      balance (mining carol) `shouldBe` 25

  describe "Searching accounts" $ do
    it "finds the first account with an id" $
      firstMatching (hasId "bob") sampleAccounts `shouldBe` Just bob
    it "returns Nothing for an unknown id (the original crashed with head [])" $
      firstMatching (hasId "dave") sampleAccounts `shouldBe` Nothing
    it "removes only the first matching account" $
      withoutFirstMatching (hasId "bob") [alice, bob, carol, bob] `shouldBe` [alice, carol, bob]

  describe "Updating accounts" $ do
    it "modifies the right account and keeps the order" $
      modifyAccount "bob" mining sampleAccounts `shouldBe` [alice, Account "bob" 75, carol]
    it "leaves the accounts unchanged for an unknown id" $
      modifyAccount "dave" mining sampleAccounts `shouldBe` sampleAccounts
    it "applies a whole block" $
      applyBlock sampleBlock sampleAccounts
        `shouldBe` [Account "alice" 70, Account "bob" 80, Account "carol" 25]
    it "applies a blockchain block after block" $
      applyBlockchain [sampleBlock, sampleBlock] sampleAccounts
        `shouldBe` [Account "alice" 40, Account "bob" 110, Account "carol" 50]

  describe "Properties" $ do
    it "paying and then collecting the same amount is the identity" $
      forAll genAccount $ \account ->
        forAll (chooseInt (0, 500)) $ \amount ->
          (collection amount . payment amount) account === account
    it "a block of payments+collections between accounts preserves the total balance" $
      forAll (chooseInt (0, 500)) $ \amount ->
        let block = [("alice", payment amount), ("bob", collection amount)]
         in sum (map balance (applyBlock block sampleAccounts)) === sum (map balance sampleAccounts)
    it "applying an empty blockchain is the identity" $
      forAll (listOf genAccount) $ \accounts -> applyBlockchain [] accounts === accounts
    it "updateFirst never changes the length of the list" $
      forAll (listOf genAccount) $ \accounts ->
        length (modifyAccount "bob" mining accounts) === length accounts
