module Guides.SamuraiJackSpec (spec) where

import Guides.SamuraiJack
import Test.Hspec
import Test.QuickCheck hiding (elements)

weakling :: Character
weakling = Character "Weakling" 10 [minion] 200

spec :: Spec
spec = do
  describe "basic changes" $ do
    it "sendToYear" $ currentYear (sendToYear 3000 jack) `shouldBe` 3000
    it "meditate multiplies health by 1.5" $ health (meditate jack) `shouldBe` 450
    it "takeDamage subtracts" $ health (takeDamage 100 jack) `shouldBe` 200
    it "takeDamage never goes below 0" $ health (takeDamage 1000 jack) `shouldBe` 0
    it "health is never negative" $
      property $ \d -> health (takeDamage d jack) >= 0

  describe "evil and damage" $ do
    it "Aku is evil" $ isEvil (aku 1 100) `shouldBe` True
    it "Jack is not evil" $ isEvil jack `shouldBe` False
    it "damageDealt by a minion is 1" $ damageDealt jack minion `shouldBe` 1
    it "the katana is deadly for Jack" $ hasDeadlyAttack jack magicKatana `shouldBe` True
    it "a minion is not deadly for Jack" $ hasDeadlyAttack jack minion `shouldBe` False
    it "deadlyEnemies" $
      map characterName (deadlyEnemies weakling [jack, aku 1 100]) `shouldBe` ["Jack"]
    it "isDead" $ isDead (takeDamage 300 jack) `shouldBe` True

  describe "elements" $ do
    it "elements are compared by kind" $ minion `shouldBe` head (evilMinions 1)
    it "show displays the kind" $ show magicKatana `shouldBe` "Magic"
    it "concentration 3 meditates three times" $
      health (defense (concentration 3) jack) `shouldBe` 300 * 1.5 * 1.5 * 1.5
    it "concentration 0 does nothing" $ health (defense (concentration 0) jack) `shouldBe` 300
    it "Aku has 2 + 100 * year elements" $ length (elements (aku 2 100)) `shouldBe` 202

  describe "Aku's future portal (laziness)" $ do
    let portal = futurePortalFrom 100
    it "attack sends to the future" $ currentYear (attack portal jack) `shouldBe` 2900
    it "defense creates a new Aku keeping the health" $ do
      let newAku = defense portal (aku 100 50)
      (characterName newAku, currentYear newAku, health newAku) `shouldBe` ("Aku", 2900, 50)

  describe "useElements" $
    it "applies every attack in order" $
      health (useElements attack jack (evilMinions 3)) `shouldBe` 297

  describe "fight" $ do
    let (winner, loser) = fight jack weakling
    it "Jack wins against a weakling" $ characterName winner `shouldBe` "Jack"
    it "the loser is dead" $ isDead loser `shouldBe` True
    it "Jack meditated during the fight" $ health winner `shouldBe` 300 * 1.5 * 1.5 * 1.5

  describe "inferenceExample" $ do
    it "uses fst when y 0 == z" $
      inferenceExample (\b a -> (a + b, a - b)) (+ (5 :: Int)) 5 [1, 2 :: Int] `shouldBe` [6, 7]
    it "uses snd otherwise" $
      inferenceExample (\b a -> (a + b, a - b)) (+ (5 :: Int)) 7 [1, 2 :: Int] `shouldBe` [-4, -3]
