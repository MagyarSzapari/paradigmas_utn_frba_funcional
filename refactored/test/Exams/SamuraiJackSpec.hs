module Exams.SamuraiJackSpec (spec) where

import Exams.SamuraiJack
import Test.Hspec
import Test.QuickCheck

samurai :: Character
samurai = Character "Samurai" 10 [magicKatana] 0

spec :: Spec
spec = do
  describe "Point 1" $ do
    it "meditating raises health by 50%" $
      health (meditate jack) `shouldBe` 450
    it "health never goes below zero" $
      health (takeDamage 500 jack) `shouldBe` 0
    it "a character can travel in time" $
      presentYear (sendToYear 3000 jack) `shouldBe` 3000

  describe "Point 2" $ do
    it "Aku is evil" $
      aku 200 100 `shouldSatisfy` isEvil
    it "Jack is not evil" $
      jack `shouldNotSatisfy` isEvil
    it "a minion deals one point of damage" $
      damageDealt jack minion `shouldBe` 1
    it "only enemies with a single deadly element are deadly enemies" $
      map name (deadlyEnemies jack [aku 1 10, samurai]) `shouldBe` ["Samurai"]

  describe "Point 3" $ do
    it "concentration 3 makes Jack meditate three times" $
      health (defense (concentration 3) jack) `shouldBe` 1012.5
    it "applyTimes with a negative count does nothing" $
      applyTimes (-1) meditate jack `shouldBe` jack
    it "the portal to the future creates a new Aku 2800 years later" $
      presentYear (defense (futurePortalFrom 200) (aku 200 50)) `shouldBe` 3000

  describe "Point 4 - fight" $ do
    it "Jack defeats Aku with his magic katana" $
      (\(winner, loser) -> (name winner, health winner, name loser, health loser)) (fight jack (aku 200 1000))
        `shouldBe` ("Jack", 1012.5, "Aku", 0)
    it "if Aku attacks first his minions win and he escapes to the future" $
      (\(winner, loser) -> (name winner, presentYear winner, name loser)) (fight (aku 200 1000) jack)
        `shouldBe` ("Aku", 3000, "Jack")

  describe "Point 5" $
    it "the inference exercise behaves as expected" $
      inferenceExercise (\a b -> (a + b, a * b)) (+ 1) (1 :: Int) [1, 2, 3] `shouldBe` [2, 3, 4 :: Int]

  describe "Properties" $ do
    it "taking damage never leaves negative health" $
      forAll (chooseInt (0, 2000)) $ \damage ->
        health (takeDamage (fromIntegral damage) jack) >= 0
    it "applyTimes n f . applyTimes m f == applyTimes (n + m) f" $
      forAll (chooseInt (0, 50)) $ \n ->
        forAll (chooseInt (0, 50)) $ \m ->
          (applyTimes n (+ 1) . applyTimes m (+ 1)) (0 :: Int) === applyTimes (n + m) (+ 1) 0
