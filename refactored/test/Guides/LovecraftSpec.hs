module Guides.LovecraftSpec (spec) where

import Guides.Lovecraft
import Test.Hspec
import Test.QuickCheck

madMan :: Investigator
madMan = Investigator "Mad" 0 [] []

spec :: Spec
spec = do
  describe "goMad" $ do
    it "subtracts sanity" $ sanity (goMad 3 pepe) `shouldBe` 7
    it "never goes below zero" $ sanity (goMad 5 luis) `shouldBe` 0
    it "sanity is never negative" $
      property $ \n -> sanity (goMad n pepe) >= 0

  describe "findItem" $ do
    let found = findItem necronomicon pepe
    it "costs the item's value in sanity" $ sanity found `shouldBe` 7
    it "adds the item" $ items found `shouldBe` [necronomicon, cursedDagger]

  describe "groupHasItem" $ do
    it "someone has the necronomicon" $ groupHasItem necronomicon [pepe, luis] `shouldBe` True
    it "nobody has it" $ groupHasItem necronomicon [pepe] `shouldBe` False

  describe "potential and leader" $ do
    it "experience is 1 + 3 per event" $ experience pepe `shouldBe` 4
    it "potential = sanity * experience + best item" $ potential pepe `shouldBe` 43
    it "insane investigators have no potential" $ potential madMan `shouldBe` 0
    it "no items no longer crashes" $
      potential (Investigator "Bare" 2 [] []) `shouldBe` 2
    it "the leader is the one with the highest potential" $
      fmap investigatorName (leader [luis, pepe]) `shouldBe` Just "Pepe"
    it "an empty group has no leader" $ leader [] `shouldBe` Nothing

  describe "deltas" $ do
    it "totalSanityDelta" $ totalSanityDelta 5 [pepe, luis] `shouldBe` 7
    it "firstSaneMemberDelta: the first sane member replaces the mad one" $
      firstSaneMemberDelta [madMan, pepe] `shouldBe` 43
    it "firstSaneMemberDelta is 0 when the first member is already sane" $
      firstSaneMemberDelta [pepe, madMan] `shouldBe` 0
    it "firstSaneMemberDelta of an empty group is 0" $ firstSaneMemberDelta [] `shouldBe` 0
