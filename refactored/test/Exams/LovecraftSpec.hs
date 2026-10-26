module Exams.LovecraftSpec (spec) where

import Exams.Lovecraft
import Test.Hspec
import Test.QuickCheck

insaneLuis :: Investigator
insaneLuis = luis {sanity = 0}

genInvestigator :: Gen Investigator
genInvestigator =
  Investigator
    <$> elements ["Pepe", "Luis", "Ana"]
    <*> chooseInt (0, 50)
    <*> sublistOf [necronomicon, cursedDagger]
    <*> sublistOf [description awakeningOfAnAncientOne, description innsmouthRitual]

spec :: Spec
spec = do
  describe "Point 1" $ do
    it "going mad lowers sanity" $
      sanity (goMad 3 pepe) `shouldBe` 7
    it "sanity never goes below zero" $
      sanity (goMad 100 pepe) `shouldBe` 0
    it "finding an item drives you mad and keeps the item" $
      findItem necronomicon pepe `shouldBe` pepe {sanity = 7, items = [necronomicon, cursedDagger]}

  describe "Point 2" $ do
    it "the group has the Necronomicon thanks to Luis" $
      [pepe, luis] `shouldSatisfy` groupHasItem "Necronomicon"
    it "Pepe alone does not have it" $
      [pepe] `shouldNotSatisfy` groupHasItem "Necronomicon"

  describe "Point 3" $ do
    it "Pepe's potential is 10 * (1 + 3 * 1) + 3" $
      potential pepe `shouldBe` 43
    it "an insane investigator has no potential" $
      potential insaneLuis `shouldBe` 0
    it "an investigator without items does not crash (the original did)" $
      potential (Investigator "Ana" 5 [] []) `shouldBe` 5
    it "Pepe leads the group" $
      fmap name (leader [pepe, luis]) `shouldBe` Just "Pepe"
    it "an empty group has no leader" $
      fmap name (leader []) `shouldBe` Nothing

  describe "Point 4" $ do
    it "total sanity delta clamps at zero" $
      totalSanityDelta 5 [pepe, luis] `shouldBe` 7
    it "first member potential delta skips the insane (the original filtered the wrong ones)" $
      firstMemberPotentialDelta [insaneLuis, pepe] `shouldBe` Just 43
    it "terminates with an infinite group thanks to lazy evaluation" $
      firstMemberPotentialDelta (insaneLuis : cycle [pepe, luis]) `shouldBe` Just 43
    it "is Nothing when nobody is sane" $
      firstMemberPotentialDelta [insaneLuis] `shouldBe` Nothing

  describe "Point 5" $ do
    it "the group avoids the awakening thanks to the Necronomicon" $
      map (\i -> (sanity i, take 1 (avoidedEvents i))) (face awakeningOfAnAncientOne [pepe, luis])
        `shouldBe` [(9, ["Awakening of an Ancient One"]), (1, ["Awakening of an Ancient One"])]
    it "without the Necronomicon the awakening takes the first member away" $
      face awakeningOfAnAncientOne [pepe] `shouldBe` []
    it "the ritual cannot be avoided by this group" $
      map sanity (face innsmouthRitual [pepe, luis]) `shouldBe` [4, 0]
    it "in the ritual the first member finds the cursed dagger" $
      map items (take 1 (face innsmouthRitual [pepe, luis])) `shouldBe` [[cursedDagger, cursedDagger]]
    it "terror is the total sanity lost" $
      map (terror [pepe, luis]) [awakeningOfAnAncientOne, innsmouthRitual] `shouldBe` [2, 8]
    it "the ritual is the scariest event" $
      fmap description (scariestEvent [pepe, luis] [awakeningOfAnAncientOne, innsmouthRitual])
        `shouldBe` Just "Ritual in Innsmouth"

  describe "Properties" $ do
    it "going mad never leaves a negative sanity" $
      forAll genInvestigator $ \investigator ->
        forAll (chooseInt (0, 100)) $ \points -> sanity (goMad points investigator) >= 0
    it "potential is never negative" $
      forAll genInvestigator $ \investigator -> potential investigator >= 0
    it "facing an event never raises the total sanity" $
      forAll (listOf genInvestigator) $ \group ->
        forAll (elements [awakeningOfAnAncientOne, innsmouthRitual]) $ \event ->
          sum (map sanity (face event group)) <= sum (map sanity group)
