module Guides.HuberSpec (spec) where

import Guides.Huber
import Test.Hspec

salgueroLavalle, salgueroPotosi, bulnesPotosi, bulnes1200 :: Location
salgueroLavalle = Intersection "Salguero" "Lavalle"
salgueroPotosi = Intersection "Salguero" "Potosi"
bulnesPotosi = Intersection "Bulnes" "Potosi"
bulnes1200 = Address "Bulnes" 1200

spec :: Spec
spec = do
  describe "sample data" $ do
    it "huberto is 42" $ age huberto `shouldBe` 42
    it "leto has 20 of free time" $ freeTime leto `shouldBe` 20
    it "there are ten locations" $ length locations `shouldBe` 10

  describe "areInOrder" $ do
    it "true when the first appears before the second" $
      areInOrder salgueroLavalle bulnesPotosi locations `shouldBe` True
    it "false when the order is reversed" $
      areInOrder bulnesPotosi salgueroLavalle locations `shouldBe` False
    it "false when the first is missing (the original said True)" $
      areInOrder (Address "Nowhere" 1) bulnesPotosi locations `shouldBe` False
    it "false when the second is missing" $
      areInOrder salgueroLavalle (Address "Nowhere" 1) locations `shouldBe` False

  describe "huberFare" $ do
    it "known ride" $ huberFare salgueroLavalle bulnesPotosi `shouldBe` 5
    it "unknown ride costs 0" $ huberFare bulnesPotosi salgueroLavalle `shouldBe` 0

  describe "canGoByBus" $ do
    it "line 14 goes from Salguero y Lavalle to Bulnes 1200" $
      canGoByBus salgueroLavalle bulnes1200 `shouldBe` True
    it "no line goes backwards" $ canGoByBus bulnes1200 salgueroLavalle `shouldBe` False

  describe "stops and fares" $ do
    let line14 = head busLines
    it "stopsBetween counts the destination but not the origin" $
      stopsBetween salgueroLavalle bulnesPotosi (stops line14) `shouldBe` 2
    it "stopsBetween is 0 when the origin is missing (the original crashed)" $
      stopsBetween (Address "Nowhere" 1) bulnesPotosi (stops line14) `shouldBe` 0
    it "busFare is 2 per stop" $ busFare salgueroLavalle bulnes1200 line14 `shouldBe` 6
    it "distanceBetween is 4 per stop" $
      distanceBetween salgueroPotosi bulnes1200 (stops line14) `shouldBe` 8

  describe "cheaperLinesThanHuber" $ do
    it "Corrientes 300 to 700: line 168 costs 4 < 5" $
      cheaperLinesThanHuber (Address "Corrientes" 300) (Address "Corrientes" 700)
        `shouldBe` [168]
    it "Salguero y Lavalle to Bulnes 1200: line 14 costs 6, not < 5" $
      cheaperLinesThanHuber salgueroLavalle bulnes1200 `shouldBe` []
