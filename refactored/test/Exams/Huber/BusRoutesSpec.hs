module Exams.Huber.BusRoutesSpec (spec) where

import Exams.Huber.BusRoutes
import Test.Hspec
import Test.QuickCheck

nowhere :: Location
nowhere = Intersection "Nowhere" "Street"

corrientes :: Int -> Location
corrientes = StreetNumber "Corrientes"

spec :: Spec
spec = do
  describe "areOrdered" $ do
    it "is True when the first location comes before the second one" $
      areOrdered salgueroAndLavalle bulnes1200 locations `shouldBe` True
    it "is False when the order is reversed" $
      areOrdered bulnes1200 salgueroAndLavalle locations `shouldBe` False
    it "is False when the first location is missing (regression: the original returned True)" $
      areOrdered nowhere bulnes1200 locations `shouldBe` False
    it "is False when the second location is missing" $
      areOrdered salgueroAndLavalle nowhere locations `shouldBe` False

  describe "Point 1 - huberFare" $ do
    it "is the fare of the first matching ride" $
      huberFare rides salgueroAndLavalle bulnesAndPotosi `shouldBe` 5
    it "is zero when no ride makes that trip" $
      huberFare rides bulnesAndPotosi salgueroAndLavalle `shouldBe` 0

  describe "Point 2 - canGoByBus" $ do
    it "line 14 goes from Salguero & Lavalle to Bulnes 1200" $
      canGoByBus busLines salgueroAndLavalle bulnes1200 `shouldBe` True
    it "no line goes backwards" $
      canGoByBus busLines bulnes1200 salgueroAndLavalle `shouldBe` False

  describe "Point 3 - cheaperThanHuber" $ do
    it "line 14 is cheaper for two stops (4 < 5)" $
      cheaperThanHuber rides busLines salgueroAndLavalle bulnesAndPotosi `shouldBe` [14]
    it "line 14 is not cheaper for three stops (6 >= 5)" $
      cheaperThanHuber rides busLines salgueroAndLavalle bulnes1200 `shouldBe` []
    it "line 168 is cheaper along Corrientes" $
      cheaperThanHuber rides busLines (corrientes 300) (corrientes 700) `shouldBe` [168]

  describe "Point 4 - distanceBetween" $ do
    it "is four blocks per step" $
      distanceBetween salgueroAndLavalle bulnes1200 locations `shouldBe` Just 36
    it "is Nothing for reversed locations (the original crashed with tail [])" $
      distanceBetween bulnes1200 salgueroAndLavalle locations `shouldBe` Nothing

  describe "Properties" $ do
    it "areOrdered x y and areOrdered y x are never both True" $
      forAll (elements locations) $ \x ->
        forAll (elements locations) $ \y ->
          not (areOrdered x y locations && areOrdered y x locations)
    it "stopsBetween agrees with the positions in the list" $
      forAll (chooseInt (0, 20)) $ \i ->
        forAll (chooseInt (i, 20)) $ \j ->
          stopsBetween i j [0 .. 20 :: Int] === Just (j - i)
