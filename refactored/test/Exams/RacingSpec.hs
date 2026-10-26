module Exams.RacingSpec (spec) where

import Exams.Racing
import Test.Hspec
import Test.QuickCheck

-- | Floating point results are compared with a tolerance.
closeTo :: Double -> Double -> Bool
closeTo expected actual = abs (expected - actual) < 1e-9

worn :: Car
worn = ferrari {wear = Wear 70 50}

genCar :: Gen Car
genCar = do
  wheelWear <- fromIntegral <$> chooseInt (0, 100)
  chassisWear <- fromIntegral <$> chooseInt (0, 100)
  speed <- fromIntegral <$> chooseInt (20, 90)
  pure (Car "Test" "Car" (Wear wheelWear chassisWear) speed 0)

spec :: Spec
spec = do
  describe "Points 2 and 3" $ do
    it "a brand new Ferrari is in good shape" $
      ferrari `shouldSatisfy` isInGoodShape
    it "a car with 50 of chassis wear is not in good shape" $
      worn `shouldNotSatisfy` isInGoodShape
    it "a car with more than 80 of wear is wrecked" $
      ferrari {wear = Wear 81 0} `shouldSatisfy` isWrecked
    it "repairing leaves new wheels and 15% of the chassis wear" $
      wear (repair fiat) `shouldSatisfy` (\w -> wheels w == 0 && closeTo 4.95 (chassis w))

  describe "Point 4: sections" $ do
    it "a dangerous curve wears the wheels 3 * 300 / 60" $
      wheels (wear (dangerousCurve ferrari)) `shouldBe` 15
    it "a curve is driven at half the top speed" $
      raceTime (dangerousCurve ferrari) `shouldSatisfy` closeTo (300 / 32.5)
    it "a classic straight wears the chassis 750 / 100" $
      chassis (wear (classicStraight ferrari)) `shouldBe` 7.5
    it "a straight is driven at top speed" $
      raceTime (classicStraight ferrari) `shouldSatisfy` closeTo (750 / 65)
    it "a wet section adds half of its own time" $
      raceTime (wet shortStraight ferrari) `shouldSatisfy` closeTo (1.5 * 280 / 65)
    it "gravel doubles the effect of the section" $
      gravel shortStraight ferrari `shouldBe` (shortStraight . shortStraight) ferrari
    it "an obstruction adds twice its length of wheel wear" $
      wheels (wear (obstructed 2 dangerousCurve ferrari)) `shouldBe` 19
    it "a car in good shape just goes through the pit stop section" $
      pitStop shortStraight ferrari `shouldBe` shortStraight ferrari
    it "a car in bad shape is repaired and loses 10 seconds at the pit stop" $
      pitStop shortStraight worn `shouldBe` (shortStraight . addTime 10 . repair) worn

  describe "Point 5" $
    it "a wrecked car does not go through sections" $
      passThrough classicStraight (ferrari {wear = Wear 90 0}) `shouldBe` ferrari {wear = Wear 90 0}

  describe "Points 6 and 7" $ do
    it "a lap goes through every section" $
      map raceTime (completeLap [straight 650, straight 650] [ferrari]) `shouldBe` [20]
    it "a race returns the state after each lap" $
      map (map raceTime) (playRace (Race [straight 650] 3) [ferrari]) `shouldBe` [[10], [20], [30]]
    it "wrecked cars leave the race" $
      playRace (Race [curve 1 100] 2) [ferrari] `shouldBe` [[], []]
    it "the Tour de Buenos Aires has 20 laps" $
      length (playRace tourBuenosAires [ferrari, lamborghini, fiat]) `shouldBe` 20

  describe "Point 8" $
    it "myPrecious adds the functions of the long enough entries" $
      myPrecious [(1, (+ 1)), (2, (* 10))] (\(k, _) -> replicate (3 * k) 'x', 5 :: Int) `shouldBe` (50 :: Int)

  describe "Properties" $ do
    it "no section of the super track makes time go backwards" $
      forAll genCar $ \car -> all ((>= raceTime car) . raceTime . ($ car)) superTrack
    it "a race never has more cars than it started with" $
      forAll (listOf genCar) $ \cars ->
        all ((<= length cars) . length) (playRace (Race superTrack 3) cars)
    it "a race has exactly as many states as laps" $
      forAll (chooseInt (0, 5)) $ \n -> length (playRace (Race superTrack n) [ferrari]) === n
