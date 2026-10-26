module Exams.Huber.DriversSpec (spec) where

import Exams.Huber.Drivers
import Test.Hspec
import Test.QuickCheck

tripWith :: Client -> Int -> Trip
tripWith = Trip (Date 2 5 2017)

olivosClient :: Client
olivosClient = Client "Ana" "Olivos"

genTrip :: Gen Trip
genTrip = tripWith <$> elements [lucas, olivosClient] <*> chooseInt (0, 500)

spec :: Spec
spec = do
  describe "Conditions" $ do
    it "Dani takes trips of clients who do not live in Olivos" $
      dani `shouldSatisfy` canTake (tripWith lucas 50)
    it "Dani rejects clients from Olivos" $
      dani `shouldNotSatisfy` canTake (tripWith olivosClient 50)
    it "expensive trips cost more than 200" $
      map (expensiveTrips . tripWith lucas) [200, 201] `shouldBe` [False, True]
    it "long client names have more than n letters" $
      map (longClientName 4 . flip tripWith 0) [lucas, olivosClient] `shouldBe` [True, False]

  describe "Settlement" $ do
    it "Dani earned 150" $
      settlement dani `shouldBe` 150
    it "Ale earned nothing yet" $
      settlement ale `shouldBe` 0

  describe "assignTrip" $ do
    it "the least busy driver who accepts the trip makes it" $
      fmap (\driver -> (driverName driver, tripCount driver)) (assignTrip (tripWith lucas 80) [dani, ale])
        `shouldBe` Just ("Alejandra", 1)
    it "returns Nothing when nobody accepts the trip (the original crashed)" $
      fmap driverName (assignTrip (tripWith olivosClient 80) [dani]) `shouldBe` Nothing

  describe "Lazy evaluation with Nito's infinite trips" $ do
    it "Nito can decide whether he takes a trip" $
      nito `shouldSatisfy` canTake (tripWith lucas 50)
    it "we can look at the first trips of Nito" $
      map cost (take 3 (trips nito)) `shouldBe` [50, 50, 50]
    it "showing Nito terminates" $
      length (show nito) `shouldSatisfy` (> 0)

  describe "gongNeng" $
    it "works on an infinite list because it only needs the first match" $
      gongNeng 0 even (* 3) [1 :: Int ..] `shouldBe` (6 :: Int)

  describe "Properties" $ do
    it "an assigned trip raises the total settlement by exactly its cost" $
      forAll genTrip $ \trip ->
        case assignTrip trip [dani, ale] of
          Nothing -> property False
          Just driver ->
            settlement driver === cost trip + settlement (head (filter ((== driverName driver) . driverName) [dani, ale]))
    it "whoever gets the trip accepts it" $
      forAll genTrip $ \trip ->
        maybe True (canTake trip) (assignTrip trip [dani, ale])
