module Exams.FedExSpec (spec) where

import Exams.FedEx
import Test.Hspec
import Test.QuickCheck

genShipment :: Gen Shipment
genShipment =
  Shipment buenosAires
    <$> elements [cordoba, montevideo]
    <*> (fromIntegral <$> chooseInt (0, 50))
    <*> (fromIntegral <$> chooseInt (0, 5000))
    <*> sublistOf ["technology", "books", "music"]
    <*> pure []

-- | Floating point results are compared with a tolerance.
closeTo :: Double -> Double -> Bool
closeTo expected actual = abs (expected - actual) < 1e-9

spec :: Spec
spec = do
  describe "Category charges" $ do
    it "technology shipments pay 18% more" $
      basePrice (technologyCharge localShipment) `shouldSatisfy` closeTo 259.6
    it "other shipments are not charged" $
      basePrice (technologyCharge internationalShipment) `shouldBe` 1500

  describe "Overweight charge" $ do
    it "charges 80 per kilogram above the limit" $
      basePrice (overweightCharge 3 internationalShipment) `shouldBe` 1500 + 2 * 80
    it "does nothing when the weight is within the limit" $
      basePrice (overweightCharge 3 localShipment) `shouldBe` 220

  describe "Composition of charges" $ do
    it "applies charges from left to right" $
      basePrice (applyCharges [addToPrice 100, overweightCharge 3] internationalShipment)
        `shouldBe` 1500 + 100 + 160
    it "the order matters for percentages" $
      basePrice (applyCharges [technologyCharge, addToPrice 100] localShipment)
        `shouldNotBe` basePrice (applyCharges [addToPrice 100, technologyCharge] localShipment)

  describe "Local and international" $ do
    it "Buenos Aires -> Cordoba is local" $
      localShipment `shouldSatisfy` isLocal
    it "Buenos Aires -> Montevideo is international" $
      internationalShipment `shouldSatisfy` isInternational

  describe "Properties" $ do
    it "no charge ever lowers the base price" $
      forAll genShipment $ \shipment ->
        all
          ((>= basePrice shipment) . basePrice . ($ shipment))
          [technologyCharge, overweightCharge 10, addToPrice 0]
    it "applying no charges is the identity on the price" $
      forAll genShipment $ \shipment -> basePrice (applyCharges [] shipment) === basePrice shipment
