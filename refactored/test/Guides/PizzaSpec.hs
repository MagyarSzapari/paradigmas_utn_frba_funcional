module Guides.PizzaSpec (spec) where

import Data.List (sort)
import Guides.Pizza
import Test.Hspec
import Test.QuickCheck

heavyPizza :: Pizza
heavyPizza = Pizza ["salsa", "jamón", "morrones"] 8 600

palmitoPizza :: Pizza
palmitoPizza = Pizza ["palmito", "salsa"] 8 300

genPizza :: Gen Pizza
genPizza =
  Pizza
    <$> sublistOf ["salsa", "mozzarella", "jamón", "anchoas", "palmito"]
    <*> chooseInt (1, 12)
    <*> (fromIntegral <$> chooseInt (0, 2000))

spec :: Spec
spec = do
  describe "satisfaction" $ do
    it "light pizza: 80 per ingredient" $ satisfaction largeMozzarella `shouldBe` 240
    it "heavy pizza: halved" $ satisfaction heavyPizza `shouldBe` 120
    it "palmito ruins everything" $ satisfaction palmitoPizza `shouldBe` 0

  describe "price" $
    it "120 * ingredients * slices" $ price largeMozzarella `shouldBe` 2880

  describe "addIngredient" $ do
    let withHam = addIngredient "jamón" largeMozzarella
    it "adds the ingredient" $ ingredients withHam `shouldContain` ["jamón"]
    it "adds twice the letters in calories (the original added 2 + cal * letters)" $
      calories withHam `shouldBe` 360
    it "is idempotent" $
      forAll genPizza $ \p ->
        addIngredient "jamón" (addIngredient "jamón" p) == addIngredient "jamón" p

  describe "enlarge" $ do
    it "adds two slices" $ slices (enlarge largeMozzarella) `shouldBe` 10
    it "never exceeds the maximum" $
      forAll genPizza $ \p -> slices (enlarge p) <= max maxSlices (slices p)

  describe "mix" $ do
    let mixed = mix largeMozzarella basicAnchovy
    it "joins ingredients without repetitions" $
      sort (ingredients mixed) `shouldBe` sort ["salsa", "anchoas", "mozzarella", "orégano"]
    it "second calories plus half the first (the original divided)" $
      calories mixed `shouldBe` 270 + 175
    it "keeps the second pizza's size" $ slices mixed `shouldBe` 8

  describe "orders and pizzerias" $ do
    let order = [largeMozzarella, heavyPizza]
    it "orderSatisfaction" $ orderSatisfaction order `shouldBe` 360
    it "losHijosDePato adds palmito to everything" $
      orderSatisfaction (losHijosDePato order) `shouldBe` 0
    it "elResumen mixes each pizza with the next" $
      length (elResumen [largeMozzarella, heavyPizza, basicAnchovy]) `shouldBe` 2
    it "elResumen leaves single orders untouched" $
      elResumen [largeMozzarella] `shouldBe` [largeMozzarella]
    it "elResumen of an empty order is empty" $ elResumen [] `shouldBe` []
    it "pescadito mixes with anchovies" $
      map ingredients (pescadito [heavyPizza]) `shouldSatisfy` all ("anchoas" `elem`)
    it "gourmet filters and enlarges" $
      gourmet 200 order `shouldBe` [enlarge largeMozzarella]
    it "laJauja discards everything at or below 399" $ laJauja order `shouldBe` []
    it "worthyOfCorrientes keeps only improving pizzerias" $
      length (worthyOfCorrientes [palmitoPizza] [losHijosDePato, map (const largeMozzarella)])
        `shouldBe` 1
    it "bestPizzeria picks the most satisfying one" $
      fmap (orderSatisfaction . ($ order)) (bestPizzeria order [losHijosDePato, id])
        `shouldBe` Just 360
    it "bestPizzeria of no pizzerias is Nothing" $
      fmap ($ order) (bestPizzeria order []) `shouldBe` Nothing
    it "favouritePizzeria composes (rightmost first)" $
      favouritePizzeria [map enlarge, losHijosDePato] order
        `shouldBe` map enlarge (losHijosDePato order)
    it "favouritePizzeria of [] is the identity" $
      favouritePizzeria [] order `shouldBe` order
