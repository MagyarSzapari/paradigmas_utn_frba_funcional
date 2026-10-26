module Exams.PizzaConmigoSpec (spec) where

import Exams.PizzaConmigo
import Test.Hspec
import Test.QuickCheck

genPizza :: Gen Pizza
genPizza =
  Pizza
    <$> sublistOf ["sauce", "mozzarella", "ham", "olives", "onion", palmHeart]
    <*> chooseInt (4, 10)
    <*> (fromIntegral <$> chooseInt (100, 900))

-- | Pizzerias are functions (no 'Show'), so QuickCheck generates their names.
genPizzeriaName :: Gen String
genPizzeriaName = elements ["palm heart", "summary", "fishy", "jauja"]

pizzeriaNamed :: String -> Pizzeria
pizzeriaNamed "palm heart" = palmHeartPizzeria
pizzeriaNamed "summary" = summaryPizzeria
pizzeriaNamed "fishy" = fishyPizzeria
pizzeriaNamed _ = jaujaPizzeria

sixIngredients :: Pizza
sixIngredients = Pizza ["sauce", "mozzarella", "ham", "olives", "onion", "egg"] 8 480

spec :: Spec
spec = do
  describe "Satisfaction and price" $ do
    it "a large mozzarella satisfies 80 per ingredient" $
      satisfaction largeMozzarella `shouldBe` 240
    it "satisfaction is halved from 500 calories on" $
      satisfaction largeMozzarella {calories = 600} `shouldBe` 120
    it "palm hearts satisfy nobody" $
      satisfaction (addIngredient palmHeart largeMozzarella) `shouldBe` 0
    it "price is 120 per ingredient and slice" $
      price largeMozzarella `shouldBe` 2880

  describe "Point 4" $ do
    it "a new ingredient adds twice its letters in calories (the original multiplied the calories)" $
      addIngredient "ham" largeMozzarella `shouldBe` largeMozzarella {ingredients = "ham" : ingredients largeMozzarella, calories = 356}
    it "a repeated ingredient changes nothing" $
      addIngredient "sauce" largeMozzarella `shouldBe` largeMozzarella
    it "enlarging adds two slices up to ten" $
      map (slices . enlarge . (\n -> Pizza [] n 0)) [6, 9, 10] `shouldBe` [8, 10, 10]
    it "mixing keeps the second pizza and adds half the calories of the first one (the original divided)" $
      mix basicAnchovy largeMozzarella
        `shouldBe` Pizza ["sauce", "mozzarella", "oregano", "anchovies"] 8 485

  describe "Points 5 and 6" $ do
    it "order satisfaction is the sum of each pizza" $
      orderSatisfaction [largeMozzarella, basicAnchovy] `shouldBe` 400
    it "Los Hijos de Pato ruin every pizza" $
      orderSatisfaction (palmHeartPizzeria [largeMozzarella, basicAnchovy]) `shouldBe` 0
    it "El Resumen mixes consecutive pizzas" $
      length (summaryPizzeria [largeMozzarella, basicAnchovy, sixIngredients]) `shouldBe` 2
    it "El Resumen keeps a single pizza" $
      summaryPizzeria [largeMozzarella] `shouldBe` [largeMozzarella]
    it "El Resumen of an empty order is empty" $
      summaryPizzeria [] `shouldBe` []
    it "Pescadito adds anchovies to every pizza" $
      map (elem "anchovies" . ingredients) (fishyPizzeria [largeMozzarella, sixIngredients]) `shouldBe` [True, True]
    it "La Jauja drops deplorable pizzas and enlarges the rest" $
      jaujaPizzeria [largeMozzarella, sixIngredients] `shouldBe` [sixIngredients {slices = 10}]

  describe "Points 7 and 9" $ do
    it "only Pescadito is worthy of Corrientes for a large mozzarella" $
      map (orderSatisfaction . ($ [largeMozzarella])) (worthyOfCorrientes [largeMozzarella] [palmHeartPizzeria, fishyPizzeria, id])
        `shouldBe` [320]
    it "Pescadito is the best pizzeria for a large mozzarella" $
      fmap ($ [largeMozzarella]) (bestPizzeria [largeMozzarella] [palmHeartPizzeria, fishyPizzeria, id])
        `shouldBe` Just (fishyPizzeria [largeMozzarella])
    it "there is no best pizzeria among none" $
      fmap ($ [largeMozzarella]) (bestPizzeria [largeMozzarella] []) `shouldBe` Nothing
    it "the favourite pizzeria of none is the identity (the original crashed)" $
      favouritePizzeria [] [largeMozzarella] `shouldBe` [largeMozzarella]
    it "the favourite pizzeria composes all of them" $
      map slices (favouritePizzeria [map enlarge, map enlarge] [largeMozzarella {slices = 4}]) `shouldBe` [8]

  describe "Properties" $ do
    it "adding an ingredient is idempotent" $
      forAll genPizza $ \pizza -> addIngredient "ham" (addIngredient "ham" pizza) === addIngredient "ham" pizza
    it "enlarging never goes above ten slices" $
      forAll genPizza $ \pizza -> slices (enlarge pizza) <= 10
    it "a mix keeps every ingredient of both pizzas" $
      forAll genPizza $ \first ->
        forAll genPizza $ \second ->
          all (`elem` ingredients (mix first second)) (ingredients first ++ ingredients second)
    it "the favourite pizzeria of a concatenation is the composition" $
      forAll (listOf genPizzeriaName) $ \xs ->
        forAll (listOf genPizzeriaName) $ \ys ->
          forAll (listOf genPizza) $ \order ->
            favouritePizzeria (map pizzeriaNamed (xs ++ ys)) order
              === (favouritePizzeria (map pizzeriaNamed xs) . favouritePizzeria (map pizzeriaNamed ys)) order
