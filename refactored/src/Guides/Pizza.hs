-- | Practice assignment "Pizza Conmigo".
module Guides.Pizza
  ( Ingredient
  , Pizza (..)
  , Order
  , Pizzeria
  , largeMozzarella
  , basicAnchovy
  , maxSlices
  , satisfaction
  , price
  , addIngredient
  , enlarge
  , mix
  , orderSatisfaction
  , losHijosDePato
  , elResumen
  , specialPizzeria
  , pescadito
  , gourmet
  , laJauja
  , worthyOfCorrientes
  , bestPizzeria
  , favouritePizzeria
  ) where

import Data.List (union)
import Guides.Utils (maximumOn)

type Ingredient = String

-- | 1.a
data Pizza = Pizza
  { ingredients :: [Ingredient]
  , slices :: Int
  , calories :: Double
  }
  deriving (Show, Eq)

-- | 6: an order is a list of pizzas and a pizzeria transforms orders.
type Order = [Pizza]

type Pizzeria = Order -> Order

-- | 1.b
largeMozzarella :: Pizza
largeMozzarella = Pizza ["salsa", "mozzarella", "orégano"] 8 350

-- | Pizza used by 'pescadito' (6.c).
basicAnchovy :: Pizza
basicAnchovy = Pizza ["salsa", "anchoas"] 8 270

-- | Maximum number of slices a pizza can have.
maxSlices :: Int
maxSlices = 10

-- | 2: 0 with palmito; otherwise 80 per ingredient, halved when the pizza
-- has 500 calories or more.
satisfaction :: Pizza -> Double
satisfaction (Pizza pizzaIngredients _ pizzaCalories)
  | "palmito" `elem` pizzaIngredients = 0
  | pizzaCalories < 500 = base
  | otherwise = base / 2
  where
    base = 80 * fromIntegral (length pizzaIngredients)

-- | 3: 120 per ingredient per slice.
price :: Pizza -> Int
price (Pizza pizzaIngredients pizzaSlices _) = 120 * length pizzaIngredients * pizzaSlices

-- | 4.a: add an ingredient (if missing) and twice its letter count in
-- calories.
--
-- The original computed @2 + calories * letters@ instead of
-- @calories + 2 * letters@.
addIngredient :: Ingredient -> Pizza -> Pizza
addIngredient ingredient pizza
  | ingredient `elem` ingredients pizza = pizza
  | otherwise =
      pizza
        { ingredients = ingredient : ingredients pizza
        , calories = calories pizza + 2 * fromIntegral (length ingredient)
        }

-- | 4.b: two more slices, never above 'maxSlices'.
enlarge :: Pizza -> Pizza
enlarge pizza = pizza {slices = min maxSlices (slices pizza + 2)}

-- | 4.c: the second pizza receives the first one's ingredients (without
-- repetitions) and half of its calories.
--
-- The original computed @first / (2 + second)@ instead of
-- @second + first / 2@.
mix :: Pizza -> Pizza -> Pizza
mix first second =
  second
    { ingredients = ingredients second `union` ingredients first
    , calories = calories second + calories first / 2
    }

-- | 5.
orderSatisfaction :: Order -> Double
orderSatisfaction = sum . map satisfaction

-- | 6.a: palmito on every pizza.
losHijosDePato :: Pizzeria
losHijosDePato = map (addIngredient "palmito")

-- | 6.b: every pizza mixed with the next one. Orders with fewer than two
-- pizzas are returned unchanged (pattern matching instead of @length@).
elResumen :: Pizzeria
elResumen order@(_ : rest@(_ : _)) = zipWith mix order rest
elResumen order = order

-- | 6.c: every pizza is mixed with the house favourite.
specialPizzeria :: Pizza -> Pizzeria
specialPizzeria favourite = map (mix favourite)

-- | 6.c
pescadito :: Pizzeria
pescadito = specialPizzeria basicAnchovy

-- | 6.d: keep pizzas above an exquisiteness level and enlarge them.
gourmet :: Double -> Pizzeria
gourmet level = map enlarge . filter ((> level) . satisfaction)

-- | 6.d
laJauja :: Pizzeria
laJauja = gourmet 399

-- | 7.a: pizzerias that improve the order's satisfaction.
worthyOfCorrientes :: Order -> [Pizzeria] -> [Pizzeria]
worthyOfCorrientes order = filter improves
  where
    improves pizzeria = orderSatisfaction (pizzeria order) > orderSatisfaction order

-- | 7.b: the pizzeria that maximises the order's satisfaction.
bestPizzeria :: Order -> [Pizzeria] -> Maybe Pizzeria
bestPizzeria order = maximumOn (orderSatisfaction . ($ order))

-- | 9: composition of every pizzeria. @foldr (.) id@ is total: the empty
-- list yields the identity instead of the crash of @foldl1 (.)@.
favouritePizzeria :: [Pizzeria] -> Pizzeria
favouritePizzeria = foldr (.) id
