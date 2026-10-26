-- | Practice exam "Pizza Conmigo": pizzas, orders and pizzerias.
--
-- A pizzeria is modelled as a transformation of a whole order
-- (@Order -> Order@), so pizzerias can be compared, filtered and even
-- composed into a single "favourite" pizzeria.
--
-- Consolidates @pizza-conmigo/solucion.hs@ and @pizza-conmigo/intento.hs@.
module Exams.PizzaConmigo
  ( -- * Model (point 1)
    Ingredient
  , Pizza (..)
  , Order
  , Pizzeria
  , largeMozzarella
  , basicAnchovy
    -- * Points 2 and 3
  , palmHeart
  , satisfaction
  , price
    -- * Point 4
  , addIngredient
  , enlarge
  , mix
    -- * Point 5
  , orderSatisfaction
    -- * Point 6: pizzerias
  , palmHeartPizzeria
  , summaryPizzeria
  , specialPizzeria
  , fishyPizzeria
  , gourmetPizzeria
  , jaujaPizzeria
    -- * Point 7
  , worthyOfCorrientes
  , bestPizzeria
    -- * Point 9
  , favouritePizzeria
  ) where

import Data.List (maximumBy, union)
import Data.Ord (comparing)

-- ---------------------------------------------------------------------------
-- Model
-- ---------------------------------------------------------------------------

type Ingredient = String

data Pizza = Pizza
  { ingredients :: [Ingredient]
  , slices :: Int
  , calories :: Double
  }
  deriving (Show, Eq)

type Order = [Pizza]

type Pizzeria = Order -> Order

largeMozzarella :: Pizza
largeMozzarella = Pizza ["sauce", "mozzarella", "oregano"] 8 350

basicAnchovy :: Pizza
basicAnchovy = Pizza ["sauce", "anchovies"] 8 270

-- ---------------------------------------------------------------------------
-- Points 2 and 3
-- ---------------------------------------------------------------------------

palmHeart :: Ingredient
palmHeart = "palm heart"

-- | Nobody is satisfied by palm hearts; otherwise 80 points per ingredient,
-- halved for pizzas of 500 calories or more.
satisfaction :: Pizza -> Int
satisfaction pizza
  | palmHeart `elem` ingredients pizza = 0
  | calories pizza < 500 = points
  | otherwise = points `div` 2
  where
    points :: Int
    points = 80 * length (ingredients pizza)

-- | 120 per ingredient and per slice.
price :: Pizza -> Int
price pizza = 120 * length (ingredients pizza) * slices pizza

-- ---------------------------------------------------------------------------
-- Point 4
-- ---------------------------------------------------------------------------

-- | Adds a new ingredient (twice its letters in calories); a repeated
-- ingredient leaves the pizza unchanged.
addIngredient :: Ingredient -> Pizza -> Pizza
addIngredient ingredient pizza
  | ingredient `elem` ingredients pizza = pizza
  | otherwise =
      pizza
        { ingredients = ingredient : ingredients pizza
        , calories = calories pizza + 2 * fromIntegral (length ingredient)
        }

-- | Two more slices, up to a maximum of ten.
enlarge :: Pizza -> Pizza
enlarge pizza = pizza {slices = min 10 (slices pizza + 2)}

-- | The second pizza with the ingredients of both (no repetitions) and
-- half the calories of the first one added to its own.
mix :: Pizza -> Pizza -> Pizza
mix first second =
  second
    { ingredients = ingredients second `union` ingredients first
    , calories = calories second + calories first / 2
    }

-- ---------------------------------------------------------------------------
-- Point 5
-- ---------------------------------------------------------------------------

orderSatisfaction :: Order -> Int
orderSatisfaction = sum . map satisfaction

-- ---------------------------------------------------------------------------
-- Point 6: pizzerias
-- ---------------------------------------------------------------------------

-- | "Los Hijos de Pato": palm hearts on every pizza.
palmHeartPizzeria :: Pizzeria
palmHeartPizzeria = map (addIngredient palmHeart)

-- | "El Resumen": mixes every pizza with the next one; a single pizza is
-- kept as is.
summaryPizzeria :: Pizzeria
summaryPizzeria [pizza] = [pizza]
summaryPizzeria order = zipWith mix order (drop 1 order)

-- | A pizzeria that mixes its favourite pizza into every pizza of the order.
specialPizzeria :: Pizza -> Pizzeria
specialPizzeria favourite = map (mix favourite)

-- | "Pescadito": the special pizzeria whose favourite is the anchovy pizza.
fishyPizzeria :: Pizzeria
fishyPizzeria = specialPizzeria basicAnchovy

-- | Drops the deplorable pizzas (satisfaction not above the exquisiteness
-- level) and enlarges the rest.
gourmetPizzeria :: Int -> Pizzeria
gourmetPizzeria exquisiteness = map enlarge . filter ((> exquisiteness) . satisfaction)

-- | "La Jauja": a gourmet pizzeria of level 399.
jaujaPizzeria :: Pizzeria
jaujaPizzeria = gourmetPizzeria 399

-- ---------------------------------------------------------------------------
-- Point 7
-- ---------------------------------------------------------------------------

-- | Pizzerias that improve the satisfaction of the order.
worthyOfCorrientes :: Order -> [Pizzeria] -> [Pizzeria]
worthyOfCorrientes order = filter improves
  where
    improves :: Pizzeria -> Bool
    improves pizzeria = orderSatisfaction (pizzeria order) > orderSatisfaction order

-- | The pizzeria that satisfies the order the most, if any.
bestPizzeria :: Order -> [Pizzeria] -> Maybe Pizzeria
bestPizzeria _ [] = Nothing
bestPizzeria order pizzerias = Just (maximumBy (comparing (orderSatisfaction . ($ order))) pizzerias)

-- ---------------------------------------------------------------------------
-- Point 9
-- ---------------------------------------------------------------------------

-- | All pizzerias composed into one. @foldr (.) id@ is the identity for an
-- empty list, whereas the original @foldl1 (.)@ crashed.
favouritePizzeria :: [Pizzeria] -> Pizzeria
favouritePizzeria = foldr (.) id
