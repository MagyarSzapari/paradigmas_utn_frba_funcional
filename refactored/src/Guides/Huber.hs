-- | Practice assignment "Huber": ride hailing vs. buses.
module Guides.Huber
  ( Street
  , Location (..)
  , Person (..)
  , huberto
  , leto
  , BusLine (..)
  , busLines
  , Ride (..)
  , rides
  , locations
  , areInOrder
  , huberFare
  , canGoByBus
  , cheaperLinesThanHuber
  , busFare
  , stopsBetween
  , distanceBetween
  ) where

import Data.List (dropWhileEnd, elemIndex, find)

type Street = String

-- | A location is either a street corner or a street number.
data Location
  = Intersection Street Street
  | Address Street Int
  deriving (Show, Eq)

data Person = Person
  { age :: Int
  , freeTime :: Int
  , cash :: Double
  }
  deriving (Show, Eq)

huberto :: Person
huberto = Person 42 10 10.2

leto :: Person
leto = Person 32 20 20

-- | A bus line and its ordered stops (the original @(Number, [Ubicacion])@).
data BusLine = BusLine
  { lineNumber :: Int
  , stops :: [Location]
  }
  deriving (Show, Eq)

busLines :: [BusLine]
busLines =
  [ BusLine 14
      [ Intersection "Salguero" "Lavalle"
      , Intersection "Salguero" "Potosi"
      , Intersection "Bulnes" "Potosi"
      , Address "Bulnes" 1200
      ]
  , BusLine 168
      [ Address "Corrientes" 100
      , Address "Corrientes" 300
      , Address "Corrientes" 500
      , Address "Corrientes" 700
      ]
  ]

-- | A known Huber ride and its fare.
data Ride = Ride
  { origin :: Location
  , destination :: Location
  , fare :: Int
  }
  deriving (Show, Eq)

rides :: [Ride]
rides =
  [ Ride (Intersection "Salguero" "Lavalle") (Intersection "Bulnes" "Potosi") 5
  , Ride (Address "Av Corrientes" 700) (Intersection "Sarmiento" "Gallo") 10
  , Ride (Intersection "Salguero" "Potosi") (Address "Ecuador" 600) 20
  , Ride (Address "Corrientes" 300) (Address "Corrientes" 700) 5
  , Ride (Intersection "Salguero" "Lavalle") (Address "Bulnes" 1200) 5
  ]

locations :: [Location]
locations =
  [ Intersection "Salguero" "Lavalle"
  , Intersection "Salguero" "Potosi"
  , Intersection "Bulnes" "Potosi"
  , Intersection "Sarmiento" "Gallo"
  , Address "Ecuador" 600
  , Address "Corrientes" 100
  , Address "Corrientes" 300
  , Address "Corrientes" 500
  , Address "Corrientes" 700
  , Address "Bulnes" 1200
  ]

-- | Given helper: does @from@ appear strictly before @to@ in the list?
-- False when either is missing.
--
-- The original returned index 0 for "missing", so a missing @from@ with a
-- present @to@ wrongly answered True. 'elemIndex' returns 'Maybe Int' and
-- the 'Applicative' instance of 'Maybe' propagates the absence.
areInOrder :: Location -> Location -> [Location] -> Bool
areInOrder from to xs =
  ((<) <$> elemIndex from xs <*> elemIndex to xs) == Just True

-- | 1: fare of the first known ride between both locations, 0 if none.
-- 'find' replaces the original @any@ + @head . filter@ double traversal.
huberFare :: Location -> Location -> Int
huberFare from to = maybe 0 fare (find connects rides)
  where
    connects ride = origin ride == from && destination ride == to

-- | 2.
canGoByBus :: Location -> Location -> Bool
canGoByBus from to = any (servesTrip from to) busLines

servesTrip :: Location -> Location -> BusLine -> Bool
servesTrip from to = areInOrder from to . stops

-- | 3: lines that make the trip cheaper than Huber.
cheaperLinesThanHuber :: Location -> Location -> [Int]
cheaperLinesThanHuber from to = map lineNumber (filter isCheaper busLines)
  where
    limit = huberFare from to
    isCheaper line = servesTrip from to line && busFare from to line < limit

-- | 3: 2 per stop travelled.
busFare :: Location -> Location -> BusLine -> Int
busFare from to = (2 *) . stopsBetween from to . stops

-- | Stops travelled from @from@ (excluded) up to @to@ (included).
-- Total: 0 when @from@ is missing (the original crashed on @tail []@).
-- 'dropWhileEnd' comes from "Data.List" (the original re-implemented it).
stopsBetween :: Location -> Location -> [Location] -> Int
stopsBetween from to =
  length . drop 1 . dropWhile (/= from) . dropWhileEnd (/= to)

-- | 4.a: 4 distance units per stop.
distanceBetween :: Location -> Location -> [Location] -> Int
distanceBetween from to = (4 *) . stopsBetween from to
