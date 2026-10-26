-- | Practice exam "Huber" (first statement, @huber/solucion.hs@): comparing
-- trips by bus against trips in a "Huber" car between city locations.
--
-- The catalogues of rides and bus lines are passed as arguments instead of
-- being read from global constants, so every function is a pure function of
-- its inputs and can be tested with any data set.
module Exams.Huber.BusRoutes
  ( -- * Model
    Location (..)
  , Person (..)
  , BusLine (..)
  , Ride (..)
    -- * Generic list helpers
  , stopsBetween
  , areOrdered
    -- * Point 1
  , connects
  , huberFare
    -- * Point 2
  , servesTrip
  , canGoByBus
    -- * Point 3
  , busFare
  , cheaperThanHuber
    -- * Point 4
  , distanceBetween
    -- * Sample data
  , huberto
  , leto
  , busLines
  , rides
  , locations
  , salgueroAndLavalle
  , salgueroAndPotosi
  , bulnesAndPotosi
  , bulnes1200
  ) where

import Control.Monad (mfilter)
import Data.List (elemIndex, find)

-- ---------------------------------------------------------------------------
-- Model
-- ---------------------------------------------------------------------------

-- | A place is either a street corner or a street with a door number.
data Location
  = Intersection String String
  | StreetNumber String Int
  deriving (Show, Eq)

-- | Modelled in the statement but not used by any of the solved points.
data Person = Person
  { age :: Int
  , freeTime :: Int
  , cashOnHand :: Double
  }
  deriving (Show, Eq)

data BusLine = BusLine
  { lineNumber :: Int
  , stops :: [Location]
  }
  deriving (Show, Eq)

-- | A ride offered by a Huber car.
data Ride = Ride
  { from :: Location
  , to :: Location
  , fare :: Int
  }
  deriving (Show, Eq)

-- ---------------------------------------------------------------------------
-- Generic list helpers
-- ---------------------------------------------------------------------------

-- | Number of steps from the first element to the second one, if both are
-- in the list and the first one does not come after the second one.
stopsBetween :: Eq a => a -> a -> [a] -> Maybe Int
stopsBetween start end xs =
  case (elemIndex start xs, elemIndex end xs) of
    (Just i, Just j) | i <= j -> Just (j - i)
    _ -> Nothing

-- | True only when both elements appear and the first one comes strictly
-- before the second one.
areOrdered :: Eq a => a -> a -> [a] -> Bool
areOrdered start end = maybe False (> 0) . stopsBetween start end

-- ---------------------------------------------------------------------------
-- Point 1: price of a Huber ride (zero when there is none)
-- ---------------------------------------------------------------------------

connects :: Location -> Location -> Ride -> Bool
connects start end ride = from ride == start && to ride == end

huberFare :: [Ride] -> Location -> Location -> Int
huberFare available start end = maybe 0 fare (find (connects start end) available)

-- ---------------------------------------------------------------------------
-- Point 2: is there a bus line that goes from one place to the other?
-- ---------------------------------------------------------------------------

servesTrip :: Location -> Location -> BusLine -> Bool
servesTrip start end = areOrdered start end . stops

canGoByBus :: [BusLine] -> Location -> Location -> Bool
canGoByBus catalogue start end = any (servesTrip start end) catalogue

-- ---------------------------------------------------------------------------
-- Point 3: bus lines cheaper than the Huber ride
-- ---------------------------------------------------------------------------

-- | Two per stop, only for lines that actually serve the trip.
busFare :: Location -> Location -> BusLine -> Maybe Int
busFare start end = fmap (2 *) . mfilter (> 0) . stopsBetween start end . stops

cheaperThanHuber :: [Ride] -> [BusLine] -> Location -> Location -> [Int]
cheaperThanHuber available catalogue start end =
  map lineNumber . filter (maybe False (< limit) . busFare start end) $ catalogue
  where
    limit :: Int
    limit = huberFare available start end

-- ---------------------------------------------------------------------------
-- Point 4: distance between two locations of a list (4 blocks per step)
-- ---------------------------------------------------------------------------

distanceBetween :: Location -> Location -> [Location] -> Maybe Int
distanceBetween start end = fmap (4 *) . stopsBetween start end

-- ---------------------------------------------------------------------------
-- Sample data
-- ---------------------------------------------------------------------------

huberto, leto :: Person
huberto = Person 42 10 10.2
leto = Person 32 20 20

salgueroAndLavalle, salgueroAndPotosi, bulnesAndPotosi, bulnes1200 :: Location
salgueroAndLavalle = Intersection "Salguero" "Lavalle"
salgueroAndPotosi = Intersection "Salguero" "Potosi"
bulnesAndPotosi = Intersection "Bulnes" "Potosi"
bulnes1200 = StreetNumber "Bulnes" 1200

busLines :: [BusLine]
busLines =
  [ BusLine 14 [salgueroAndLavalle, salgueroAndPotosi, bulnesAndPotosi, bulnes1200]
  , BusLine 168 (map (StreetNumber "Corrientes") [100, 300, 500, 700])
  ]

rides :: [Ride]
rides =
  [ Ride salgueroAndLavalle bulnesAndPotosi 5
  , Ride (StreetNumber "Av Corrientes" 700) (Intersection "Sarmiento" "Gallo") 10
  , Ride salgueroAndPotosi (StreetNumber "Ecuador" 600) 20
  , Ride (StreetNumber "Corrientes" 300) (StreetNumber "Corrientes" 700) 5
  , Ride salgueroAndLavalle bulnes1200 5
  ]

locations :: [Location]
locations =
  [ salgueroAndLavalle
  , salgueroAndPotosi
  , bulnesAndPotosi
  , Intersection "Sarmiento" "Gallo"
  , StreetNumber "Ecuador" 600
  ]
    ++ map (StreetNumber "Corrientes") [100, 300, 500, 700]
    ++ [bulnes1200]
