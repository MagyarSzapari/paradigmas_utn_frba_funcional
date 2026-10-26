-- | Practice exam "Huber" (second statement, @huber/solucion-v2.hs@): drivers,
-- the trips they make and the conditions they impose on the trips they take.
--
-- Each driver stores its acceptance condition as a function (@Trip -> Bool@),
-- which is a direct use of higher-order data: the behaviour lives in the
-- value, not in a chain of @if@s.
module Exams.Huber.Drivers
  ( -- * Model (point 1)
    Date (..)
  , Client (..)
  , Trip (..)
  , TripCondition
  , Driver (..)
    -- * Conditions (point 2)
  , anyTrip
  , expensiveTrips
  , longClientName
  , clientNotLivingIn
    -- * Sample data (point 3)
  , lucas
  , dani
  , ale
    -- * Points 4 to 6
  , canTake
  , settlement
  , tripCount
  , leastBusy
  , addTrip
  , assignTrip
    -- * Point 7: infinite trips
  , nito
  , infiniteTrips
    -- * Point 8: type inference
  , gongNeng
  ) where

import Data.List (minimumBy)
import Data.Ord (comparing)

-- ---------------------------------------------------------------------------
-- Model
-- ---------------------------------------------------------------------------

-- | A named record instead of an anonymous @(Int, Int, Int)@ tuple: the
-- compiler can no longer confuse the day with the month.
data Date = Date
  { day :: Int
  , month :: Int
  , year :: Int
  }
  deriving (Show, Eq)

data Client = Client
  { clientName :: String
  , residence :: String
  }
  deriving (Show, Eq)

data Trip = Trip
  { date :: Date
  , client :: Client
  , cost :: Int
  }
  deriving (Show, Eq)

type TripCondition = Trip -> Bool

data Driver = Driver
  { driverName :: String
  , mileage :: Int
  , trips :: [Trip]
  , condition :: TripCondition
  }

-- | The condition is a function, so it is left out. Only the first trips are
-- shown, which keeps @show nito@ finite even though Nito has infinite trips.
instance Show Driver where
  showsPrec d driver =
    showParen (d > 10) $
      showString "Driver "
        . showsPrec 11 (driverName driver)
        . showChar ' '
        . showsPrec 11 (mileage driver)
        . showChar ' '
        . showsPrec 11 (take 3 (trips driver))

-- ---------------------------------------------------------------------------
-- Conditions
-- ---------------------------------------------------------------------------

anyTrip :: TripCondition
anyTrip = const True

expensiveTrips :: TripCondition
expensiveTrips = (> 200) . cost

longClientName :: Int -> TripCondition
longClientName letters = (> letters) . length . clientName . client

clientNotLivingIn :: String -> TripCondition
clientNotLivingIn place = (/= place) . residence . client

-- ---------------------------------------------------------------------------
-- Sample data
-- ---------------------------------------------------------------------------

lucas :: Client
lucas = Client "Lucas" "Victoria"

dani :: Driver
dani = Driver "Daniel" 23500 [Trip (Date 20 4 2017) lucas 150] (clientNotLivingIn "Olivos")

ale :: Driver
ale = Driver "Alejandra" 180000 [] anyTrip

-- ---------------------------------------------------------------------------
-- Points 4 to 6
-- ---------------------------------------------------------------------------

canTake :: Trip -> Driver -> Bool
canTake trip driver = condition driver trip

-- | Total earned by a driver.
settlement :: Driver -> Int
settlement = sum . map cost . trips

tripCount :: Driver -> Int
tripCount = length . trips

-- | The driver with the fewest trips (the first one on ties), if any.
leastBusy :: [Driver] -> Maybe Driver
leastBusy [] = Nothing
leastBusy drivers = Just (minimumBy (comparing tripCount) drivers)

addTrip :: Trip -> Driver -> Driver
addTrip trip driver = driver {trips = trip : trips driver}

-- | The least busy driver among those who accept the trip makes it.
-- 'Nothing' when nobody accepts it (the original crashed in that case).
assignTrip :: Trip -> [Driver] -> Maybe Driver
assignTrip trip = fmap (addTrip trip) . leastBusy . filter (canTake trip)

-- ---------------------------------------------------------------------------
-- Point 7: a driver with infinite trips
-- ---------------------------------------------------------------------------

infiniteTrips :: [Trip]
infiniteTrips = repeat (Trip (Date 11 3 2017) lucas 50)

-- | @settlement nito@ never ends (it must add infinitely many costs), but
-- @canTake trip nito@ answers at once: it only looks at the condition, and
-- lazy evaluation never forces the infinite list of trips.
nito :: Driver
nito = Driver "Nito Infy" 70000 infiniteTrips (longClientName 3)

-- ---------------------------------------------------------------------------
-- Point 8: type inference exercise
-- ---------------------------------------------------------------------------

-- | Kept as in the statement to study its inferred type. Note that it is
-- partial: it fails if no mapped element satisfies the predicate.
gongNeng :: Ord c => c -> (c -> Bool) -> (a -> c) -> [a] -> c
gongNeng minimumValue predicate transform =
  max minimumValue . head . filter predicate . map transform
