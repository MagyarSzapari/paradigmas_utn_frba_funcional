-- | Practice exam "Pisteando como un campeon" (racing like a champion):
-- cars, track sections, tracks and races.
--
-- A section is a car transformation (@Car -> Car@). Sections such as the
-- pit stop, a wet section, gravel or an obstruction *wrap* another section,
-- so they are higher-order functions @Section -> Section@ (decorators).
-- A track is a list of sections and a lap is a left fold over it.
module Exams.Racing
  ( -- * Model (point 1)
    Wear (..)
  , Car (..)
  , Section
  , Track
  , Race (..)
  , ferrari
  , lamborghini
  , fiat
    -- * Point 2
  , isInGoodShape
  , isWrecked
    -- * Point 3
  , repair
    -- * Point 4: sections
  , addTime
  , addWheelWear
  , addChassisWear
  , curve
  , dangerousCurve
  , tightCurve
  , straight
  , classicStraight
  , shortStraight
  , pitStop
  , wet
  , gravel
  , obstructed
    -- * Point 5
  , passThrough
    -- * Point 6
  , superTrack
  , completeLap
    -- * Point 7
  , tourBuenosAires
  , playRace
    -- * Point 8: type inference
  , myPrecious
  ) where

-- ---------------------------------------------------------------------------
-- Model
-- ---------------------------------------------------------------------------

-- | A record instead of the original @(Number, Number)@ tuple: @wheels@ and
-- @chassis@ cannot be swapped by mistake any more.
data Wear = Wear
  { wheels :: Double
  , chassis :: Double
  }
  deriving (Show, Eq)

data Car = Car
  { brand :: String
  , model :: String
  , wear :: Wear
  , topSpeed :: Double
  , raceTime :: Double
  }
  deriving (Show, Eq)

type Section = Car -> Car

type Track = [Section]

data Race = Race
  { track :: Track
  , laps :: Int
  }

ferrari, lamborghini, fiat :: Car
ferrari = Car "Ferrari" "F50" (Wear 0 0) 65 0
lamborghini = Car "Lamborghini" "Diablo" (Wear 4 7) 73 0
fiat = Car "Fiat" "600" (Wear 27 33) 44 0

-- ---------------------------------------------------------------------------
-- Point 2
-- ---------------------------------------------------------------------------

isInGoodShape :: Car -> Bool
isInGoodShape car = chassis (wear car) < 40 && wheels (wear car) < 60

isWrecked :: Car -> Bool
isWrecked car = wheels (wear car) > 80 || chassis (wear car) > 80

-- ---------------------------------------------------------------------------
-- Point 3
-- ---------------------------------------------------------------------------

-- | New wheels, and the chassis keeps only 15% of its wear.
repair :: Car -> Car
repair car = car {wear = Wear 0 (chassis (wear car) * 0.15)}

-- ---------------------------------------------------------------------------
-- Point 4: sections
-- ---------------------------------------------------------------------------

addTime :: Double -> Car -> Car
addTime seconds car = car {raceTime = raceTime car + seconds}

addWheelWear :: Double -> Car -> Car
addWheelWear amount car = car {wear = (wear car) {wheels = wheels (wear car) + amount}}

addChassisWear :: Double -> Car -> Car
addChassisWear amount car = car {wear = (wear car) {chassis = chassis (wear car) + amount}}

-- | Wheels wear @3 * length / angle@; the car goes at half its top speed.
curve :: Double -> Double -> Section
curve angle metres car =
  addTime (metres / (topSpeed car / 2)) . addWheelWear (3 * metres / angle) $ car

dangerousCurve :: Section
dangerousCurve = curve 60 300

tightCurve :: Section
tightCurve = curve 110 550

-- | The chassis wears @length / 100@; the car goes at its top speed.
straight :: Double -> Section
straight metres car =
  addTime (metres / topSpeed car) . addChassisWear (metres / 100) $ car

classicStraight :: Section
classicStraight = straight 750

shortStraight :: Section
shortStraight = straight 280

-- | A car in good shape just goes through the section; otherwise it is
-- repaired first, which costs 10 extra seconds.
pitStop :: Section -> Section
pitStop section car
  | isInGoodShape car = section car
  | otherwise = section . addTime 10 . repair $ car

-- | A wet section adds half of the time the section itself took.
wet :: Section -> Section
wet section car = addTime (timeTaken / 2) afterSection
  where
    afterSection :: Car
    afterSection = section car

    timeTaken :: Double
    timeTaken = raceTime afterSection - raceTime car

-- | Gravel doubles the effect of the section.
gravel :: Section -> Section
gravel section = section . section

-- | An obstruction of @metres@ adds @2 * metres@ of wheel wear.
obstructed :: Double -> Section -> Section
obstructed metres section = addWheelWear (2 * metres) . section

-- ---------------------------------------------------------------------------
-- Point 5
-- ---------------------------------------------------------------------------

-- | A wrecked car does not go through sections any more.
passThrough :: Section -> Car -> Car
passThrough section car
  | isWrecked car = car
  | otherwise = section car

-- ---------------------------------------------------------------------------
-- Point 6
-- ---------------------------------------------------------------------------

superTrack :: Track
superTrack =
  [ classicStraight
  , tightCurve
  , wet shortStraight
  , shortStraight
  , obstructed 2 (curve 80 400)
  , curve 115 650
  , straight 970
  , dangerousCurve
  , gravel shortStraight
  , pitStop (straight 800)
  ]

-- | Every car goes through every section of the track, in order.
completeLap :: Track -> [Car] -> [Car]
completeLap sections = map (\car -> foldl (flip passThrough) car sections)

-- ---------------------------------------------------------------------------
-- Point 7
-- ---------------------------------------------------------------------------

tourBuenosAires :: Race
tourBuenosAires = Race superTrack 20

-- | The state of the cars at the end of each lap; wrecked cars leave the
-- race. @iterate@ builds an infinite list of laps and @take@ keeps only the
-- ones the race has: lazy evaluation computes just those.
playRace :: Race -> [Car] -> [[Car]]
playRace race = take (laps race) . drop 1 . iterate nextLap
  where
    nextLap :: [Car] -> [Car]
    nextLap = filter (not . isWrecked) . completeLap (track race)

-- ---------------------------------------------------------------------------
-- Point 8: type inference exercise
-- ---------------------------------------------------------------------------

-- | Kept as in the statement. GHC infers a more general type than the one
-- written in the original answer: any 'Foldable' container and any 'Num'.
myPrecious :: (Foldable t, Num n) => [(a, b -> n)] -> ((a, b -> n) -> t c, b) -> n
myPrecious ts x = sum . map (($ snd x) . snd) . filter ((> 5) . length . fst x) $ ts
