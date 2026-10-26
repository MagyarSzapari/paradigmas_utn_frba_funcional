-- | Exercise guide 1: basic functions, guards and composition.
module Guides.Basics
  ( double
  , isMultipleOfThree
  , isMultipleOf
  , cube
  , triangleArea
  , isLeapYear
  , isLeapYearGuards
  , celsiusToFahrenheit
  , fahrenheitToCelsius
  , isColdFahrenheit
  , leastCommonMultiple
  , dispersion
  , areEvenDays
  , areCrazyDays
  , areNormalDays
  , pineWeight
  , isUsefulWeight
  , isUsefulPine
  , isPerfectSquare
  ) where

-- | Sample function used by the original project to check the setup.
double :: Int -> Int
double n = n + n

-- | Exercise 1.
isMultipleOfThree :: Int -> Bool
isMultipleOfThree = (`isMultipleOf` 3)

-- | Exercise 2. @x `isMultipleOf` y@ holds when @y@ divides @x@.
isMultipleOf :: Int -> Int -> Bool
isMultipleOf x y = x `mod` y == 0

-- | Exercise 3. Polymorphic: works for any 'Num' instance.
cube :: Num a => a -> a
cube x = x * x * x

-- | Exercise 4: area of a triangle given base and height.
triangleArea :: Double -> Double -> Double
triangleArea base height = base * height / 2

-- | Exercise 5 (Gregorian rule): divisible by 400, or by 4 but not by 100.
--
-- The original wrote @... || (mult 4 && mult 100)@, which rejects 2024.
isLeapYear :: Int -> Bool
isLeapYear year =
  year `isMultipleOf` 400 || (year `isMultipleOf` 4 && not (year `isMultipleOf` 100))

-- | Exercise 5, alternative with guards (this one was already correct).
isLeapYearGuards :: Int -> Bool
isLeapYearGuards year
  | year `isMultipleOf` 400 = True
  | year `isMultipleOf` 100 = False
  | otherwise = year `isMultipleOf` 4

-- | Exercise 6.
celsiusToFahrenheit :: Double -> Double
celsiusToFahrenheit celsius = celsius * 1.8 + 32

-- | Exercise 7.
fahrenheitToCelsius :: Double -> Double
fahrenheitToCelsius fahrenheit = (fahrenheit - 32) / 1.8

-- | Exercise 8: it is cold below 8 degrees Celsius.
isColdFahrenheit :: Double -> Bool
isColdFahrenheit = (< 8) . fahrenheitToCelsius

-- | Exercise 9. Delegates to the Prelude's 'lcm', which also handles 0
-- (the original @x * y / gcd x y@ divided by zero for @0 0@).
leastCommonMultiple :: Int -> Int -> Int
leastCommonMultiple = lcm

-- | Exercise 10.a: difference between the greatest and smallest value.
dispersion :: Int -> Int -> Int -> Int
dispersion x y z = maximum values - minimum values
  where
    values = [x, y, z]

-- | Exercise 10.b.
areEvenDays :: Int -> Int -> Int -> Bool
areEvenDays x y z = dispersion x y z < 30

-- | Exercise 10.b.
areCrazyDays :: Int -> Int -> Int -> Bool
areCrazyDays x y z = dispersion x y z > 100

-- | Exercise 10.b: neither even nor crazy (no @== False@ comparisons).
areNormalDays :: Int -> Int -> Int -> Bool
areNormalDays x y z = not (areEvenDays x y z || areCrazyDays x y z)

-- | Exercise 11.a: 3 kg/cm for the first 300 cm, 2 kg/cm afterwards.
pineWeight :: Int -> Int
pineWeight height
  | height <= 300 = height * 3
  | otherwise = 300 * 3 + (height - 300) * 2

-- | Exercise 11.b.
isUsefulWeight :: Int -> Bool
isUsefulWeight weight = weight > 400 && weight < 1000

-- | Exercise 11.c.
isUsefulPine :: Int -> Bool
isUsefulPine = isUsefulWeight . pineWeight

-- | Exercise 12 (left unfinished in the original). Searches an infinite
-- list lazily: 'takeWhile' stops as soon as the squares exceed @n@.
isPerfectSquare :: Int -> Bool
isPerfectSquare n = n `elem` takeWhile (<= n) (map (^ (2 :: Int)) [0 ..])
