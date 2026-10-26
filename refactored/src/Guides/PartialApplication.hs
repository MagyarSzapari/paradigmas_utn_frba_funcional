-- | Exercise guide 2: partial application, sections and composition.
module Guides.PartialApplication
  ( successor
  , half
  , inverse
  , triple
  , isPositive
  , divides
  , isLeapYearPointFree
  , inverseSquareRoot
  , addSquareOf
  , isPowerEven
  ) where

import Guides.Basics (isMultipleOf)

-- | Exercise 1.
successor :: Int -> Int
successor = (+ 1)

-- | Exercise 2.
half :: Double -> Double
half = (/ 2)

-- | Exercise 3.
inverse :: Double -> Double
inverse = (1 /)

-- | Exercise 4.
triple :: Int -> Int
triple = (3 *)

-- | Exercise 5.
isPositive :: Int -> Bool
isPositive = (> 0)

-- | Exercise 6. @divides d n@: does @d@ divide @n@?
--
-- The original was named @esMultiploDeAP@ but its arguments were in the
-- opposite order of @esMultiploDe@; the new name states the real meaning.
divides :: Int -> Int -> Bool
divides d = (== 0) . (`mod` d)

-- | Exercise 7: leap year built only from sections and composition.
--
-- @(m400 ||) . (m4 &&) . not . (multiple of)@ applied to 100.
isLeapYearPointFree :: Int -> Bool
isLeapYearPointFree year =
  ((year `isMultipleOf` 400 ||) . (year `isMultipleOf` 4 &&) . not . isMultipleOf year) 100

-- | Exercise 8.
inverseSquareRoot :: Double -> Double
inverseSquareRoot = inverse . sqrt

-- | Exercise 9: @addSquareOf m n = n + m^2@.
addSquareOf :: Int -> Int -> Int
addSquareOf = (+) . (^ (2 :: Int))

-- | Exercise 10: is @base ^ exponent@ even?
isPowerEven :: Int -> Int -> Bool
isPowerEven base = even . (base ^)
