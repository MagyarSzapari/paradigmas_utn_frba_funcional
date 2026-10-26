-- | Exercise guide 4 (higher order, higher order + lists, folds).
module Guides.HigherOrder
  ( -- * Higher order
    existsInTriple
  , better
  , applyToPair
  , pairOfFunctions
    -- * Higher order + lists
  , isMultipleOfAny
  , averages
  , averagesWithoutFails
  , bestGrades
  , passed
  , passedStudents
  , divisors
  , exists
  , hasAnyNegative
  , applyFunctions
  , sumOfFunctions
  , maxSkill
  , raiseSkills
  , clamped
  , changeSkills
  , raiseLowSkillsToFour
  , leadingEvens
  , leadingDivisorsOf
  , leadingNonDivisorsOf
    -- * Folds
  , sumWithFold
  , productWithFold
  , listDispersion
  ) where

import Data.List (foldl')
import Guides.Basics (isMultipleOf)
import Guides.Tuples (fst3, snd3, thd3)
import Guides.Utils (average)

-- | Higher order 1: does any component of the triple satisfy @p@?
existsInTriple :: (a -> Bool) -> (a, a, a) -> Bool
existsInTriple p triple = any p [fst3 triple, snd3 triple, thd3 triple]

-- | Higher order 2: the greatest of both results.
-- The original evaluated @f x@ / @g x@ up to three times each.
better :: Ord b => (a -> b) -> (a -> b) -> a -> b
better f g x = max (f x) (g x)

-- | Higher order 3.
applyToPair :: (a -> b) -> (a, a) -> (b, b)
applyToPair f (x, y) = (f x, f y)

-- | Higher order 4.
pairOfFunctions :: (a -> b) -> (a -> c) -> a -> (b, c)
pairOfFunctions f g x = (f x, g x)

-- | Lists 1: is @n@ a multiple of any element?
isMultipleOfAny :: Int -> [Int] -> Bool
isMultipleOfAny n = any (isMultipleOf n)

-- | Lists 2.
averages :: [[Int]] -> [Double]
averages = map average

-- | Lists 3: averages ignoring failing grades (below 4).
-- One 'map' over a composition instead of two traversals.
averagesWithoutFails :: [[Int]] -> [Double]
averagesWithoutFails = map (average . filter (>= 4))

-- | Lists 4. Empty sub-lists are skipped instead of crashing 'maximum'.
bestGrades :: [[Int]] -> [Int]
bestGrades = map maximum . filter (not . null)

-- | Lists 5: every grade is at least 6.
-- Uses 'all' instead of @(>= 6) . minimum@, which crashed on @[]@.
passed :: [Int] -> Bool
passed = all (>= 6)

-- | Lists 6.
passedStudents :: [[Int]] -> [[Int]]
passedStudents = filter passed

-- | Lists 7.
divisors :: Int -> [Int]
divisors k = filter (isMultipleOf k) [1 .. k]

-- | Lists 8: the original's @exists f = any f@ is just 'any' (eta-reduced).
exists :: (a -> Bool) -> [a] -> Bool
exists = any

-- | Lists 9 (left unfinished in the original).
hasAnyNegative :: [Int] -> Bool
hasAnyNegative = any (< 0)

-- | Lists 10: explicit recursion replaced by 'map' and a section of '($)'.
applyFunctions :: [a -> b] -> a -> [b]
applyFunctions fs x = map ($ x) fs

-- | Lists 11.
sumOfFunctions :: [a -> Int] -> a -> Int
sumOfFunctions fs = sum . applyFunctions fs

-- | Upper bound of a skill level.
maxSkill :: Int
maxSkill = 12

-- | Lists 12: raise every skill, capped at 'maxSkill'.
raiseSkills :: Int -> [Int] -> [Int]
raiseSkills amount = map (min maxSkill . (+ amount))

-- | Lists 13: apply @f@ and clamp the result into @[0, maxSkill]@.
clamped :: (Int -> Int) -> Int -> Int
clamped f = max 0 . min maxSkill . f

-- | Lists 13.a.
changeSkills :: (Int -> Int) -> [Int] -> [Int]
changeSkills f = map (clamped f)

-- | Lists 13.b: raise every skill below 4 up to 4.
raiseLowSkillsToFour :: [Int] -> [Int]
raiseLowSkillsToFour = changeSkills (max 4)

-- | Lists 15.
leadingEvens :: [Int] -> [Int]
leadingEvens = takeWhile even

-- | Lists 15: leading elements that divide @n@.
leadingDivisorsOf :: Int -> [Int] -> [Int]
leadingDivisorsOf n = takeWhile (isMultipleOf n)

-- | Lists 15: leading elements that do not divide @n@.
leadingNonDivisorsOf :: Int -> [Int] -> [Int]
leadingNonDivisorsOf n = takeWhile (not . isMultipleOf n)

-- | Lists 19. Strict left fold: no thunk chain is accumulated.
sumWithFold :: [Int] -> Int
sumWithFold = foldl' (+) 0

-- | Lists 20.
productWithFold :: [Int] -> Int
productWithFold = foldl' (*) 1

-- | Lists 21 (left unfinished in the original): greatest minus smallest
-- in a single fold. The empty list has dispersion 0.
listDispersion :: [Int] -> Int
listDispersion [] = 0
listDispersion (x : xs) = uncurry (-) (foldl' step (x, x) xs)
  where
    -- 'seq' forces both components: foldl' alone only evaluates the
    -- pair constructor, which would still leave thunks inside it.
    step (high, low) y =
      let high' = max high y
          low' = min low y
       in high' `seq` low' `seq` (high', low')
