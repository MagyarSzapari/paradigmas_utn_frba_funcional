-- | Small, reusable combinators shared by several guide exercises.
--
-- Original sources: @maximoSegun@ (Pizza Conmigo, Lovecraft) and
-- @deltaSegun@ (Lovecraft).
module Guides.Utils
  ( maximumOn
  , deltaBy
  , average
  ) where

-- | Element whose image under @f@ is the greatest.
--
-- Total version of the course's @maximoSegun = foldl1 (mayorSegun f)@:
-- an empty list yields 'Nothing' instead of a runtime crash.
-- On ties the /last/ maximal element wins, exactly like the original.
maximumOn :: Ord b => (a -> b) -> [a] -> Maybe a
maximumOn _ [] = Nothing
maximumOn f (x : xs) = Just (foldl pickLarger x xs)
  where
    pickLarger best candidate
      | f best > f candidate = best
      | otherwise = candidate

-- | Absolute change of a measure after a transformation.
--
-- @deltaBy measure transform x = |measure (transform x) - measure x|@
deltaBy :: Num n => (a -> n) -> (a -> a) -> a -> n
deltaBy measure transform x = abs (measure (transform x) - measure x)

-- | Arithmetic mean of integral values. By convention the mean of an
-- empty list is @0@ (the original divided by zero and returned @NaN@).
average :: [Int] -> Double
average [] = 0
average xs = fromIntegral (sum xs) / fromIntegral (length xs)
