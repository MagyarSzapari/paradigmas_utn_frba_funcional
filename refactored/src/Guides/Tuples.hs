-- | Exercise guide 3: tuples, pattern matching and guards.
module Guides.Tuples
  ( fst3
  , snd3
  , thd3
  , applyBoth
  , oddCalculation
  , Grade
  , Grades
  , Retakes
  , StudentRecord
  , isFailingGrade
  , passedBoth
  , promoted
  , passedFirstPartial
  , finalGrades
  , mustRetakeCourse
  , retookFirstPartial
  , retookForFun
  , isOfAge
  , calculate
  ) where

import Data.Maybe (isJust)

-- | Exercise 1.
fst3 :: (a, b, c) -> a
fst3 (x, _, _) = x

-- | Exercise 1.
snd3 :: (a, b, c) -> b
snd3 (_, y, _) = y

-- | Exercise 1.
thd3 :: (a, b, c) -> c
thd3 (_, _, z) = z

-- | Exercise 2: apply two functions to the same argument.
-- Generalised from @Number@ to any type @a@ thanks to type inference.
applyBoth :: (a -> b, a -> c) -> a -> (b, c)
applyBoth (f, g) x = (f x, g x)

-- | Exercise 3. If the first is greater, add; otherwise, if the gap is
-- greater than 10, subtract; otherwise multiply.
--
-- The original's last guard was @y - x < 10 && y - x > 0@, so @(5, 5)@ and
-- @(0, 10)@ fell through every guard and crashed at runtime.
oddCalculation :: (Int, Int) -> Int
oddCalculation (x, y)
  | x > y = x + y
  | y - x > 10 = y - x
  | otherwise = x * y

-- | A numeric grade from 1 to 10.
type Grade = Int

-- | Grades of the first and second partial exams.
type Grades = (Grade, Grade)

-- | Optional retake grades. 'Nothing' replaces the original magic value @-1@.
type Retakes = (Maybe Grade, Maybe Grade)

-- | Original grades plus retakes.
type StudentRecord = (Grades, Retakes)

-- | Exercise 4.a.
isFailingGrade :: Grade -> Bool
isFailingGrade = (< 6)

-- | Exercise 4.b.
passedBoth :: Grades -> Bool
passedBoth (first, second) = all (not . isFailingGrade) [first, second]

-- | Exercise 4.c: both at least 7 and adding up to at least 15.
promoted :: Grades -> Bool
promoted (first, second) = first + second >= 15 && first >= 7 && second >= 7

-- | Exercise 4.d.
passedFirstPartial :: Grades -> Bool
passedFirstPartial = not . isFailingGrade . fst

-- | Exercise 5.a: the best grade of each partial, retake included.
finalGrades :: StudentRecord -> Grades
finalGrades ((first, second), (retake1, retake2)) =
  (bestOf first retake1, bestOf second retake2)
  where
    bestOf grade = maybe grade (max grade)

-- | Exercise 5.b.
mustRetakeCourse :: StudentRecord -> Bool
mustRetakeCourse = not . passedBoth . finalGrades

-- | Exercise 5.c.
retookFirstPartial :: StudentRecord -> Bool
retookFirstPartial (_, (retake1, _)) = isJust retake1

-- | Exercise 5.d: promoted with the original grades and still retook one.
retookForFun :: StudentRecord -> Bool
retookForFun (grades, (retake1, retake2)) =
  promoted grades && any isJust [retake1, retake2]

-- | Exercise 6 (left unfinished in the original, completed from the guide
-- statement): a person is of age when older than 21.
isOfAge :: (String, Int) -> Bool
isOfAge (_, age) = age > 21

-- | Exercise 7 (left unfinished in the original, completed from the guide
-- statement): double the first element if even; add 1 to the second if odd.
calculate :: (Int, Int) -> (Int, Int)
calculate (x, y) = (doubleIfEven x, incrementIfOdd y)
  where
    doubleIfEven n
      | even n = 2 * n
      | otherwise = n
    incrementIfOdd n
      | odd n = n + 1
      | otherwise = n
