-- | Exercise guide 4 (lists): basic list processing.
module Guides.Lists
  ( sumList
  , heartRates
  , averageHeartRate
  , heartRateAt
  , heartRatesUntil
  , isPalindrome
  , CallLog (..)
  , callLogs
  , bandWithMoreMinutes
  , bandWithMoreCalls
  ) where

import Guides.Utils (average)

-- | Exercise 1.
sumList :: [Int] -> Int
sumList = sum

-- | Exercise 2: heart rate sampled every 10 minutes.
heartRates :: [Int]
heartRates = [80, 100, 120, 128, 130, 123, 125]

-- | Exercise 2.a.
averageHeartRate :: [Int] -> Double
averageHeartRate = average

-- | Exercise 2.b: heart rate at a given minute (a multiple of 10).
--
-- Total version: the original used @(!!)@, which crashes out of range.
heartRateAt :: Int -> Maybe Int
heartRateAt minute =
  case drop (minute `div` 10) heartRates of
    (rate : _) | minute >= 0 -> Just rate
    _ -> Nothing

-- | Exercise 2.c: every sample up to the given minute (inclusive).
heartRatesUntil :: Int -> [Int]
heartRatesUntil minute = take (minute `div` 10 + 1) heartRates

-- | Exercise 3: is the concatenation of the words a palindrome?
-- The @where@ binding guarantees @concat@ is evaluated only once.
isPalindrome :: [String] -> Bool
isPalindrome wordsList = text == reverse text
  where
    text = concat wordsList

-- | Exercise 4: durations (in minutes) of the calls of a time band.
-- Replaces the anonymous @(String, [Number])@ tuple with a record.
data CallLog = CallLog
  { band :: String
  , durations :: [Int]
  }
  deriving (Show, Eq)

-- | Exercise 4 sample data: (reduced band, normal band).
callLogs :: (CallLog, CallLog)
callLogs =
  ( CallLog "horarioReducido" [20, 10, 25, 15]
  , CallLog "horarioNormal" [10, 5, 8, 2, 9, 10]
  )

-- | Exercise 4.a (commented out in the original because it used a value,
-- @duracionLlamadas@, as if it were a type). Ties go to the second band.
bandWithMoreMinutes :: (CallLog, CallLog) -> String
bandWithMoreMinutes = bandWithMoreBy (sum . durations)

-- | Exercise 4.b. Ties go to the second band.
bandWithMoreCalls :: (CallLog, CallLog) -> String
bandWithMoreCalls = bandWithMoreBy (length . durations)

-- | The shared logic of 4.a and 4.b, abstracted over the measure.
bandWithMoreBy :: (CallLog -> Int) -> (CallLog, CallLog) -> String
bandWithMoreBy measure (first, second)
  | measure first > measure second = band first
  | otherwise = band second
