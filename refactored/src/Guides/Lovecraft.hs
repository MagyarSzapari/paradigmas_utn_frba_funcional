-- | Exam practice "Lovecraft": investigators, items and sanity.
module Guides.Lovecraft
  ( Item (..)
  , Investigator (..)
  , Group
  , StateChange
  , GroupChange
  , necronomicon
  , cursedDagger
  , pepe
  , luis
  , goMad
  , findItem
  , groupHasItem
  , leader
  , potential
  , experience
  , totalSanityDelta
  , firstSaneMemberDelta
  ) where

import Data.Maybe (listToMaybe)
import Guides.Utils (deltaBy, maximumOn)

data Item = Item
  { itemName :: String
  , value :: Int
  }
  deriving (Show, Eq)

data Investigator = Investigator
  { investigatorName :: String
  , sanity :: Int
  , items :: [Item]
  , avoidedEvents :: [String]
  }
  deriving (Show, Eq)

type Group = [Investigator]

type StateChange = Investigator -> Investigator

type GroupChange = Group -> Group

necronomicon :: Item
necronomicon = Item "Necronomicon" 3

cursedDagger :: Item
cursedDagger = Item "Daga maldita" 3

pepe :: Investigator
pepe = Investigator "Pepe" 10 [cursedDagger] ["Despertar de un antiguo"]

luis :: Investigator
luis = Investigator "Luis" 2 [necronomicon] ["Ritual en Innsmouth"]

-- | 1.a: lose sanity, never below 0.
goMad :: Int -> StateChange
goMad amount investigator =
  investigator {sanity = max 0 (sanity investigator - amount)}

-- | 1.b: finding an item costs as much sanity as the item is worth.
-- Reuses 'goMad' instead of duplicating the clamping logic.
findItem :: Item -> StateChange
findItem item = addItem . goMad (value item)
  where
    addItem investigator = investigator {items = item : items investigator}

-- | 2: does any member own the item?
groupHasItem :: Item -> Group -> Bool
groupHasItem item = any ((item `elem`) . items)

-- | 3: the member with the highest potential ('Nothing' for an empty group).
leader :: Group -> Maybe Investigator
leader = maximumOn potential

-- | 3: 0 when insane; otherwise sanity * experience + best item value.
-- An investigator without items contributes 0 (the original crashed).
potential :: Investigator -> Int
potential investigator
  | sanity investigator == 0 = 0
  | otherwise = sanity investigator * experience investigator + bestItemValue
  where
    bestItemValue = maximum (0 : map value (items investigator))

-- | 3: 1 + 3 per avoided event.
experience :: Investigator -> Int
experience = (1 +) . (3 *) . length . avoidedEvents

-- | 4.a: total sanity lost by the group if everybody goes mad by @n@.
totalSanityDelta :: Int -> Group -> Int
totalSanityDelta n = sum . map (deltaBy sanity (goMad n))

-- | 4.b: change in the potential of the group's first member after
-- removing the insane ones.
--
-- The original filtered with @(0 ==) . cordura@, keeping /only/ the insane
-- members (whose potential is 0 by definition), so the result was always
-- the first member's potential. It also crashed on empty groups.
firstSaneMemberDelta :: Group -> Int
firstSaneMemberDelta = deltaBy firstPotential (filter ((> 0) . sanity))
  where
    firstPotential = maybe 0 potential . listToMaybe
