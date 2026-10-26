-- | Practice exam "Lovecraft": investigators of the occult, their sanity,
-- the items they find and the dreadful events they face as a group.
--
-- Consolidates the two original solutions (@solucion.hs@ and
-- @solucion-alternativa.hs@) of the same statement.
module Exams.Lovecraft
  ( -- * Model
    Investigator (..)
  , Item (..)
  , Group
  , InvestigatorChange
  , GroupChange
    -- * Helpers provided by the statement (made total)
  , maximumOn
  , deltaOn
    -- * Point 1
  , modifySanity
  , goMad
  , addItem
  , findItem
    -- * Point 2
  , hasItemNamed
  , groupHasItem
    -- * Point 3
  , isSane
  , experience
  , maxItemValue
  , potential
  , leader
    -- * Point 4
  , totalSanityDelta
  , firstMemberPotentialDelta
    -- * Point 5
  , Event (..)
  , goMadAll
  , firstFinds
  , recordEvent
  , sufferConsequences
  , face
  , terror
  , scariestEvent
  , awakeningOfAnAncientOne
  , innsmouthRitual
    -- * Sample data
  , pepe
  , luis
  , necronomicon
  , cursedDagger
  ) where

import Data.List (maximumBy)
import Data.Ord (comparing)

-- ---------------------------------------------------------------------------
-- Model
-- ---------------------------------------------------------------------------

data Investigator = Investigator
  { name :: String
  , sanity :: Int
  , items :: [Item]
  , avoidedEvents :: [String]
  }
  deriving (Show, Eq)

data Item = Item
  { itemName :: String
  , value :: Int
  }
  deriving (Show, Eq)

type Group = [Investigator]

type InvestigatorChange = Investigator -> Investigator

type GroupChange = Group -> Group

-- ---------------------------------------------------------------------------
-- Helpers provided by the statement
-- ---------------------------------------------------------------------------

-- | @maximoSegun@ of the statement, total: 'Nothing' for an empty list.
-- On ties the last element wins, exactly like the provided @foldl1@ version.
maximumOn :: Ord b => (a -> b) -> [a] -> Maybe a
maximumOn _ [] = Nothing
maximumOn weight xs = Just (maximumBy (comparing weight) xs)

-- | @deltaSegun@ of the statement: how much a measure changes when the
-- value is transformed.
deltaOn :: Num b => (a -> b) -> (a -> a) -> a -> b
deltaOn measure transform x = abs (measure (transform x) - measure x)

-- ---------------------------------------------------------------------------
-- Point 1
-- ---------------------------------------------------------------------------

modifySanity :: (Int -> Int) -> InvestigatorChange
modifySanity change investigator = investigator {sanity = change (sanity investigator)}

-- | Loses sanity points, never going below zero.
goMad :: Int -> InvestigatorChange
goMad points = modifySanity (max 0 . subtract points)

addItem :: Item -> InvestigatorChange
addItem item investigator = investigator {items = item : items investigator}

-- | Finding an item drives you as mad as the item is valuable.
findItem :: Item -> InvestigatorChange
findItem item = addItem item . goMad (value item)

-- ---------------------------------------------------------------------------
-- Point 2
-- ---------------------------------------------------------------------------

hasItemNamed :: String -> Investigator -> Bool
hasItemNamed wanted = elem wanted . map itemName . items

groupHasItem :: String -> Group -> Bool
groupHasItem = any . hasItemNamed

-- ---------------------------------------------------------------------------
-- Point 3
-- ---------------------------------------------------------------------------

isSane :: Investigator -> Bool
isSane = (> 0) . sanity

-- | One plus three times the number of avoided events.
experience :: Investigator -> Int
experience = (1 +) . (3 *) . length . avoidedEvents

-- | Zero for investigators without items (the original crashed).
maxItemValue :: Investigator -> Int
maxItemValue = maximum . (0 :) . map value . items

potential :: Investigator -> Int
potential investigator
  | isSane investigator = sanity investigator * experience investigator + maxItemValue investigator
  | otherwise = 0

leader :: Group -> Maybe Investigator
leader = maximumOn potential

-- ---------------------------------------------------------------------------
-- Point 4
-- ---------------------------------------------------------------------------

-- | Total sanity lost by the group if everybody went mad by @points@.
totalSanityDelta :: Int -> Group -> Int
totalSanityDelta points = sum . map (deltaOn sanity (goMad points))

-- | Potential difference between the first member and the first member that
-- would remain if the insane ones were left out. Thanks to lazy evaluation
-- this terminates even for an infinite group, as long as some sane member
-- eventually appears.
firstMemberPotentialDelta :: Group -> Maybe Int
firstMemberPotentialDelta group =
  case (group, filter isSane group) of
    (first : _, firstSane : _) -> Just (abs (potential firstSane - potential first))
    _ -> Nothing

-- ---------------------------------------------------------------------------
-- Point 5
-- ---------------------------------------------------------------------------

data Event = Event
  { description :: String
  , consequences :: [GroupChange]
  , canBeAvoided :: Group -> Bool
  }

instance Show Event where
  show event = "Event " ++ show (description event)

goMadAll :: Int -> GroupChange
goMadAll = map . goMad

-- | The first member finds an item; an empty group stays empty.
firstFinds :: Item -> GroupChange
firstFinds _ [] = []
firstFinds item (first : rest) = findItem item first : rest

recordEvent :: Event -> InvestigatorChange
recordEvent event investigator =
  investigator {avoidedEvents = description event : avoidedEvents investigator}

-- | Consequences happen in the order they are listed.
sufferConsequences :: Event -> GroupChange
sufferConsequences event group = foldl (flip ($)) group (consequences event)

-- | Facing an event always costs one sanity point to everybody. If the
-- group (as it was before facing it) can avoid it, everyone records it;
-- otherwise the group suffers its consequences.
face :: Event -> GroupChange
face event group
  | canBeAvoided event group = goMadAll 1 . map (recordEvent event) $ group
  | otherwise = goMadAll 1 . sufferConsequences event $ group

-- | How much total sanity the group would lose by facing the event.
terror :: Group -> Event -> Int
terror group event = deltaOn (sum . map sanity) (face event) group

scariestEvent :: Group -> [Event] -> Maybe Event
scariestEvent = maximumOn . terror

awakeningOfAnAncientOne :: Event
awakeningOfAnAncientOne =
  Event
    { description = "Awakening of an Ancient One"
    , consequences = [goMadAll 10, drop 1]
    , canBeAvoided = groupHasItem (itemName necronomicon)
    }

innsmouthRitual :: Event
innsmouthRitual =
  Event
    { description = "Ritual in Innsmouth"
    , consequences = [firstFinds cursedDagger, goMadAll 2]
    , canBeAvoided = maybe False ((> 100) . potential) . leader
    }

-- ---------------------------------------------------------------------------
-- Sample data
-- ---------------------------------------------------------------------------

necronomicon, cursedDagger :: Item
necronomicon = Item "Necronomicon" 3
cursedDagger = Item "Cursed dagger" 3

pepe, luis :: Investigator
pepe = Investigator "Pepe" 10 [cursedDagger] [description awakeningOfAnAncientOne]
luis = Investigator "Luis" 2 [necronomicon] [description innsmouthRitual]
