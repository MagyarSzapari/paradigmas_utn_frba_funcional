-- | Practice exam "Samurai Jack": characters, the elements they carry and
-- the fights between them.
--
-- Every element bundles two behaviours as functions (its attack and its
-- defence), so a fight is just folding those functions over a character.
module Exams.SamuraiJack
  ( -- * Model
    ElementKind (..)
  , Element (..)
  , Character (..)
    -- * Point 1
  , sendToYear
  , modifyHealth
  , meditate
  , takeDamage
    -- * Point 2
  , isEvil
  , damageDealt
  , isDead
  , hasDeadlyAttack
  , isDeadlyEnemy
  , deadlyEnemies
    -- * Point 3
  , applyTimes
  , concentration
  , minion
  , evilMinions
  , magicKatana
  , jack
  , aku
  , futurePortalFrom
    -- * Point 4
  , useElements
  , fight
    -- * Point 5: type inference
  , inferenceExercise
  ) where

import Data.Function (on)

-- ---------------------------------------------------------------------------
-- Model
-- ---------------------------------------------------------------------------

-- | A closed set of kinds instead of free-form strings: a typo such as
-- @"Maldad "@ is now a compile-time error, not a silent bug.
data ElementKind = Magic | Evil
  deriving (Show, Eq)

data Element = Element
  { kind :: ElementKind
  , attack :: Character -> Character
  , defense :: Character -> Character
  }

-- | Elements are shown and compared by their kind only (functions have no
-- 'Show' nor 'Eq'), as in the original solution.
instance Show Element where
  showsPrec d element = showParen (d > 10) $ showString "Element " . showsPrec 11 (kind element)

instance Eq Element where
  (==) = (==) `on` kind

data Character = Character
  { name :: String
  , health :: Float
  , elements :: [Element]
  , presentYear :: Int
  }
  deriving (Show, Eq)

-- ---------------------------------------------------------------------------
-- Point 1
-- ---------------------------------------------------------------------------

sendToYear :: Int -> Character -> Character
sendToYear year character = character {presentYear = year}

-- | The single place where health changes.
modifyHealth :: (Float -> Float) -> Character -> Character
modifyHealth change character = character {health = change (health character)}

meditate :: Character -> Character
meditate = modifyHealth (* 1.5)

-- | Health never goes below zero.
takeDamage :: Float -> Character -> Character
takeDamage damage = modifyHealth (max 0 . subtract damage)

-- ---------------------------------------------------------------------------
-- Point 2
-- ---------------------------------------------------------------------------

isEvil :: Character -> Bool
isEvil = any ((== Evil) . kind) . elements

damageDealt :: Character -> Element -> Float
damageDealt character element = health character - health (attack element character)

isDead :: Character -> Bool
isDead = (== 0) . health

hasDeadlyAttack :: Character -> Element -> Bool
hasDeadlyAttack character element = isDead (attack element character)

isDeadlyEnemy :: Character -> Character -> Bool
isDeadlyEnemy character = any (hasDeadlyAttack character) . elements

deadlyEnemies :: Character -> [Character] -> [Character]
deadlyEnemies = filter . isDeadlyEnemy

-- ---------------------------------------------------------------------------
-- Point 3
-- ---------------------------------------------------------------------------

-- | Applies a function @n@ times (@n <= 0@ means not at all). Safer than
-- @(!! n) . iterate f@, which fails for negative @n@.
applyTimes :: Int -> (a -> a) -> a -> a
applyTimes n f = foldr (.) id (replicate n f)

-- | Magic whose defence is meditating @level@ times.
concentration :: Int -> Element
concentration level = Element Magic id (applyTimes level meditate)

minion :: Element
minion = Element Evil (takeDamage 1) id

evilMinions :: Int -> [Element]
evilMinions amount = replicate amount minion

magicKatana :: Element
magicKatana = Element Magic (takeDamage 1000) id

jack :: Character
jack = Character "Jack" 300 [concentration 3, magicKatana] 200

-- | Aku is recursive: his portal to the future creates *another* Aku. The
-- definition is fine because of lazy evaluation; a new Aku is only built
-- when the portal's defence is actually used.
aku :: Int -> Float -> Character
aku year initialHealth =
  Character
    { name = "Aku"
    , health = initialHealth
    , elements = concentration 4 : futurePortalFrom year : evilMinions (100 * year)
    , presentYear = year
    }

futurePortalFrom :: Int -> Element
futurePortalFrom year = Element Magic (sendToYear futureYear) (aku futureYear . health)
  where
    futureYear :: Int
    futureYear = year + 2800

-- ---------------------------------------------------------------------------
-- Point 4
-- ---------------------------------------------------------------------------

-- | Applies one of the behaviours (attack or defence) of every element.
useElements :: (Element -> Character -> Character) -> Character -> [Element] -> Character
useElements behaviour character = foldl (flip ($)) character . map behaviour

-- | Returns @(winner, loser)@. Each round the attacker hits the defender
-- with all its elements and defends itself with them; then roles swap.
-- Like the original, it does not terminate if nobody can ever die.
fight :: Character -> Character -> (Character, Character)
fight attacker defender
  | isDead attacker = (defender, attacker)
  | otherwise = fight nextAttacker nextDefender
  where
    nextAttacker :: Character
    nextAttacker = useElements attack defender (elements attacker)

    nextDefender :: Character
    nextDefender = useElements defense attacker (elements attacker)

-- ---------------------------------------------------------------------------
-- Point 5: type inference exercise
-- ---------------------------------------------------------------------------

-- | Function @f@ of the statement, with the type GHC infers for it.
inferenceExercise :: (Eq t1, Num t2) => (t1 -> a1 -> (a2, a2)) -> (t2 -> t1) -> t1 -> [a1] -> [a2]
inferenceExercise x y z
  | y 0 == z = map (fst . x z)
  | otherwise = map (snd . x (y 0))
