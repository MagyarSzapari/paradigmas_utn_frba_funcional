-- | Exam practice "Samurai Jack": characters, elements and fights.
module Guides.SamuraiJack
  ( ElementKind (..)
  , Character (..)
  , Element (..)
  , sendToYear
  , meditate
  , takeDamage
  , modifyHealth
  , isEvil
  , isOfKind
  , damageDealt
  , deadlyEnemies
  , isDeadlyEnemy
  , hasDeadlyAttack
  , isDead
  , doNothing
  , concentration
  , evilMinions
  , minion
  , magicKatana
  , jack
  , aku
  , futurePortalFrom
  , fight
  , useElements
  , inferenceExample
  ) where

-- | Kinds of elements. Replaces the free-text @"Magia"@ / @"Maldad"@, so a
-- typo becomes a compile-time error instead of a silent False.
data ElementKind = Magic | Evil
  deriving (Show, Eq)

data Character = Character
  { characterName :: String
  , health :: Double
  , elements :: [Element]
  , currentYear :: Int
  }
  deriving (Show, Eq)

-- | Elements hold functions, so 'Show' and 'Eq' cannot be derived; both are
-- defined in terms of the element kind, like in the original.
data Element = Element
  { kind :: ElementKind
  , attack :: Character -> Character
  , defense :: Character -> Character
  }

instance Show Element where
  show = show . kind

instance Eq Element where
  e1 == e2 = kind e1 == kind e2

-- | 1: record-update syntax (the original also had a version that rebuilt
-- the whole constructor by pattern matching).
sendToYear :: Int -> Character -> Character
sendToYear year character = character {currentYear = year}

-- | 1
meditate :: Character -> Character
meditate = modifyHealth (* 1.5)

-- | 1: health never drops below 0.
takeDamage :: Double -> Character -> Character
takeDamage damage = modifyHealth (max 0 . subtract damage)

-- | The reusable abstraction behind 'meditate' and 'takeDamage'.
modifyHealth :: (Double -> Double) -> Character -> Character
modifyHealth transform character = character {health = transform (health character)}

-- | 2
isEvil :: Character -> Bool
isEvil = any (isOfKind Evil) . elements

isOfKind :: ElementKind -> Element -> Bool
isOfKind k = (== k) . kind

-- | 2: health lost by the character when hit by the element.
damageDealt :: Character -> Element -> Double
damageDealt character element = health character - health (attack element character)

-- | 2: enemies with at least one element that kills the character.
deadlyEnemies :: Character -> [Character] -> [Character]
deadlyEnemies character = filter (isDeadlyEnemy character)

isDeadlyEnemy :: Character -> Character -> Bool
isDeadlyEnemy character = any (hasDeadlyAttack character) . elements

-- | The original had two equivalent versions; this is the declarative one.
hasDeadlyAttack :: Character -> Element -> Bool
hasDeadlyAttack character element = isDead (attack element character)

isDead :: Character -> Bool
isDead = (== 0) . health

-- | 3
doNothing :: Character -> Character
doNothing = id

-- | 3: a magic element whose defense meditates @level@ times.
-- 'iterate' builds an infinite lazy list; '(!!)' only forces what it needs.
concentration :: Int -> Element
concentration level =
  Element
    { kind = Magic
    , attack = doNothing
    , defense = (!! level) . iterate meditate
    }

-- | 3
evilMinions :: Int -> [Element]
evilMinions amount = replicate amount minion

minion :: Element
minion = Element Evil (takeDamage 1) doNothing

magicKatana :: Element
magicKatana = Element Magic (takeDamage 1000) doNothing

-- | 3
jack :: Character
jack = Character "Jack" 300 [concentration 3, magicKatana] 200

-- | 3: Aku is (lazily) self-referential: his portal's defense creates a
-- new Aku 2800 years later. Nothing is built until someone uses it.
aku :: Int -> Double -> Character
aku year initialHealth =
  Character
    { characterName = "Aku"
    , health = initialHealth
    , elements = concentration 4 : futurePortalFrom year : evilMinions (100 * year)
    , currentYear = year
    }

futurePortalFrom :: Int -> Element
futurePortalFrom year = Element Magic (sendToYear future) (aku future . health)
  where
    future = year + 2800

-- | 4: the attacker uses all his elements' attacks on the defender and
-- their defenses on himself; then roles swap. Returns (winner, loser).
-- Note: like the original, it may not terminate if nobody can die.
fight :: Character -> Character -> (Character, Character)
fight attacker defender
  | isDead attacker = (defender, attacker)
  | otherwise = fight nextAttacker nextDefender
  where
    nextAttacker = useElements attack defender (elements attacker)
    nextDefender = useElements defense attacker (elements attacker)

-- | Apply one effect of every element, in order, to a character.
-- @foldl (flip ($))@ threads the character through the list of effects.
useElements :: (Element -> Character -> Character) -> Character -> [Element] -> Character
useElements effect character = foldl (flip ($)) character . map effect

-- | 5: type-inference exercise (the original was a commented-out @f@).
-- GHC infers exactly this signature, with the 'Eq' and 'Num' constraints
-- coming from @== z@ and the literal @0@.
inferenceExample :: (Eq b, Num n) => (b -> a -> (c, c)) -> (n -> b) -> b -> [a] -> [c]
inferenceExample x y z
  | y 0 == z = map (fst . x z)
  | otherwise = map (snd . x (y 0))
