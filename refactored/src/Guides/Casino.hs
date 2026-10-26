-- | Practice assignment "Casino Royale".
module Guides.Casino
  ( Suit (..)
  , suitName
  , validSuitNames
  , Card (..)
  , Player (..)
  , fourAces
  , jokersFullHouse
  , ninesLeg
  , jamesBond
  , leChiffre
  , felixLeiter
  , table
  , occurrencesOf
  , concatenate
  , largerBy
  , maximumBy'
  , withoutDuplicates
  , isInvalidCard
  ) where

-- | The four legal suits of a French deck.
data Suit = Hearts | Spades | Clubs | Diamonds
  deriving (Show, Eq, Enum, Bounded)

-- | Name of the suit as it appears in the cards' raw data.
suitName :: Suit -> String
suitName Hearts = "Corazones"
suitName Spades = "Picas"
suitName Clubs = "Tréboles"
suitName Diamonds = "Diamantes"

-- | Every legal suit name, derived from the ADT (no hand-written list).
validSuitNames :: [String]
validSuitNames = map suitName [minBound .. maxBound]

-- | A card as it is dealt: the suit stays a 'String' on purpose because
-- the exercise is precisely about detecting cards with illegal data
-- (e.g. @"Copas"@, a Spanish-deck suit).
data Card = Card
  { rank :: Int
  , suit :: String
  }
  deriving (Show, Eq)

-- | A player at the table.
data Player = Player
  { name :: String
  , hand :: [Card]
  , drink :: String
  }
  deriving (Show, Eq)

-- | Helper to build hands from @(rank, suit)@ pairs.
cards :: [(Int, String)] -> [Card]
cards = map (uncurry Card)

fourAces :: [Card]
fourAces =
  cards [(1, "Corazones"), (1, "Picas"), (1, "Tréboles"), (1, "Diamantes"), (10, "Diamantes")]

jokersFullHouse :: [Card]
jokersFullHouse =
  cards [(11, "Corazones"), (11, "Picas"), (11, "Tréboles"), (10, "Diamantes"), (10, "Picas")]

ninesLeg :: [Card]
ninesLeg =
  cards [(9, "Corazones"), (9, "Picas"), (9, "Tréboles"), (10, "Diamantes"), (4, "Copas")]

jamesBond :: Player
jamesBond = Player "jamesBond" fourAces "Martini... shaken, not stirred"

leChiffre :: Player
leChiffre = Player "leChiffre" jokersFullHouse "Gin"

felixLeiter :: Player
felixLeiter = Player "felixLeiter" ninesLeg "Whisky"

-- | The original @mesaQueMasAplauda@.
table :: [Player]
table = [jamesBond, leChiffre, felixLeiter]

-- | Given helper: how many times @x@ occurs.
occurrencesOf :: Eq a => a -> [a] -> Int
occurrencesOf x = length . filter (== x)

-- | Given helper. The original @foldl (++) []@ is quadratic because every
-- step re-copies the accumulated prefix; 'foldr' (i.e. 'concat') is
-- linear and also works lazily on infinite lists.
concatenate :: [[a]] -> [a]
concatenate = foldr (++) []

-- | Exercise 1.a: the element (not its image!) that is greater by @f@.
-- On ties the first one wins.
--
-- The original returned @f x1@ in two branches and @x1@ in the third,
-- mixing "element" and "measure" and forcing the type @Number -> Number@.
largerBy :: Ord b => (a -> b) -> a -> a -> a
largerBy f x y
  | f y > f x = y
  | otherwise = x

-- | Exercise 1.b: the element that maximises @f@ ('Nothing' if empty).
-- The original returned the maximum /image/, not the element.
maximumBy' :: Ord b => (a -> b) -> [a] -> Maybe a
maximumBy' _ [] = Nothing
maximumBy' f xs = Just (foldl1 (largerBy f) xs)

-- | Exercise 1.c: remove duplicates keeping first occurrences
-- (equivalent to @Data.List.nub@).
withoutDuplicates :: Eq a => [a] -> [a]
withoutDuplicates [] = []
withoutDuplicates (x : xs) = x : withoutDuplicates (filter (/= x) xs)

-- | Exercise 2.a: a card is invalid when its rank is outside 1..13 or its
-- suit is not one of the legal ones.
isInvalidCard :: Card -> Bool
isInvalidCard (Card cardRank cardSuit) =
  not (cardRank `elem` [1 .. 13] && cardSuit `elem` validSuitNames)
