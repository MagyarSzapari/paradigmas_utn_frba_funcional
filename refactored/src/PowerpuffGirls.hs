-- | Group assignment (TP) "Las Chicas Superpoderosas" (The Powerpuff Girls).
--
-- Girls, threats and cities are modelled as plain algebraic data types.
-- Every behaviour that changes an entity (a food, a villain effect, an
-- episode) is a pure transformation @a -> a@, so they can be combined with
-- function composition and folds.
module PowerpuffGirls
  ( -- * Domain types
    Name
  , Skill
  , Person (..)
  , Threat (..)
  , City (..)
  , Food
  , CityEffect
  , Episode (..)
  , Season
    -- * Sample data
  , blossom
  , bubbles
  , buttercup
  , msBellum
  , mrPig
  , silico
  , mojoJojo
  , princess
  , gangreneGang
  , townsville
  , stardewValley
  , palletTown
    -- * Part 1: threats
  , potentialDamage
  , canAttack
  , canDefeat
  , isHighLevel
    -- * Part 2: warm-up
  , isWeakTo
  , isInvulnerableFor
  , predominantThreat
  , namesOfDefeaters
  , bNamedAndMultiSkilled
    -- * Part 2: going to the nutritionist
  , modifyResistance
  , befriend
  , substanceX
  , beer
  , flavouring
  , fernet
  , barbecueChef
  , gatorei
  , vodka
  , liquidCaramel
  , cocucha
    -- * Part 2: my favourite villain
  , rumourOfAttack
  , attack
  , noEffect
  , gangreneEffect
    -- * Part 2: let's watch one more
  , fedGirl
  , affects
  , play
  , marathon
  , episode1
  , episode2
  , episode3
  , episode4
  , season1
  ) where

import Data.Char (toLower)
import Data.List (isPrefixOf, maximumBy, nub)
import Data.Ord (comparing)

-- ---------------------------------------------------------------------------
-- Domain types
-- ---------------------------------------------------------------------------

type Name = String

type Skill = String

data Person = Person
  { name :: Name
  , resistance :: Int
  , skills :: [Skill]
  , friends :: [Name]
  }
  deriving (Show, Eq)

-- | A threat carries its own effect as a first-class function.
data Threat = Threat
  { purpose :: String
  , power :: Int
  , weaknesses :: [Skill]
  , effect :: CityEffect
  }

data City = City
  { cityName :: Name
  , population :: Int
  }
  deriving (Show, Eq)

-- | Anything a girl consumes: a pure transformation of the person.
type Food = Person -> Person

-- | Anything that happens to a city: a pure transformation of the city.
type CityEffect = City -> City

-- | An episode names the city the villain targets (as the assignment says).
data Episode = Episode
  { villain :: Threat
  , girl :: Person
  , targetCity :: Name
  , foods :: [Food]
  }

-- Functions cannot be shown, so these instances show every other field
-- (useful for test failure messages and for debugging in GHCi).
instance Show Threat where
  showsPrec d threat =
    showParen (d > 10) $
      showString "Threat "
        . showsPrec 11 (purpose threat)
        . showChar ' '
        . showsPrec 11 (power threat)
        . showChar ' '
        . showsPrec 11 (weaknesses threat)

instance Show Episode where
  showsPrec d episode =
    showParen (d > 10) $
      showString "Episode "
        . showsPrec 11 (villain episode)
        . showChar ' '
        . showsPrec 11 (name (girl episode))
        . showChar ' '
        . showsPrec 11 (targetCity episode)
        . showString " <"
        . shows (length (foods episode))
        . showString " foods>"

-- | A season is just an ordered list of episodes.
type Season = [Episode]

-- ---------------------------------------------------------------------------
-- Sample data
-- ---------------------------------------------------------------------------

blossom, bubbles, buttercup, msBellum, mrPig, silico :: Person
blossom = Person "Blossom" 55 ["listening to songs", "strong punches"] ["Mr. Pig", "Silico"]
bubbles = Person "Bubbles" 30 ["speed", "bubbles"] ["Ms. Bellum"]
buttercup = Person "Buttercup" 75 ["speed", "super strength"] []
msBellum = Person "Ms. Bellum" 10 ["not showing her face"] ["Bubbles"]
mrPig = Person "Mr. Pig" 0 [] ["Buttercup"]
silico = Person "Silico" 29 ["tolerating odd numbers"] ["Blossom", "Bubbles"]

-- NOTE: 'canDefeat' depends on the parity of the purpose's length, so the
-- English purposes were chosen to keep the same (even) parity as the
-- original Spanish strings.

-- | Mojo Jojo's effect refers to Mojo Jojo himself. This recursive value is
-- fine in Haskell: thanks to lazy evaluation the record is built first and
-- the self-reference is only followed when the effect is actually applied.
mojoJojo :: Threat
mojoJojo =
  Threat
    { purpose = "to destroy the powerpuff girls"
    , power = 70
    , weaknesses = ["speed", "super strength"]
    , effect = rumourOfAttack mojoJojo
    }

princess :: Threat
princess =
  Threat
    { purpose = "be the only powerpuff girl"
    , power = 95
    , weaknesses = ["bubbles", "strong punches"]
    , effect = noEffect
    }

gangreneGang :: Threat
gangreneGang =
  Threat
    { purpose = "spread chaos and make everyone lazy and fight each other"
    , power = 49
    , weaknesses = ["listening to songs", "super strength", kryptonite]
    , effect = gangreneEffect
    }

townsville, stardewValley, palletTown :: City
townsville = City "Townsville" 21
stardewValley = City "Stardew Valley" 27
palletTown = City "Pallet Town" 10

-- | Single source of truth for the weakness checked by 'isHighLevel'
-- (the original spelled it two different ways).
kryptonite :: Skill
kryptonite = "kryptonite"

-- ---------------------------------------------------------------------------
-- Part 1: threats
-- ---------------------------------------------------------------------------

-- | Power minus three times the number of weaknesses.
potentialDamage :: Threat -> Int
potentialDamage threat = power threat - 3 * length (weaknesses threat)

-- | A threat can attack a city when its damage exceeds twice the population.
canAttack :: Threat -> City -> Bool
canAttack threat = (potentialDamage threat >) . (2 *) . population

-- | Even-length purpose: resistance must exceed half the damage.
-- Odd-length purpose: resistance must exceed the whole damage.
-- @r > d / 2@ is rewritten as @2 * r > d@ so everything stays in 'Int'.
canDefeat :: Threat -> Person -> Bool
canDefeat threat person
  | even (length (purpose threat)) = 2 * resistance person > damage
  | otherwise = resistance person > damage
  where
    damage = potentialDamage threat

-- | High level: even number of weaknesses, no kryptonite, damage above 50.
isHighLevel :: Threat -> Bool
isHighLevel threat = all ($ threat) highLevelConditions
  where
    highLevelConditions :: [Threat -> Bool]
    highLevelConditions =
      [ even . length . weaknesses
      , notElem kryptonite . weaknesses
      , (> 50) . potentialDamage
      ]

-- ---------------------------------------------------------------------------
-- Part 2: warm-up
-- ---------------------------------------------------------------------------

isWeakTo :: Threat -> Skill -> Bool
isWeakTo threat = (`elem` weaknesses threat)

-- | A threat is invulnerable for a girl when none of her skills is a weakness.
isInvulnerableFor :: Threat -> Person -> Bool
isInvulnerableFor threat = not . any (isWeakTo threat) . skills

-- | The threat with the highest power ('Nothing' for an empty list).
-- On ties the last one wins, as in the original @foldl1@.
predominantThreat :: [Threat] -> Maybe Threat
predominantThreat [] = Nothing
predominantThreat threats = Just (maximumBy (comparing power) threats)

namesOfDefeaters :: [Person] -> Threat -> [Name]
namesOfDefeaters girls threat = map name . filter (canDefeat threat) $ girls

-- | Girls whose name starts with B (case-insensitive) and have 2+ skills.
bNamedAndMultiSkilled :: [Person] -> [Person]
bNamedAndMultiSkilled = filter (\person -> startsWithB person && hasSeveralSkills person)
  where
    startsWithB :: Person -> Bool
    startsWithB = isPrefixOf "b" . map toLower . name

    hasSeveralSkills :: Person -> Bool
    hasSeveralSkills = (> 1) . length . skills

-- ---------------------------------------------------------------------------
-- Part 2: going to the nutritionist
-- ---------------------------------------------------------------------------

-- | The single place where resistance changes; every food reuses it.
modifyResistance :: (Int -> Int) -> Person -> Person
modifyResistance change person = person {resistance = change (resistance person)}

-- | Adds new friends: never herself, never someone already a friend,
-- never the same person twice.
befriend :: [Name] -> Person -> Person
befriend people person = person {friends = friends person ++ newFriends}
  where
    newFriends :: [Name]
    newFriends = filter isNew (nub people)

    isNew :: Name -> Bool
    isNew candidate = candidate /= name person && candidate `notElem` friends person

substanceX :: Food
substanceX = modifyResistance (const 0)

-- | Beer is always drunk with (potential) friends.
beer :: [Name] -> Food
beer = befriend

-- | Lowers resistance by as many points as letters the flavour has.
flavouring :: String -> Food
flavouring flavour = modifyResistance (subtract (length flavour))

barbecueChef :: Skill
barbecueChef = "Barbecue Chef"

fernet :: Food
fernet person
  | barbecueChef `elem` skills person = person
  | otherwise = person {skills = skills person ++ [barbecueChef]}

-- | Recovers 5 resistance points per friend.
gatorei :: Food
gatorei person = modifyResistance (+ 5 * length (friends person)) person

-- | Each shot removes the last letter of the name (never below "").
vodka :: Int -> Food
vodka shots person = person {name = take (length (name person) - shots) (name person)}

liquidCaramel :: Food
liquidCaramel = modifyResistance (subtract 10)

-- | Removes the first skill; @drop 1 []@ is @[]@, so no special case needed.
cocucha :: Food
cocucha person = person {skills = drop 1 (skills person)}

-- ---------------------------------------------------------------------------
-- Part 2: my favourite villain
-- ---------------------------------------------------------------------------

-- | Whatever happens, a tenth of the potential damage flees (integer
-- division) and the population never becomes negative.
rumourOfAttack :: Threat -> CityEffect
rumourOfAttack threat city =
  city {population = max 0 (population city - potentialDamage threat `div` 10)}

-- | The extra effect applies only if the villain could attack the city
-- *before* the rumour made people flee.
attack :: Threat -> CityEffect
attack threat city = extraEffect . rumourOfAttack threat $ city
  where
    extraEffect :: CityEffect
    extraEffect
      | canAttack threat city = effect threat
      | otherwise = id

noEffect :: CityEffect
noEffect = id

gangreneEffect :: CityEffect
gangreneEffect city = city {cityName = "Gangrene City", population = 2 * population city}

-- ---------------------------------------------------------------------------
-- Part 2: let's watch one more
-- ---------------------------------------------------------------------------

-- | The girl after eating every food of the episode, in the listed order.
fedGirl :: Episode -> Person
fedGirl episode = foldl (flip ($)) (girl episode) (foods episode)

-- | An episode affects a city when it is the named one and the (fed) girl
-- cannot defeat the villain.
affects :: Episode -> City -> Bool
affects episode city =
  cityName city == targetCity episode
    && not (canDefeat (villain episode) (fedGirl episode))

play :: City -> Episode -> City
play city episode
  | affects episode city = attack (villain episode) city
  | otherwise = city

-- | Each episode starts from the city as the previous one left it.
marathon :: City -> Season -> City
marathon = foldl play

episode1, episode2, episode3, episode4 :: Episode
episode1 = Episode mojoJojo blossom "Townsville" [substanceX]
episode2 = Episode princess bubbles "Townsville" [substanceX]
episode3 = Episode princess bubbles "Stardew Valley" [fernet]
episode4 = Episode gangreneGang buttercup "Pallet Town" [substanceX, cocucha]

season1 :: Season
season1 = [episode1, episode2, episode3, episode4]
