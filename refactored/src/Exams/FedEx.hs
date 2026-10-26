-- | Practice exam "FedEx": shipments, charges and taxes.
--
-- A charge transforms a shipment (it raises its base price); a tax computes
-- an amount from a shipment. Both are plain functions, so new charges are
-- built by partial application of a couple of generic combinators.
module Exams.FedEx
  ( -- * Model (point 1)
    Price
  , Category
  , Location (..)
  , Shipment (..)
  , Charge
  , Tax
    -- * Charge combinators (point 2.a)
  , chargeWhen
  , addToPrice
  , hasCategory
  , categoryCharge
  , technologyCharge
  , overweightCharge
  , applyCharges
    -- * Local and international shipments (point 2.b)
  , isInternational
  , isLocal
  , buenosAires
  , cordoba
  , montevideo
  , localShipment
  , internationalShipment
  ) where

-- ---------------------------------------------------------------------------
-- Model
-- ---------------------------------------------------------------------------

type Price = Double

type Category = String

data Location = Location
  { city :: String
  , country :: String
  }
  deriving (Show, Eq)

data Shipment = Shipment
  { origin :: Location
  , destination :: Location
  , weightKg :: Double
  , basePrice :: Price
  , categories :: [Category]
  , taxes :: [Tax]
  }

-- | Taxes are functions, so they are shown only by count.
instance Show Shipment where
  showsPrec d shipment =
    showParen (d > 10) $
      showString "Shipment "
        . showsPrec 11 (origin shipment)
        . showChar ' '
        . showsPrec 11 (destination shipment)
        . showChar ' '
        . showsPrec 11 (weightKg shipment)
        . showChar ' '
        . showsPrec 11 (basePrice shipment)
        . showChar ' '
        . showsPrec 11 (categories shipment)
        . showString " <"
        . shows (length (taxes shipment))
        . showString " taxes>"

-- | A charge returns the same shipment with a new base price.
type Charge = Shipment -> Shipment

-- | A tax computes an amount to pay for a shipment.
type Tax = Shipment -> Price

-- ---------------------------------------------------------------------------
-- Charges
-- ---------------------------------------------------------------------------

-- | Generic conditional charge: when the condition holds, the base price is
-- transformed with the given function (the original had two near-identical
-- versions, @aplicarCargo@ and @aplicarCargo'@; this one subsumes both).
chargeWhen :: (Shipment -> Bool) -> (Shipment -> Price -> Price) -> Charge
chargeWhen condition priceChange shipment
  | condition shipment = shipment {basePrice = priceChange shipment (basePrice shipment)}
  | otherwise = shipment

-- | Adds a fixed amount to the base price.
addToPrice :: Price -> Charge
addToPrice amount = chargeWhen (const True) (const (+ amount))

hasCategory :: Category -> Shipment -> Bool
hasCategory category = elem category . categories

-- | A percentage on the base price, only for shipments of that category.
categoryCharge :: Category -> Double -> Charge
categoryCharge category percentage =
  chargeWhen (hasCategory category) (const (* (1 + percentage / 100)))

technologyCharge :: Charge
technologyCharge = categoryCharge "technology" 18

-- | 80 per kilogram above the limit.
overweightCharge :: Double -> Charge
overweightCharge limitKg =
  chargeWhen ((> limitKg) . weightKg) (\shipment -> (+ 80 * (weightKg shipment - limitKg)))

-- | Composes several charges; they are applied from left to right.
applyCharges :: [Charge] -> Charge
applyCharges charges shipment = foldl (flip ($)) shipment charges

-- ---------------------------------------------------------------------------
-- Local and international shipments
-- ---------------------------------------------------------------------------

isInternational :: Shipment -> Bool
isInternational shipment = country (origin shipment) /= country (destination shipment)

isLocal :: Shipment -> Bool
isLocal = not . isInternational

buenosAires, cordoba, montevideo :: Location
buenosAires = Location "Buenos Aires" "Argentina"
cordoba = Location "Cordoba" "Argentina"
montevideo = Location "Montevideo" "Uruguay"

localShipment :: Shipment
localShipment = Shipment buenosAires cordoba 2 220 ["music", "technology"] []

internationalShipment :: Shipment
internationalShipment = Shipment buenosAires montevideo 5 1500 ["books"] []
