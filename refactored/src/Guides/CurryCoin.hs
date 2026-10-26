-- | Practice assignment "CurryCoin": accounts, transactions and blocks.
module Guides.CurryCoin
  ( AccountId
  , Account (..)
  , Transaction
  , Block
  , Blockchain
  , transaction
  , payment
  , collection
  , mining
  , hasId
  , firstMatching
  , withoutFirstMatching
  , updateAccount
  , runBlock
  , areStable
  , isStableBlockchain
  , shamelessFunction
  ) where

import Data.List (find, foldl')

type AccountId = String

-- | 1.a
data Account = Account
  { accountId :: AccountId
  , balance :: Double
  }
  deriving (Show, Eq)

-- | A transaction is just a function between account states.
type Transaction = Account -> Account

-- | A block pairs each transaction with the account it applies to.
type Block = [(AccountId, Transaction)]

type Blockchain = [Block]

-- | 1.b: add an amount to the balance.
transaction :: Double -> Transaction
transaction amount account = account {balance = balance account + amount}

-- | 1.b
payment :: Double -> Transaction
payment = transaction . negate

-- | 1.b
collection :: Double -> Transaction
collection = transaction

-- | 1.b: mining rewards 25 coins.
mining :: Transaction
mining = transaction 25

-- | 2.a
hasId :: AccountId -> Account -> Bool
hasId identifier = (== identifier) . accountId

-- | 2.b: total version of @head . filter@.
firstMatching :: (Account -> Bool) -> [Account] -> Maybe Account
firstMatching = find

-- | 2.c: the list without its first matching element.
-- 'break' splits the list once, instead of the original
-- @takeWhile@ + @dropWhile@ + equality comparison.
withoutFirstMatching :: (a -> Bool) -> [a] -> [a]
withoutFirstMatching p xs =
  case break p xs of
    (before, _ : after) -> before ++ after
    (before, []) -> before

-- | 3: apply a transaction to the account with the given id.
--
-- Unlike the original, the account keeps its position in the list (it
-- was moved to the front) and a missing id leaves the list unchanged
-- (it crashed on @head []@).
updateAccount :: AccountId -> [Account] -> Transaction -> [Account]
updateAccount identifier accounts apply =
  case break (hasId identifier) accounts of
    (before, account : after) -> before ++ apply account : after
    (_, []) -> accounts

-- | 4: run every transaction of the block, in order.
runBlock :: Block -> [Account] -> [Account]
runBlock block accounts = foldl' step accounts block
  where
    step current (identifier, apply) = updateAccount identifier current apply

-- | 5: no account is in the red.
areStable :: [Account] -> Bool
areStable = all ((>= 0) . balance)

-- | 6: every intermediate state (after each block) is stable.
--
-- The original evaluated every block against the /initial/ accounts, so
-- the effect of previous blocks was lost. 'scanl' builds the lazy list of
-- successive states and 'all' stops at the first unstable one.
isStableBlockchain :: Blockchain -> [Account] -> Bool
isStableBlockchain chain accounts =
  all areStable (drop 1 (scanl (flip runBlock) accounts chain))

-- | 7: type-inference exercise. If more than 10 images of the first list
-- under @f@ are even, return 'id'; otherwise return @g@.
-- An empty outer list counts as "no evens" instead of crashing on 'head'.
shamelessFunction :: Integral n => [[a]] -> (a -> n, b -> b) -> b -> b
shamelessFunction xss (f, g)
  | evens > 10 = id
  | otherwise = g
  where
    evens = length (filter even (map f (concat (take 1 xss))))
