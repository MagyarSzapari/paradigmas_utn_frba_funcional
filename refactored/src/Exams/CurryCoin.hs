-- | Practice exam "CurryCoin": accounts, transactions, blocks and a blockchain.
--
-- A transaction is a pure function @Account -> Account@; a block pairs each
-- transaction with the id of the account it targets; a blockchain is an
-- ordered list of blocks. Applying them is nothing but a left fold.
module Exams.CurryCoin
  ( -- * Model (point 1.a)
    AccountId
  , Amount
  , Account (..)
  , Transaction
  , Block
  , Blockchain
    -- * Transactions (point 1.b)
  , deposit
  , payment
  , collection
  , mining
    -- * Searching accounts (point 2)
  , hasId
  , firstMatching
  , withoutFirstMatching
    -- * Updating accounts (points 3 to 6)
  , updateFirst
  , modifyAccount
  , applyBlock
  , applyBlockchain
    -- * Sample data
  , alice
  , bob
  , carol
  , sampleAccounts
  , sampleBlock
  ) where

import Data.List (find)

-- ---------------------------------------------------------------------------
-- Model
-- ---------------------------------------------------------------------------

type AccountId = String

-- | Amounts are whole units of the coin, so arithmetic stays exact.
type Amount = Int

data Account = Account
  { accountId :: AccountId
  , balance :: Amount
  }
  deriving (Show, Eq)

type Transaction = Account -> Account

type Block = [(AccountId, Transaction)]

type Blockchain = [Block]

-- ---------------------------------------------------------------------------
-- Transactions
-- ---------------------------------------------------------------------------

-- | Every transaction is a deposit of some (possibly negative) amount.
deposit :: Amount -> Transaction
deposit amount account = account {balance = balance account + amount}

payment :: Amount -> Transaction
payment = deposit . negate

collection :: Amount -> Transaction
collection = deposit

-- | Mining always rewards 25 coins.
mining :: Transaction
mining = deposit 25

-- ---------------------------------------------------------------------------
-- Searching accounts
-- ---------------------------------------------------------------------------

hasId :: AccountId -> Account -> Bool
hasId wanted = (wanted ==) . accountId

-- | Total version of @head . filter condition@.
firstMatching :: (Account -> Bool) -> [Account] -> Maybe Account
firstMatching = find

-- | Removes only the first account satisfying the condition.
withoutFirstMatching :: (Account -> Bool) -> [Account] -> [Account]
withoutFirstMatching condition accounts = before ++ drop 1 after
  where
    (before, after) = break condition accounts

-- ---------------------------------------------------------------------------
-- Updating accounts
-- ---------------------------------------------------------------------------

-- | Applies a change to the first element satisfying the condition, keeping
-- every other element (and the order) untouched.
updateFirst :: (a -> Bool) -> (a -> a) -> [a] -> [a]
updateFirst condition change xs =
  case break condition xs of
    (before, x : after) -> before ++ change x : after
    (before, []) -> before

-- | Applies a transaction to the account with the given id. Unknown ids
-- leave the accounts unchanged.
modifyAccount :: AccountId -> Transaction -> [Account] -> [Account]
modifyAccount wanted = updateFirst (hasId wanted)

-- | Runs every transaction of a block, from left to right.
applyBlock :: Block -> [Account] -> [Account]
applyBlock block accounts = foldl applyEntry accounts block
  where
    applyEntry :: [Account] -> (AccountId, Transaction) -> [Account]
    applyEntry current (target, transaction) = modifyAccount target transaction current

-- | Runs every block of the chain, from the oldest to the newest.
applyBlockchain :: Blockchain -> [Account] -> [Account]
applyBlockchain chain accounts = foldl (flip applyBlock) accounts chain

-- ---------------------------------------------------------------------------
-- Sample data
-- ---------------------------------------------------------------------------

alice, bob, carol :: Account
alice = Account "alice" 100
bob = Account "bob" 50
carol = Account "carol" 0

sampleAccounts :: [Account]
sampleAccounts = [alice, bob, carol]

-- | Alice pays 30 to Bob and Carol mines a coin reward.
sampleBlock :: Block
sampleBlock = [("alice", payment 30), ("bob", collection 30), ("carol", mining)]
