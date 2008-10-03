{-| 
All the main data types, defined here to avoid import cycles.
-}
module Ledger.Types 
where
import Ledger.Utils
import qualified Data.Map as Map

-- | a date
type Date = String

-- | a date and time
type DateTime = String

-- | the currency of an Amount.
data Currency = Currency {
      symbol :: String,
      rate :: Double  -- ^ relative to the dollar (rates are currently hardcoded)
    } deriving (Eq,Show)

-- | some amount of money, shares, or anything else.
data Amount = Amount {
      currency :: Currency,
      quantity :: Double,
      precision :: Int           -- ^ number of significant decimal places
    } deriving (Eq)

-- | AccountNames are strings like @assets:cash:petty@, from which we derive
-- the chart of accounts
type AccountName = String

-- | a single transaction line within a ledger entry. We call it raw to
-- distinguish from the cached 'Transaction'.
data RawTransaction = RawTransaction {
      taccount :: AccountName,
      tamount :: Amount,
      tcomment :: String
    } deriving (Eq)

-- | a ledger "modifier" entry. Currently ignored.
data ModifierEntry = ModifierEntry {
      valueexpr :: String,
      m_transactions :: [RawTransaction]
    } deriving (Eq)

-- | a ledger "periodic" entry. Currently ignored.
data PeriodicEntry = PeriodicEntry {
      periodexpr :: String,
      p_transactions :: [RawTransaction]
    } deriving (Eq)

-- | a regular ledger entry, containing two or more transactions which balance
data LedgerEntry = LedgerEntry {
      edate :: Date,
      estatus :: Bool,
      ecode :: String,
      edescription :: String,
      ecomment :: String,
      etransactions :: [RawTransaction],
      epreceding_comment_lines :: String
    } deriving (Eq)

-- | a parsed ledger file. We call it raw to distinguish from the cached
-- 'Ledger'.
data RawLedger = RawLedger {
      modifier_entries :: [ModifierEntry],
      periodic_entries :: [PeriodicEntry],
      entries :: [LedgerEntry],
      final_comment_lines :: String
    } deriving (Eq)

-- | a timelog entry in a timelog file (generated by timeclock.el)
data TimeLogEntry = TimeLogEntry {
      tlcode :: Char,
      tldatetime :: DateTime,
      tlcomment :: String
    } deriving (Eq,Ord)

-- | a parsed timelog file
data TimeLog = TimeLog {
      timelog_entries :: [TimeLogEntry]
    } deriving (Eq)

-- | optimisations: these types provide some caching and are easier to work with.
-- 
-- A Transaction is a RawTransaction with some of its parent
-- LedgerEntry's data attached.
data Transaction = Transaction {
      entryno :: Int,
      date :: Date,
      description :: String,
      account :: AccountName,
      amount :: Amount
    } deriving (Eq)

-- | an Account stores an account name, all transactions in the account
-- (excluding subaccounts), and the total balance (including subaccounts).
data Account = Account {
      aname :: AccountName,
      atransactions :: [Transaction],
      abalance :: Amount
    }

-- | a raw ledger plus its tree of account names, a map from account names
-- to Accounts, and the preferred precision.
data Ledger = Ledger {
      rawledger :: RawLedger,
      accountnametree :: Tree AccountName,
      accounts :: Map.Map AccountName Account,
      lprecision :: Int
    }

