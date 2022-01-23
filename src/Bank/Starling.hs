{-# LANGUAGE RankNTypes, GADTs, RecordWildCards, DuplicateRecordFields, NamedFieldPuns #-}
module Bank.Starling where

import           Bank.Starling.API.Core ( AccessToken, getApiEndpoint, Environment (Production), Endpoint)
import qualified Bank.Starling.API.Schemas as Schemas
import qualified Bank.Starling.API.Accounts as AccountsAPI (accounts, balance)

-- | Bank interface

data Account = forall a. (BankAccount a, Show a) => Account a
instance Show Account where
  show (Account a) = show a

data SavingPot = forall pot. (BankSavingPot pot, Show pot) => SavingPot pot
instance Show SavingPot where
  show (SavingPot pot) = show pot

data Balance = Balance
  { clearedBalance :: Schemas.SignedCurrencyAndAmount -- excl pots
  , effectiveBalance :: Schemas.SignedCurrencyAndAmount -- excl pots
  , totalClearedBalance :: Schemas.SignedCurrencyAndAmount -- incl pots
  , totalEffectiveBalance :: Schemas.SignedCurrencyAndAmount -- incl pots
  , pendingTransactions :: Schemas.SignedCurrencyAndAmount
  , acceptedOverdraft :: Schemas.SignedCurrencyAndAmount
  } deriving (Show)

data AccountDetails = AccountDetails
  {
  } deriving (Show)



class Bank b where
  getAccounts :: b -> IO (Maybe [Account])

class BankAccount a where
  getDetails :: a -> IO (Maybe ()) -- TODO
  getIdentifiers :: a -> IO (Maybe ()) -- TODO
  getAccountBalance :: a -> IO (Maybe Balance)
  getSavingsPots :: a -> IO (Maybe [SavingPot]) -- If they don't exist on the account just return empty

class BankSavingPot pot where
  getPotBalance :: pot -> IO (Maybe Balance)


-- | Starling implementation of the bank interface.

initStarling :: AccessToken -> IO Starling
initStarling token = do
  -- TODO: Get scopes allowed with access token then instance methods can check if the required scope exists
  return Starling
    { accessToken = token
    , endpoint = getApiEndpoint Production
    , scopes = Nothing
    }

initStarlingAccount :: AccessToken -> Schemas.AccountUid -> IO (Maybe StarlingAccount)
initStarlingAccount = undefined -- TODO: Needs to pull account details down,
-- Maybe also require that other values are given


data Starling = Starling
  { accessToken :: AccessToken
  , endpoint :: Endpoint
  , scopes :: Maybe [String] -- TODO: Scope type
  } deriving (Show)


data StarlingAccount = StarlingAccount
  { starling :: Starling
  , accountUid :: Schemas.AccountUid
  , accountType :: Schemas.AccountType
  , name :: String
  , currency :: Schemas.Currency
  , defaultCategory :: Schemas.CategoryUid
  } deriving (Show)


data StarlingSavingPot = StarlingSavingPot
  {
  } deriving (Show)



instance Bank Starling where
  getAccounts starling@Starling { .. } = do
    accounts <- AccountsAPI.accounts endpoint accessToken
    return $ case accounts of
      Just Schemas.Accounts { accounts } -> Just [
        Account StarlingAccount
          { starling
          , accountUid
          , accountType
          , name
          , currency
          , defaultCategory
          } | Schemas.Account { .. } <- accounts ]
      Nothing -> Nothing


instance BankAccount StarlingAccount where
  getDetails _ = return Nothing
  getIdentifiers _ = return Nothing
  getAccountBalance StarlingAccount { starling = Starling { .. }, .. } = do
    balance <- AccountsAPI.balance accountUid endpoint accessToken
    return $ case balance of
      Just Schemas.Balance { .. } -> Just Balance
        { clearedBalance
        , effectiveBalance
        , totalClearedBalance
        , totalEffectiveBalance
        , pendingTransactions
        , acceptedOverdraft
        }
      Nothing -> Nothing
  getSavingsPots _ = return (Just [])


instance BankSavingPot StarlingSavingPot where
  getPotBalance _ = return Nothing
