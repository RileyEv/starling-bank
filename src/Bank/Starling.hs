{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE RecordWildCards       #-}

module Bank.Starling where

import qualified Bank.Core                       as Bank
import qualified Bank.Starling.API.AccountHolder as AccountHolderAPI (accountHolder, individual)
import qualified Bank.Starling.API.Accounts      as AccountsAPI (accounts, balance, identifiers)
import           Bank.Starling.API.Core
    (AccessToken, Endpoint, Environment (Production), getApiEndpoint)
import qualified Bank.Starling.API.Schemas       as StarlingSchemas

-- | Bank interface

-- | Starling implementation of the bank interface.
initStarling :: AccessToken -> IO Starling
initStarling token = do
  -- TODO: Get scopes allowed with access token then instance methods can check if the required scope exists
  return
    Starling
      { accessToken = token,
        endpoint = getApiEndpoint Production,
        scopes = Nothing
      }

initStarlingAccount :: AccessToken -> StarlingSchemas.AccountUid -> IO (Maybe StarlingAccount)
initStarlingAccount = undefined -- TODO: Needs to pull account details down,
-- Maybe also require that other values are given

data Starling = Starling
                  { accessToken :: AccessToken
                  , endpoint    :: Endpoint
                  , scopes      :: Maybe [String] -- TODO: Scope type
                  }
  deriving (Show)

data StarlingAccount = StarlingAccount
                         { starling        :: Starling
                         , accountUid      :: StarlingSchemas.AccountUid
                         , accountType     :: StarlingSchemas.AccountType
                         , name            :: String
                         , currency        :: Bank.Currency
                         , defaultCategory :: StarlingSchemas.CategoryUid
                         }
  deriving (Show)

data StarlingSavingPot = StarlingSavingPot
                           { temp  :: String
                           , temp2 :: String
                           }
  deriving (Show)

instance Bank.Bank Starling where
  getAccounts starling@Starling {..} = do
    accounts <- AccountsAPI.accounts endpoint accessToken
    return $ case accounts of
      Just StarlingSchemas.Accounts {accounts} ->
        Just
          [ Bank.Account
              StarlingAccount
                { starling,
                  accountUid,
                  accountType,
                  name,
                  currency,
                  defaultCategory
                }
            | StarlingSchemas.Account {..} <- accounts
          ]
      Nothing -> Nothing

instance Bank.BankAccount StarlingAccount where
  getDetails StarlingAccount {starling = Starling {..}, name = accountName, ..} = do
    details <- AccountHolderAPI.accountHolder endpoint accessToken
    case details of
      Just StarlingSchemas.AccountHolder {accountHolderType} -> case accountHolderType of
        StarlingSchemas.IndividualAccount -> do
          individual <- AccountHolderAPI.individual endpoint accessToken
          return $ case individual of
            Just StarlingSchemas.Individual {..} ->
              Just
                Bank.AccountDetails
                  { holderName = firstName ++ " " ++ lastName,
                    name = accountName,
                    currency
                  }
            Nothing -> Nothing
        _ -> return Nothing
      Nothing -> return Nothing
  getIdentifiers StarlingAccount {starling = Starling {..}, ..} = do
    identifiers <- AccountsAPI.identifiers accountUid endpoint accessToken
    return $ case identifiers of
      Just StarlingSchemas.AccountIdentifiers {..} -> Just Bank.AccountIdentifiers {accountIdentifier, bankIdentifier}
      Nothing -> Nothing
  getAccountBalance StarlingAccount {starling = Starling {..}, ..} = do
    balance <- AccountsAPI.balance accountUid endpoint accessToken
    return $ case balance of
      Just StarlingSchemas.Balance {..} ->
        Just
          Bank.Balance
            { clearedBalance = signedCurAndAmountToCurAndAmount clearedBalance,
              effectiveBalance = signedCurAndAmountToCurAndAmount effectiveBalance,
              totalClearedBalance = signedCurAndAmountToCurAndAmount totalClearedBalance,
              totalEffectiveBalance = signedCurAndAmountToCurAndAmount totalEffectiveBalance,
              pendingTransactions = signedCurAndAmountToCurAndAmount pendingTransactions,
              acceptedOverdraft = signedCurAndAmountToCurAndAmount acceptedOverdraft
            }
      Nothing -> Nothing
  getSavingsPots _ = return (Just [])

instance Bank.BankSavingPot StarlingSavingPot where
  getPotBalance _ = return Nothing

signedCurAndAmountToCurAndAmount :: StarlingSchemas.SignedCurrencyAndAmount -> Bank.CurrencyAndAmount
signedCurAndAmountToCurAndAmount StarlingSchemas.SignedCurrencyAndAmount {..} = Bank.CurrencyAndAmount {..}
