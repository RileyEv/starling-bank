module Bank.Starling.API.Accounts
  ( accounts
  , statementPeriods
  , statementDownload
  , statementDownloadDateRange
  , balance
  , identifiers
  , confirmationOfFunds
  ) where

import           Bank.Starling.API.Core    (AccessToken, Endpoint, getWithAuth,
                                            (</>))


import           Bank.Starling.API.Schemas (AccountIdentifiers,
                                            AccountStatementPeriods,
                                            AccountUid (..), Accounts, Balance,
                                            ConfirmationOfFundsResponse)


-- | GET: </api/v2/accounts>
--
--   Scopes: @["account:read", "account-list:read"]@
accounts :: Endpoint -> AccessToken -> IO (Maybe Accounts)
accounts = getWithAuth "/api/v2/accounts"

-- | GET: </api/v2/accounts/{accountUid}/statement/available-periods>
--
--   Scopes: @["statement-csv:read", "statement-pdf:read"]@
statementPeriods :: AccountUid -> Endpoint -> AccessToken -> IO (Maybe AccountStatementPeriods)
statementPeriods (AccountUid accountUid) = getWithAuth ("/api/v2/accounts" </> show accountUid </> "statement/available-periods")

-- | GET: </api​/v2​/accounts​/{accountUid}​/statement​/download>
--
--   Scopes: @["statement-csv:read", "statement-pdf:read"]@
statementDownload :: AccountUid -> String -> FilePath -> Endpoint -> AccessToken -> IO ()
statementDownload (AccountUid accountUid) yearMonth path = error "Not implemented yet"

-- | GET: </api/v2/accounts/{accountUid}/statement/downloadForDateRange>
--
--   Scopes: @["statement-csv:read", "statement-pdf:read"]@
statementDownloadDateRange :: AccountUid -> String -> String -> FilePath -> Endpoint -> AccessToken -> IO ()
statementDownloadDateRange (AccountUid accountUid) startDate endDate path = error "Not implemented yet"

-- | GET: </api/v2/accounts/{accountUid}/balance>
--
--   Scopes: @["balance:read"]@
balance :: AccountUid -> Endpoint -> AccessToken -> IO (Maybe Balance)
balance (AccountUid accountUid) = getWithAuth ("/api/v2/accounts" </> show accountUid </> "balance")

-- | GET: </api/v2/accounts/{accountUid}/identifiers>
--
--   Scopes: @["account-identifier:read"]@
identifiers :: AccountUid -> Endpoint -> AccessToken -> IO (Maybe AccountIdentifiers)
identifiers (AccountUid accountUid) = getWithAuth ("/api/v2/accounts" </> show accountUid </> "identifiers")

-- | GET: </api/v2/accounts/{accountUid}/confirmation-of-funds>
--
--   Scopes: @["confirmation-of-funds:read"]@
confirmationOfFunds :: AccountUid -> Int -> Endpoint -> AccessToken -> IO (Maybe ConfirmationOfFundsResponse)
confirmationOfFunds (AccountUid accountUid) targetAmountInMinorUnits
  = getWithAuth ("/api/v2/accounts" </> show accountUid </> "confirmation-of-funds" ++ "?targetAmountInMinorUnits=" ++ show targetAmountInMinorUnits)
