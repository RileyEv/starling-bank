module Bank.Starling.API.DirectDebit
  ( mandates
  , mandatesByAccount
  , mandate
  , mandateTransactionHistory
  ) where

import Bank.Starling.API.Core
  ( AccessToken
  , Endpoint
  , getWithAuth
  , (</>)
  )

import Bank.Starling.API.Schemas
  ( AccountUid(..)
  , DirectDebitMandate
  , DirectDebitMandates
  , DirectDebitMandateUid(..)
  )


-- | GET: <​/api​/v2​/direct-debits/mandates>
--
--   Scopes: @["mandate:read"]@
mandates :: Endpoint -> AccessToken -> IO (Maybe DirectDebitMandates)
mandates = getWithAuth "/api/v2/direct-debit/mandates"

-- | GET: </api​/v2​/direct-debits/mandates/account/{accountUid}>
--
--   Scopes: @["mandate:read"]@
mandatesByAccount :: AccountUid -> Endpoint -> AccessToken -> IO (Maybe DirectDebitMandates)
mandatesByAccount (AccountUid accountUid)
  = getWithAuth ("/api/v2/direct-debit/mandates/account" </> show accountUid)

-- | GET: </api​/v2​/direct-debits/mandates/{mandateUid}>
--
--   Scopes: @["mandate:read"]@
mandate :: DirectDebitMandateUid -> Endpoint -> AccessToken -> IO (Maybe DirectDebitMandate)
mandate (DirectDebitMandateUid directDebitMandateUid)
  = getWithAuth ("/api/v2/direct-debit/mandates" </> show directDebitMandateUid)

-- | GET: </api​/v2​/direct-debits/mandates/{mandateUid}>
--
--   Scopes: @["mandate:read"]@
mandateTransactionHistory :: DirectDebitMandateUid -> Endpoint -> AccessToken -> IO (Maybe DirectDebitMandate)
mandateTransactionHistory (DirectDebitMandateUid directDebitMandateUid)
  = getWithAuth ("/api/v2/direct-debit/mandates" </> show directDebitMandateUid </> "payments")
