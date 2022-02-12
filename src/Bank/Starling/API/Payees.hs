module Bank.Starling.API.Payees
  ( payees
  , scheduledPayments
  , payments
  ) where

import           Bank.Starling.API.Core    (AccessToken, Endpoint, getWithAuth,
                                            (</>))

import           Bank.Starling.API.Schemas (PayeeAccountUid (..), PayeeUid (..),
                                            Payees, Payments, ScheduledPayments)


-- | GET: <​/api​/v2​/payees>
--
--   Scopes: @["payee:read"]@
payees :: Endpoint -> AccessToken -> IO (Maybe Payees)
payees = getWithAuth "/api/v2/payees"

-- | GET: </api/v2/payees/{payeeUid}/account/{accountUid}/scheduled-payments>
--
--   Scopes: @["scheduled-payment:read"]@
scheduledPayments :: PayeeUid -> PayeeAccountUid -> Endpoint -> AccessToken -> IO (Maybe ScheduledPayments)
scheduledPayments (PayeeUid payeeUid) (PayeeAccountUid payeeAccountUid)
  = getWithAuth ("/api/v2/payees" </> show payeeUid </> "account" </> show payeeAccountUid </> "scheduled-payments")

-- | GET: </api/v2/payees/{payeeUid}/account/{accountUid}/payments>
--
--   Scopes: @["payee-transaction:read"]@
payments :: PayeeUid -> PayeeAccountUid -> Endpoint -> AccessToken -> IO (Maybe Payments)
payments (PayeeUid payeeUid) (PayeeAccountUid payeeAccountUid)
  = getWithAuth ("/api/v2/payees" </> show payeeUid </> "account" </> show payeeAccountUid </> "payments")
