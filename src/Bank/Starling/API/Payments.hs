module Bank.Starling.API.Payments
  ( standingOrders
  ) where

import Bank.Starling.API.Core
  ( AccessToken
  , Endpoint
  , getWithAuth
  , (</>)
  )

import Bank.Starling.API.Schemas
  ( AccountUid(..)
  , CategoryUid(..)
  , PaymentOrderUid(..)
  , PaymentOrder
  , PaymentOrderPaymentsResponse
  , StandingOrder
  , StandingOrdersResponse
  )


-- | GET: @​/api/v2/payments/local/account/{accountUid}/category/{categoryUid}/standing-orders@
--   Scopes: @["standing-order:read", "standing-order-own:read"]@
standingOrders :: AccountUid -> CategoryUid -> Endpoint -> AccessToken -> IO (Maybe StandingOrdersResponse)
standingOrders (AccountUid accountUid) (CategoryUid categoryUid)
  = getWithAuth ("/api/v2/payments/local/account" </> show accountUid </> "category" </> show categoryUid </> "standing-orders")

-- | GET: @​/api/v2/payments/local/account/{accountUid}/category/{categoryUid}/standing-orders/{paymentOrderUid}@
--   Scopes: @["standing-order:read", "standing-order-own:read"]@
standingOrder :: AccountUid -> CategoryUid -> PaymentOrderUid -> Endpoint -> AccessToken -> IO (Maybe StandingOrder)
standingOrder (AccountUid accountUid) (CategoryUid categoryUid) (PaymentOrderUid paymentOrderUid)
  = getWithAuth ("/api/v2/payments/local/account" </> show accountUid </> "category" </> show categoryUid </> "standing-orders" </> show paymentOrderUid)

-- | GET: @​/api/v2/payments/local/account/{accountUid}/category/{categoryUid}/standing-orders/{paymentOrderUid}/upcoming-payments@
--   Scopes: @["standing-order:read"]@
standingOrderUpcomingPayments :: AccountUid -> CategoryUid -> PaymentOrderUid -> Endpoint -> AccessToken -> IO (Maybe NextPaymentsDateResponse)
standingOrderUpcomingPayments (AccountUid accountUid) (CategoryUid categoryUid) (PaymentOrderUid paymentOrderUid)
  = getWithAuth
     (  "/api/v2/payments/local/account"
    </> show accountUid
    </> "category"
    </> show categoryUid
    </> "standing-orders"
    </> show paymentOrderUid
    </> "upcoming-payments")

-- | GET: @​/api/v2/payments/local/payment-order/{paymentOrderUid}@
--   Scopes: @["pay-local:read"]@
paymentOrder :: PaymentOrderUid -> Endpoint -> AccessToken -> IO (Maybe PaymentOrder)
paymentOrder (PaymentOrderUid paymentOrderUid)
  = getWithAuth ("/api/v2/payments/local/payment-order" </> show paymentOrderUid)

-- | GET: @​/api/v2/payments/local/payment-order/{paymentOrderUid}/payments@
--   Scopes: @["pay-local:read"]@
paymentOrderPayments :: PaymentOrderUid -> Endpoint -> AccessToken -> IO (Maybe PaymentOrderPaymentsResponse)
paymentOrderPayments (PaymentOrderUid paymentOrderUid)
  = getWithAuth ("/api/v2/payments/local/payment-order" </> show paymentOrderUid </> "payments")

-- | GET: @​/api​/v2​/direct-debits/mandates/account/{accountUid}@
--   Scopes: @["mandate:read"]@
-- mandatesByAccount :: AccountUid -> Endpoint -> AccessToken -> IO (Maybe DirectDebitMandates)
-- mandatesByAccount (AccountUid accountUid)
--   = getWithAuth ("/api/v2/direct-debit/mandates/account" </> accountUid)

-- | GET: @​/api​/v2​/direct-debits/mandates/{mandateUid}@
--   Scopes: @["mandate:read"]@
-- mandate :: DirectDebitMandateUid -> Endpoint -> AccessToken -> IO (Maybe DirectDebitMandate)
-- mandate (DirectDebitMandateUid directDebitMandateUid)
--   = getWithAuth ("/api/v2/direct-debit/mandates" </> directDebitMandateUid)

-- | GET: @​/api​/v2​/direct-debits/mandates/{mandateUid}@
--   Scopes: @["mandate:read"]@
-- mandateTransactionHistory :: DirectDebitMandateUid -> Endpoint -> AccessToken -> IO (Maybe DirectDebitMandate)
-- mandateTransactionHistory (DirectDebitMandateUid directDebitMandateUid)
--   = getWithAuth ("/api/v2/direct-debit/mandates" </> directDebitMandateUid </> "payments")
