module Bank.Starling.API.Payments
  ( standingOrders
  , standingOrder
  , standingOrderUpcomingPayments
  , paymentOrder
  , paymentOrderPayments
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
  , NextPaymentDatesResponse
  , PaymentOrderUid(..)
  , PaymentOrder
  , PaymentOrderPaymentsResponse
  , StandingOrder
  , StandingOrdersResponse
  )


-- | GET: <​/api/v2/payments/local/account/{accountUid}/category/{categoryUid}/standing-orders>
--
--   Scopes: @["standing-order:read", "standing-order-own:read"]@
standingOrders :: AccountUid -> CategoryUid -> Endpoint -> AccessToken -> IO (Maybe StandingOrdersResponse)
standingOrders (AccountUid accountUid) (CategoryUid categoryUid)
  = getWithAuth ("/api/v2/payments/local/account" </> show accountUid </> "category" </> show categoryUid </> "standing-orders")

-- | GET: <​/api/v2/payments/local/account/{accountUid}/category/{categoryUid}/standing-orders/{paymentOrderUid}>
--
--   Scopes: @["standing-order:read", "standing-order-own:read"]@
standingOrder :: AccountUid -> CategoryUid -> PaymentOrderUid -> Endpoint -> AccessToken -> IO (Maybe StandingOrder)
standingOrder (AccountUid accountUid) (CategoryUid categoryUid) (PaymentOrderUid paymentOrderUid)
  = getWithAuth ("/api/v2/payments/local/account" </> show accountUid </> "category" </> show categoryUid </> "standing-orders" </> show paymentOrderUid)

-- | GET: <​/api/v2/payments/local/account/{accountUid}/category/{categoryUid}/standing-orders/{paymentOrderUid}/upcoming-payments>
--
--   Scopes: @["standing-order:read"]@
standingOrderUpcomingPayments :: AccountUid -> CategoryUid -> PaymentOrderUid -> Endpoint -> AccessToken -> IO (Maybe NextPaymentDatesResponse)
standingOrderUpcomingPayments (AccountUid accountUid) (CategoryUid categoryUid) (PaymentOrderUid paymentOrderUid)
  = getWithAuth
     (  "/api/v2/payments/local/account"
    </> show accountUid
    </> "category"
    </> show categoryUid
    </> "standing-orders"
    </> show paymentOrderUid
    </> "upcoming-payments")

-- | GET: </api/v2/payments/local/payment-order/{paymentOrderUid}>
--
--   Scopes: @["pay-local:read"]@
paymentOrder :: PaymentOrderUid -> Endpoint -> AccessToken -> IO (Maybe PaymentOrder)
paymentOrder (PaymentOrderUid paymentOrderUid)
  = getWithAuth ("/api/v2/payments/local/payment-order" </> show paymentOrderUid)

-- | GET: </api/v2/payments/local/payment-order/{paymentOrderUid}/payments>
--
--   Scopes: @["pay-local:read"]@
paymentOrderPayments :: PaymentOrderUid -> Endpoint -> AccessToken -> IO (Maybe PaymentOrderPaymentsResponse)
paymentOrderPayments (PaymentOrderUid paymentOrderUid)
  = getWithAuth ("/api/v2/payments/local/payment-order" </> show paymentOrderUid </> "payments")
