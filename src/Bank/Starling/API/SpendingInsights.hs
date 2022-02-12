module Bank.Starling.API.SpendingInsights
  ( byCounterParty
  , byCountry
  , byCategory
  ) where

import           Bank.Starling.API.Core    (AccessToken, Endpoint, Month, getWithAuth, (</>))


import           Bank.Starling.API.Schemas
    (AccountUid (..), SpendingCategorySummary, SpendingCounterPartySummary, SpendingCountrySummary)


-- | GET: <​/api​/v2​/accounts​/{accountUid}​/spending-insights​/counter-party>
--
--   Scopes: @["transaction:read"]@
byCounterParty :: AccountUid -> Int -> Month -> Endpoint -> AccessToken -> IO (Maybe SpendingCounterPartySummary)
byCounterParty (AccountUid accountUid) year month
  = getWithAuth ("/api/v2/accounts" </> show accountUid </> "spending-insights/counter-party?year=" ++ show year ++ "&month=" ++ show month)

-- | GET: </api​/v2​/accounts​/{accountUid}​/spending-insights​/country>
--
--   Scopes: @["transaction:read"]@
byCountry :: AccountUid -> Int -> Month -> Endpoint -> AccessToken -> IO (Maybe SpendingCountrySummary)
byCountry (AccountUid accountUid) year month
  = getWithAuth ("/api/v2/accounts" </> show accountUid </> "spending-insights/country?year=" ++ show year ++ "&month=" ++ show month)

-- | GET: </api​/v2​/accounts​/{accountUid}​/spending-insights​/spending-category>
--
--   Scopes: @["transaction:read"]@
byCategory :: AccountUid -> Int -> Month -> Endpoint -> AccessToken -> IO (Maybe SpendingCategorySummary)
byCategory (AccountUid accountUid) year month
  = getWithAuth ("/api/v2/accounts" </> show accountUid </> "spending-insights/spending-category?year=" ++ show year ++ "&month=" ++ show month)
