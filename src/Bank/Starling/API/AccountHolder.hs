module Bank.Starling.API.AccountHolder
  ( accountHolder
  , accountHolderName
  , business
  , businessCorrespondenceAddress
  , businessRegisteredAddress
  , individual
  , joint
  , soleTrader
  ) where


import           Bank.Starling.API.Core    (AccessToken, Endpoint, getWithAuth,
                                            (</>))


import           Bank.Starling.API.Schemas (AccountHolder, AccountHolderName,
                                            Address, Business, Individual,
                                            Joint, SoleTrader)

-- * General Holder APIs

-- | GET: </api/v2/account-holder>
--
--   Scopes: @["customer:read", "account-holder-type:read"]@
accountHolder :: Endpoint -> AccessToken -> IO (Maybe AccountHolder)
accountHolder = getWithAuth "/api/v2/account-holder"

-- | GET: </api/v2/account-holder/name>
--
--   Scopes: @["account-holder-name:read"]@
accountHolderName :: Endpoint -> AccessToken -> IO (Maybe AccountHolderName)
accountHolderName = getWithAuth "/api/v2/account-holder/name"

-- * Business Holder APIs

-- | GET: </api/v2/account-holder/business>
--
--   Scopes: @["account:read"]@
business :: Endpoint -> AccessToken -> IO (Maybe Business)
business = getWithAuth "/api/v2/account-holder/business"

-- | GET: </api/v2/account-holder/business/correspondence-address>
--
--   Scopes: @["address:read"]@
businessCorrespondenceAddress :: Endpoint -> AccessToken -> IO (Maybe Address)
businessCorrespondenceAddress = getWithAuth "/api/v2/account-holder/business/correspondence-address"

-- | GET: </api/v2/account-holder/business/registered-address>
--
--   Scopes: @["address:read"]@
businessRegisteredAddress :: Endpoint -> AccessToken -> IO (Maybe Address)
businessRegisteredAddress = getWithAuth "/api/v2/account-holder/business/registered-address"

-- * Individuals & Sole-Traders Holder APIs

-- | GET: </api/v2/account-holder/individual>
--
--   Scopes: @["customer:read"]@
individual :: Endpoint -> AccessToken -> IO (Maybe Individual)
individual = getWithAuth "/api/v2/account-holder/individual"

setIndividualEmail = undefined

-- * Joint Account Holder APIs

-- | GET: </api/v2/account-holder/joint>
--
--   Scopes: @["customer:read"]@
joint :: Endpoint -> AccessToken -> IO (Maybe Joint)
joint = getWithAuth "/api/v2/account-holder/joint"

-- * Sole-Trader Holder APIs

-- | GET: </api/v2/account-holder/sole-trader>
--
--   Scopes: @["account:read"]@
soleTrader :: Endpoint -> AccessToken -> IO (Maybe SoleTrader)
soleTrader = getWithAuth "/api/v2/account-holder/sole-trader"
