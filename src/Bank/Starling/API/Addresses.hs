module Bank.Starling.API.Addresses
  ( addresses
  ) where

import           Bank.Starling.API.Core    (AccessToken, Endpoint, getWithAuth)

import           Bank.Starling.API.Schemas (Addresses)


-- | GET: <​/api​/v2​/addresses>
--
--   Scopes: @["address:read"]@
addresses :: Endpoint -> AccessToken -> IO (Maybe Addresses)
addresses = getWithAuth "/api/v2/addresses"
