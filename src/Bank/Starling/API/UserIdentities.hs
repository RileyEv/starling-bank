module Bank.Starling.API.UserIdentities
  ( module Bank.Starling.API.Schemas
  , individual
  , token
  ) where

import Bank.Starling.API.Core
  ( AccessToken
  , Endpoint
  , basicGetAuthReq
  )

    
import Bank.Starling.API.Schemas
  ( TokenIdentity(..)
  , Individual(..)
  )


-- | GET: /api/v2/identity/individual
--   Scopes: ["authorising-individual:read"]
individual :: Endpoint -> AccessToken -> IO (Maybe Individual)
individual = basicGetAuthReq "/api/v2/identity/individual"

-- | GET: /api/v2/identity/token
--   Scopes: None
token :: Endpoint -> AccessToken -> IO (Maybe TokenIdentity)
token = basicGetAuthReq "/api/v2/identity/token"

