module Bank.Starling where

import Bank.Core.Bank
  ( Bank(..)
  , HolderDetails(HolderDetails)
  )
import Bank.Starling.API.Core
  ( AccessToken
  , Environment
  , getApiEndpoint
  )
import qualified Bank.Starling.API.UserIdentities as UserIdentities
  ( token
  )
import qualified Bank.Starling.API.Schemas as Schemas

requiredScopes :: [String]
requiredScopes = ["account-holder-type:read"]

createStarlingClient :: Environment -> AccessToken -> IO StarlingBank
createStarlingClient env accessToken = do
  let endpoint = getApiEndpoint env
  (Just tokenIdentity) <- UserIdentities.token endpoint accessToken
  -- TODO: verify required scopes exist
  verifyScopes requiredScopes (Schemas.scopes tokenIdentity)
  accountType <- getAccountHolder endpoint accessToken
  return (StarlingBank env accessToken)


verifyScopes :: [String] -> [String] -> IO Bool
verifyScopes required allowed = undefined

getAccountHolder = undefined


data StarlingBank = StarlingBank
  { sandbox :: Environment
  , accessToken :: AccessToken
  }

instance Bank StarlingBank where
  holderDetails starling = do
    return (HolderDetails "test" "155" "5th Sep")
  getAccounts starling = do
    return []


