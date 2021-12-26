module Bank.Starling.API.Core where

import Control.Lens
  ( (&)
  , (^.)
  , (?~)
  )
import Data.Aeson (FromJSON)
import Data.String (fromString)
import Network.Wreq
  ( asJSON
  , getWith
  , oauth2Bearer
  , defaults
  , auth
  , responseBody
  )


data Environment = Sandbox | Production
newtype Endpoint = Endpoint String
newtype AccessToken = AccessToken String

getApiEndpoint :: Environment -> Endpoint
getApiEndpoint Sandbox    = Endpoint "https://api-sandbox.starlingbank.com"
getApiEndpoint Production = Endpoint "https://api.starlingbank.com"


basicGetAuthReq :: FromJSON a => String -> Endpoint -> AccessToken -> IO (Maybe a)
basicGetAuthReq uri (Endpoint endpoint) (AccessToken accessToken) = do
  let opts = defaults & auth ?~ oauth2Bearer (fromString accessToken)
  r <- asJSON =<< getWith opts (endpoint ++ uri)
  return (r ^. responseBody)
