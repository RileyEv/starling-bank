module Bank.Starling.API.Core
  ( Environment(..)
  , Endpoint(..)
  , AccessToken(..)
  , Month(..)
  , (</>)
  , getWithAuth
  , getApiEndpoint
  ) where

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
import System.FilePath.Posix ((</>))
import System.Environment (getEnvironment)


data Environment = Sandbox | Production deriving Show
newtype Endpoint = Endpoint String deriving Show
newtype AccessToken = AccessToken String deriving Show

data Month = January | February | March | April
           | May | June | July | August | September
           | October | November | December
instance Show Month where
  show January   = "JANUARY"
  show February  = "FEBRUARY"
  show March     = "MARCH"
  show April     = "APRIL"
  show May       = "MAY"
  show June      = "JUNE"
  show July      = "JULY"
  show August    = "AUGUST"
  show September = "SEPTEMBER"
  show October   = "OCTOBER"
  show November  = "NOVEMBER"
  show December  = "DECEMBER"

getApiEndpoint :: Environment -> Endpoint
getApiEndpoint Sandbox    = Endpoint "https://api-sandbox.starlingbank.com"
getApiEndpoint Production = Endpoint "https://api.starlingbank.com"


getWithAuth :: FromJSON a => String -> Endpoint -> AccessToken -> IO (Maybe a)
getWithAuth uri (Endpoint endpoint) (AccessToken accessToken) = do
  let opts = defaults & auth ?~ oauth2Bearer (fromString accessToken)
  r <- asJSON =<< getWith opts (endpoint ++ uri)
  return (r ^. responseBody)
