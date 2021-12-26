module Bank.Starling.API.Schemas where

import Data.Aeson (FromJSON)
import GHC.Generics (Generic)

data TokenIdentity = TokenIdentity
  { accountHolderUid :: String
  , expiresAt :: String
  , expiresInSeconds :: Int
  , scopes :: [String]
  , authenticated :: Bool
  , applicationUserUid :: String
  } deriving (Show, Generic)
instance FromJSON TokenIdentity

data Individual = Individual
  { title :: String
  , firstName :: String
  , lastName :: String
  , dateOfBirth :: String
  , email :: String
  , phoneNumber :: String
  } deriving (Show, Generic)
instance FromJSON Individual


-- Error
data ErrorResponse = ErrorResponse
  { errors :: ErrorDetail
  , success :: Bool
  } deriving (Show, Generic)
newtype ErrorDetail = ErrorDetail
  { message :: String
  } deriving (Show, Generic)
instance FromJSON ErrorDetail
instance FromJSON ErrorResponse
