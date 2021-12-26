{-# LANGUAGE LambdaCase, OverloadedStrings, DeriveAnyClass, DuplicateRecordFields #-}
module Bank.Starling.API.Schemas where

import Data.Aeson (FromJSON (..), ToJSON(..), withText)
import GHC.Generics (Generic)

data Account = Account
  { accountUid      :: AccountUid
  , accountType     :: AccountType
  , defaultCategory :: String
  , currency        :: Currency
  , createdAt       :: String
  , name            :: String
  } deriving (Show, Generic, ToJSON, FromJSON)

data AccountIdentifier = AccountIdentifier
  { identifierType    :: IdentifierType
  , bankIdentifier    :: String
  , accountIdentifier :: String
  } deriving (Show, Generic, ToJSON, FromJSON)

data AccountIdentifiers = AccountIdentifiers
  { accountIdentifier  :: String
  , bankIdentifier     :: String
  , iban               :: String
  , bic                :: String
  , accountIdentifiers :: [AccountIdentifier]
  } deriving (Show, Generic, ToJSON, FromJSON)

newtype Accounts = Accounts
  { accounts :: [Account]
  } deriving (Show, Generic, ToJSON, FromJSON)

data AccountStatementPeriod = AccountStatementPeriod
  { period  :: String
  , partial :: Bool
  , endsAt  :: Maybe String
  } deriving (Show, Generic, ToJSON, FromJSON)

newtype AccountStatementPeriods = AccountStatementPeriods
  { periods :: [AccountStatementPeriod]
  } deriving (Show, Generic, ToJSON, FromJSON)

data AccountType = Primary | Additional | FixedTermDeposit | Loan deriving (Show, Generic)
instance FromJSON AccountType where
  parseJSON = withText "accountType" $ \case
      "PRIMARY"            -> return Primary
      "ADDITIONAL"         -> return Additional
      "FIXED_TERM_DEPOSIT" -> return FixedTermDeposit
      "LOAN"               -> return Loan
      _                    -> fail "string is not one of known enum values"
instance ToJSON AccountType where
  toJSON Primary          = "PRIMARY"
  toJSON Additional       = "ADDITIONAL"
  toJSON FixedTermDeposit = "FIXED_TERM_DEPOSIT"
  toJSON Loan             = "LOAN"

newtype AccountUid = AccountUid String deriving (Show, Generic, ToJSON, FromJSON)
newtype AccountHolderUid = AccountHolderUid String deriving (Show, Generic, ToJSON, FromJSON)

data Balance = Balance
  { clearedBalance        :: SignedCurrencyAndAmount
  , effectiveBalance      :: SignedCurrencyAndAmount
  , pendingTransactions   :: SignedCurrencyAndAmount
  , acceptedOverdraft     :: SignedCurrencyAndAmount
  , amount                :: SignedCurrencyAndAmount
  , totalClearedBalance   :: SignedCurrencyAndAmount
  , totalEffectiveBalance :: SignedCurrencyAndAmount
  } deriving (Show, Generic, ToJSON, FromJSON)

data ConfirmationOfFundsResponse = ConfirmationOfFundsResponse
  { requestedAmountAvailableToSpend :: Bool
  , accountWouldBeInOverdraftIfRequestedAmountSpent :: Bool
  } deriving(Show, Generic, ToJSON, FromJSON)

data Currency = UNDEFINED
  | AED | AFN | ALL | AMD | ANG | AOA | ARS | AUD | AWG | AZN
  | BAM | BBD | BDT | BGN | BHD | BIF | BMD | BND | BOB | BOV
  | BRL | BSD | BTN | BWP | BYN | BYR | BZD | CAD | CDF | CHE
  | CHF | CHW | CLF | CLP | CNY | COP | COU | CRC | CUC | CUP
  | CVE | CZK | DJF | DKK | DOP | DZD | EGP | ERN | ETB | EUR
  | FJD | FKP | GBP | GEL | GHS | GIP | GMD | GNF | GTQ | GYD
  | HKD | HNL | HRK | HTG | HUF | IDR | ILS | INR | IQD | IRR
  | ISK | JMD | JOD | JPY | KES | KGS | KHR | KMF | KPW | KRW
  | KWD | KYD | KZT | LAK | LBP | LKR | LRD | LSL | LTL | LYD
  | MAD | MDL | MGA | MKD | MMK | MNT | MOP | MRO | MRU | MUR
  | MVR | MWK | MXN | MXV | MYR | MZN | NAD | NGN | NIO | NOK
  | NPR | NZD | OMR | PAB | PEN | PGK | PHP | PKR | PLN | PYG
  | QAR | RON | RSD | RUB | RUR | RWF | SAR | SBD | SCR | SDG
  | SEK | SGD | SHP | SLL | SOS | SRD | SSP | STD | STN | SVC
  | SYP | SZL | THB | TJS | TMT | TND | TOP | TRY | TTD | TWD
  | TZS | UAH | UGX | USD | USN | USS | UYI | UYU | UZS | VEF
  | VES | VND | VUV | WST | XAF | XAG | XAU | XBA | XBB | XBC
  | XBD | XCD | XDR | XOF | XPD | XPF | XPT | XSU | XTS | XUA
  | XXX | YER | ZAR | ZMW | ZWL deriving (Show, Generic, ToJSON, FromJSON)

data IdentifierType = SortCode | IbanBic | AbaAch deriving (Show, Generic)
instance FromJSON IdentifierType where
  parseJSON = withText "accountType" $ \case
      "SORT_CODE" -> return SortCode
      "IBAN_BIC"  -> return IbanBic
      "ABA_ACH"   -> return AbaAch
      _           -> fail "string is not one of known enum values"
instance ToJSON IdentifierType where
  toJSON SortCode = "SORT_CODE"
  toJSON IbanBic  = "IBAN_BIC"
  toJSON AbaAch   = "ABA_ACH"

data Individual = Individual
  { title       :: String
  , firstName   :: String
  , lastName    :: String
  , dateOfBirth :: String
  , email       :: String
  , phoneNumber :: String
  } deriving (Show, Generic, ToJSON, FromJSON)

data SignedCurrencyAndAmount = SignedCurrencyAndAmount
  { currency   :: Currency
  , minorUnits :: Int
  } deriving (Show, Generic, ToJSON, FromJSON)

data TokenIdentity = TokenIdentity
  { accountHolderUid   :: AccountHolderUid
  , expiresAt          :: String
  , expiresInSeconds   :: Int
  , scopes             :: [String]
  , authenticated      :: Bool
  , applicationUserUid :: String
  } deriving (Show, Generic, ToJSON, FromJSON)


-- Error
data ErrorResponse = ErrorResponse
  { errors  :: ErrorDetail
  , success :: Bool
  } deriving (Show, Generic, ToJSON, FromJSON)

newtype ErrorDetail = ErrorDetail
  { message :: String
  } deriving (Show, Generic, ToJSON, FromJSON)
