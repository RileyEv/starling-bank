{-# LANGUAGE LambdaCase, OverloadedStrings, DeriveAnyClass, DuplicateRecordFields #-}
module Bank.Starling.API.Schemas where

import Data.Aeson (FromJSON (..), ToJSON(..), withText)
import Data.Time.LocalTime (ZonedTime)
import Data.Time.Calendar (Day)
import Data.UUID (UUID)

import GHC.Generics (Generic)

data Account = Account
  { accountUid      :: AccountUid
  , accountType     :: AccountType
  , defaultCategory :: CategoryUid
  , currency        :: Currency
  , createdAt       :: ZonedTime
  , name            :: String
  } deriving (Show, Generic, ToJSON, FromJSON)

data AccountHolder = AccountHolder
  { accoundHolderUid  :: AccountHolderUid
  , accountHolderType :: AccountHolderType
  } deriving (Show, Generic, ToJSON, FromJSON)

newtype AccountHolderName = AccountHolderName
  { accountHolderName :: String
  } deriving (Show, Generic, ToJSON, FromJSON)

data AccountHolderType = IndividualAccount | BusinessAccount | SoleTraderAccount | JointAccount | BankingAsAService deriving (Show, Generic)
instance FromJSON AccountHolderType where
  parseJSON = withText "accountType" $ \case
      "INDIVIDUAL"           -> return IndividualAccount
      "BUSINESS"             -> return BusinessAccount
      "SOLE_TRADER"          -> return SoleTraderAccount
      "JOINT"                -> return JointAccount
      "BANKING_AS_A_SERVICE" -> return BankingAsAService
      _                      -> fail "string is not one of known enum values"
instance ToJSON AccountHolderType where
  toJSON IndividualAccount = "INDIVIDUAL"
  toJSON BusinessAccount   = "BUSINESS"
  toJSON SoleTraderAccount = "SOLE_TRADER"
  toJSON JointAccount      = "JOINT"
  toJSON BankingAsAService = "BANKING_AS_A_SERVICE"

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
  , endsAt  :: Maybe ZonedTime
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

newtype AccountUid = AccountUid UUID deriving (Show, Generic, ToJSON, FromJSON)
newtype AccountHolderUid = AccountHolderUid UUID deriving (Show, Generic, ToJSON, FromJSON)

data Address = Address
  { line1       :: String
  , line2       :: Maybe String
  , line3       :: Maybe String
  , postTown    :: String
  , postCode    :: String
  , countryCode :: String
  } deriving (Show, Generic, ToJSON, FromJSON)

data Addresses = Addresses
  { current :: Address
  , previous :: [Address]
  } deriving (Show, Generic, ToJSON, FromJSON)

data Balance = Balance
  { clearedBalance        :: SignedCurrencyAndAmount
  , effectiveBalance      :: SignedCurrencyAndAmount
  , pendingTransactions   :: SignedCurrencyAndAmount
  , acceptedOverdraft     :: SignedCurrencyAndAmount
  , amount                :: SignedCurrencyAndAmount
  , totalClearedBalance   :: SignedCurrencyAndAmount
  , totalEffectiveBalance :: SignedCurrencyAndAmount
  } deriving (Show, Generic, ToJSON, FromJSON)

data BankIdentifierType = SORT_CODE | SWIFT | IBAN | ABA | ABA_WIRE | ABA_ACH deriving (Show, Generic, ToJSON, FromJSON)

data Business = Business
  { companyName               :: String
  , companyType               :: String
  , companyCategory           :: String
  , companySubCategory        :: String
  , companyRegistrationNumber :: String
  , email                     :: String
  , phone                     :: String
  } deriving (Show, Generic, ToJSON, FromJSON)

newtype CategoryUid = CategoryUid UUID deriving (Show, Generic, ToJSON, FromJSON)

data ConfirmationOfFundsResponse = ConfirmationOfFundsResponse
  { requestedAmountAvailableToSpend :: Bool
  , accountWouldBeInOverdraftIfRequestedAmountSpent :: Bool
  } deriving(Show, Generic, ToJSON, FromJSON)

data CounterPartyType = Category | Cheque | Customer | PayeeCounterParty | Merchant
                      | Sender | Starling | LoanCounterParty deriving (Show, Generic)
instance FromJSON CounterPartyType where
  parseJSON = withText "accountType" $ \case
      "CATEGORY" -> return Category
      "CHEQUE"   -> return Cheque
      "CUSTOMER" -> return Customer
      "PAYEE"    -> return PayeeCounterParty
      "MERCHANT" -> return Merchant
      "SENDER"   -> return Sender
      "STARLING" -> return Starling
      "LOAN"     -> return LoanCounterParty
      _          -> fail "string is not one of known enum values"
instance ToJSON CounterPartyType where
  toJSON Category          = "CATEGORY"
  toJSON Cheque            = "CHEQUE"
  toJSON Customer          = "CUSTOMER"
  toJSON PayeeCounterParty = "PAYEE"
  toJSON Merchant          = "MERCHANT"
  toJSON Sender            = "SENDER"
  toJSON Starling          = "STARLING"
  toJSON LoanCounterParty  = "LOAN"

data CountryCode
  = AC | AD | AE | AF | AG | AI | AL | AM | AN | AO | AQ | AR
  | AS | AT | AU | AW | AX | AZ | BA | BB | BD | BE | BF | BG
  | BH | BI | BJ | BL | BM | BN | BO | BQ | BR | BS | BT | BU
  | BV | BW | BY | BZ | CA | CC | CD | CF | CG | CH | CI | CK
  | CL | CM | CN | CO | CP | CR | CS | CU | CV | CW | CX | CY
  | CZ | DE | DG | DJ | DK | DM | DO | DZ | EA | EC | EE | EG
  | EH | ER | ES | ET | EU | EZ | FI | FJ | FK | FM | FO | FR
  | FX | GA | GB | GD | GE | GF | GG | GH | GI | GL | GM | GN
  | GP | GQ | GR | GS | GT | GU | GW | GY | HK | HM | HN | HR
  | HT | HU | IC | ID | IE | IL | IM | IN | IO | IQ | IR | IS
  | IT | JE | JM | JO | JP | KE | KG | KH | KI | KM | KN | KP
  | KR | KW | KY | KZ | LA | LB | LC | LI | LK | LR | LS | LT
  | LU | LV | LY | MA | MC | MD | ME | MF | MG | MH | MK | ML
  | MM | MN | MO | MP | MQ | MR | MS | MT | MU | MV | MW | MX
  | MY | MZ | NA | NC | NE | NF | NG | NI | NL | NO | NP | NR
  | NT | NU | NZ | OM | PA | PE | PF | PG | PH | PK | PL | PM
  | PN | PR | PS | PT | PW | PY | QA | RE | RO | RS | RU | RW
  | SA | SB | SC | SD | SE | SF | SG | SH | SI | SJ | SK | SL
  | SM | SN | SO | SR | SS | ST | SU | SV | SX | SY | SZ | TA
  | TC | TD | TF | TG | TH | TJ | TK | TL | TM | TN | TO | TP
  | TR | TT | TV | TW | TZ | UA | UG | UK | UM | US | UY | UZ
  | VA | VC | VE | VG | VI | VN | VU | WF | WS | XI | XU | XK
  | YE | YT | YU | ZA | ZM | ZR | ZW deriving (Show, Generic, ToJSON, FromJSON)

data Currency
  = AED | AFN | ALL | AMD | ANG | AOA | ARS | AUD | AWG | AZN
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

data CurrencyAndAmount = CurrencyAndAmount
  { currency :: Currency
  , minorUnits :: Int
  } deriving (Show, Generic, ToJSON, FromJSON)

data DirectDebitMandate = DirectDebitMandate
  { uid            :: DirectDebitMandateUid
  , reference      :: String
  , status         :: String -- enum ?
  , source         :: String -- enum ?
  , created        :: ZonedTime
  , cancelled      :: Maybe ZonedTime
  , nextDate       :: Maybe Day
  , lastDate       :: Maybe Day
  , originatorName :: String
  , originatorUid  :: UUID
  , merchantUid    :: UUID
  , lastPayment    :: Maybe LastPayment
  , accountUid     :: AccountUid
  , categoryUid    :: CategoryUid
  } deriving (Show, Generic, ToJSON, FromJSON)

data DirectDebitMandates = DirectDebitMandates
  { mandates :: [DirectDebitMandate]
  } deriving (Show, Generic, ToJSON, FromJSON)

newtype DirectDebitMandateUid = DirectDebitMandateUid UUID deriving (Show, Generic, ToJSON, FromJSON)

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
  , dateOfBirth :: Day
  , email       :: String
  , phone       :: String
  } deriving (Show, Generic, ToJSON, FromJSON)

data Joint = Joint
  { accountHolderUid :: AccountHolderUid
  , personOne :: Individual
  , personTwo :: Individual
  } deriving (Show, Generic, ToJSON, FromJSON)

data LastPayment = LastPayment
  { lastDate   :: Day
  , lastAmount :: CurrencyAndAmount
  } deriving (Show, Generic, ToJSON, FromJSON)

data NextPaymentDatesResponse = NextPaymentDatesResponse
  { nextPaymentDates :: [Day]
  } deriving (Show, Generic, ToJSON, FromJSON)

data Payee = Payee
  { payeeUid :: PayeeUid
  , payeeName :: String
  , phoneNumber :: String
  , payeeType :: PayeeType
  , firstName :: Maybe String
  , middleName :: Maybe String
  , lastName :: Maybe String
  , businessName :: Maybe String
  , dateOfBirth :: Maybe Day
  , accounts :: [PayeeAccount]
  } deriving (Show, Generic, ToJSON, FromJSON)

data PayeeAccount = PayeeAccount
  { payeeAccountUid :: PayeeAccountUid
  , payeeChannelType :: PayeeChannelType
  , description :: String
  , defaultAccount :: Bool
  , countryCode :: CountryCode
  , accountIdentifier :: String
  , bankIdentifier :: String
  , bankIdentifierType :: BankIdentifierType
  , lastReferences :: [String]
  } deriving (Show, Generic, ToJSON, FromJSON)

newtype PayeeAccountUid = PayeeAccountUid UUID deriving (Show, Generic, ToJSON, FromJSON)

data PayeeChannelType = BANK_ACCOUNT | SETTLE_UP | NEARBY deriving (Show, Generic, ToJSON, FromJSON)

data PayeePayment = PayeePayment
  { paymentUid :: PaymentUid
  , amount :: CurrencyAndAmount
  , reference :: String
  , createdAt :: ZonedTime
  , spendingCategory :: SpendingCategory
  , paymentAmount :: CurrencyAndAmount
  } deriving (Show, Generic, ToJSON, FromJSON)

data Payees = Payees
  { payees :: [Payee]
  } deriving (Show, Generic, ToJSON, FromJSON)

data PayeeType = BUSINESS | INDIVIDUAL deriving (Show, Generic, ToJSON, FromJSON)

newtype PayeeUid = PayeeUid UUID deriving (Show, Generic, ToJSON, FromJSON)


data Payments = Payments
  { payments :: [PayeePayment]
  } deriving (Show, Generic, ToJSON, FromJSON)

data PaymentOrder = PaymentOrder
  { paymentOrderUid :: PaymentOrderUid
  , amount :: CurrencyAndAmount
  , reference :: String
  , payeeUid :: PayeeUid
  , payeeAccountUid :: PayeeAccountUid
  , spendingCategory :: Maybe String
  } deriving (Show, Generic, ToJSON, FromJSON)

data PaymentOrderPayment = PaymentOrderPayment
  { paymentUid :: PaymentUid
  , amount :: CurrencyAndAmount
  , reference :: String
  , payeeUid :: PayeeUid
  , payeeAccountUid :: PayeeAccountUid
  , createdAt :: ZonedTime
  , completedAt :: Maybe ZonedTime
  , rejectedAt :: Maybe ZonedTime
  , paymentStatusDetails :: PaymentStatusDetails
  } deriving (Show, Generic, ToJSON, FromJSON)

data PaymentOrderPaymentsResponse = PaymentOrderPaymentsResponse
  { payments :: [PaymentOrderPayment]
  } deriving (Show, Generic, ToJSON, FromJSON)

newtype PaymentOrderUid = PaymentOrderUid UUID deriving (Show, Generic, ToJSON, FromJSON)

data PaymentStatus = Accepted | Rejected | Pending deriving (Show, Generic)
instance FromJSON PaymentStatus where
  parseJSON = withText "accountType" $ \case
      "ACCEPTED" -> return Accepted
      "REJECTED" -> return Rejected
      "PENDING"  -> return Pending
      _     -> fail "string is not one of known enum values"
instance ToJSON PaymentStatus where
  toJSON Accepted = "ACCEPTED"
  toJSON Rejected = "REJECTED"
  toJSON Pending  = "PENDING"

data PaymentStatusDescription
  = ACCEPTED
  | QUALIFIED_ACCEPT_WITHIN_TWO_HOURS
  | QUALIFIED_ACCEPT_UNSPECIFIED_DAY
  | QUALIFIED_ACCEPT_SAME_DAY
  | QUALIFIED_ACCEPT_NEXT_CALENDAR_DAY
  | QUALIFIED_ACCEPT_NEXT_WORKING_DAY
  | QUALIFIED_ACCEPT_AFTER_NEXT_WORKING_DAY
  | DESTINATION_ACCOUNT_INVALID
  | DESTINATION_ACCOUNT_NAME_MISMATCH
  | REFERENCE_INFORMATION_INCORRECT
  | DESTINATION_ACCOUNT_UNAVAILABLE
  | PENDING deriving (Show, Generic, ToJSON, FromJSON)

data PaymentStatusDetails = PaymentStatusDetails
  { paymentStatus :: PaymentStatus
  , description :: PaymentStatusDescription
  } deriving (Show, Generic, ToJSON, FromJSON)

newtype PaymentUid = PaymentUid UUID deriving (Show, Generic, ToJSON, FromJSON)

data RecurrenceRule = RecurrenceRule
  { startDate :: Day
  , frequency :: String -- enum ?
  , interval :: Maybe Int
  , count :: Maybe Int
  , untilDate :: Maybe Day
  , weekStart :: Maybe String -- enum ?
  , days :: Maybe [String] -- enum ?
  , monthDay :: Maybe Int
  , monthWeek :: Maybe Int
  } deriving (Show, Generic, ToJSON, FromJSON)

data ScheduledPayment = ScheduledPayment
  { accountHolderUid :: AccountHolderUid
  , paymentOrderUid :: PaymentOrderUid
  , categoryUid :: CategoryUid
  , nextPaymentAmount :: CurrencyAndAmount
  , reference :: String
  , payeeUid :: PayeeUid
  , payeeAccountUid :: PayeeAccountUid
  , recepientName :: Maybe String
  , recurrenceRule :: RecurrenceRule
  , startDate :: Maybe Day
  , nextDate :: Maybe Day
  , endDate :: Maybe Day
  , paymentType :: String
  , spendingCategory :: SpendingCategory
  } deriving (Show, Generic, ToJSON, FromJSON)

data ScheduledPayments = ScheduledPayments
  { scheduledPayments :: [ScheduledPayment]
  } deriving (Show, Generic, ToJSON, FromJSON)

data SignedCurrencyAndAmount = SignedCurrencyAndAmount
  { currency   :: Currency
  , minorUnits :: Int
  } deriving (Show, Generic, ToJSON, FromJSON)

data SoleTrader = SoleTrader
  { tradingAsName       :: String
  , businessCategory    :: String
  , businessSubCategory :: String
  } deriving (Show, Generic, ToJSON, FromJSON)

-- TODO: Build from enum
newtype SpendingCategory = SpendingCategory String deriving (Show, Generic, ToJSON, FromJSON)

data SpendingCategoryBreakdown = SpendingCategoryBreakdown
  { spendingCategory :: SpendingCategory
  , totalSpend       :: Double
  , totalRecieved    :: Double
  , netSpend         :: Double
  , netDirection     :: TransactionDirection
  , currency         :: Currency
  , percentage       :: Double
  , transactionCount :: Int
  } deriving (Show, Generic, ToJSON, FromJSON)

data SpendingCategorySummary = SpendingCategorySummary
  { period           :: String
  , totalSpend       :: Double
  , totalRecieved    :: Double
  , netSpend         :: Double
  , totalSpendNetOut :: Double
  , totalSpendNetIn  :: Double
  , currency         :: Currency
  , direction        :: TransactionDirection
  , breakdown        :: [SpendingCategoryBreakdown]
  } deriving (Show, Generic, ToJSON, FromJSON)

data SpendingCounterPartyBreakdown = SpendingCounterPartyBreakdown
  { counterPartyUid  :: UUID
  , counterPartyType :: CounterPartyType
  , counterPartyName :: String
  , totalSpend       :: Double
  , totalRecieved    :: Double
  , netSpend         :: Double
  , netDirection     :: TransactionDirection
  , currency         :: Currency
  , percentage       :: Double
  , transactionCount :: Int
  } deriving (Show, Generic, ToJSON, FromJSON)

data SpendingCounterPartySummary = SpendingCounterPartySummary
  { period           :: String
  , totalSpend       :: Double
  , totalRecieved    :: Double
  , netSpend         :: Double
  , totalSpendNetOut :: Double
  , totalSpendNetIn  :: Double
  , currency         :: Currency
  , direction        :: TransactionDirection
  , breakdown        :: [SpendingCounterPartyBreakdown]
  } deriving (Show, Generic, ToJSON, FromJSON)

data SpendingCountryBreakdown = SpendingCountryBreakdown
  { countryCode      :: CountryCode
  , totalSpend       :: Double
  , totalRecieved    :: Double
  , netSpend         :: Double
  , netDirection     :: TransactionDirection
  , currency         :: Currency
  , percentage       :: Double
  , transactionCount :: Int
  } deriving (Show, Generic, ToJSON, FromJSON)

data SpendingCountrySummary = SpendingCountrySummary
  { period           :: String
  , totalSpend       :: Double
  , totalRecieved    :: Double
  , netSpend         :: Double
  , totalSpendNetOut :: Double
  , totalSpendNetIn  :: Double
  , currency         :: Currency
  , direction        :: TransactionDirection
  , breakdown        :: [SpendingCountryBreakdown]
  } deriving (Show, Generic, ToJSON, FromJSON)

data StandingOrder = StandingOrder
  { paymentOrderUid         :: PaymentOrderUid
  , amount                  :: CurrencyAndAmount
  , reference               :: String
  , payeeUid                :: PayeeUid
  , payeeAccountUid         :: PayeeAccountUid
  , standingOrderRecurrence :: StandingOrderRecurrence
  , nextDate                :: Maybe Day
  , cancelledAt             :: Maybe ZonedTime
  , updateAt                :: Maybe ZonedTime
  , spendingCategory        :: Maybe String
  , categoryUid             :: CategoryUid
  } deriving (Show, Generic, ToJSON, FromJSON)

data StandingOrderFrequency = DAILY | WEEKLY | MONTHLY | YEARLY deriving (Show, Generic, ToJSON, FromJSON)

data StandingOrderRecurrence = StandingOrderRecurrence
  { startDate :: Day
  , frequency :: StandingOrderFrequency
  , interval  :: Maybe Int
  , count     :: Maybe Int
  , untilDate :: Maybe Day
  } deriving (Show, Generic, ToJSON, FromJSON)

data StandingOrdersResponse = StandingOrdersResponse
  { standingOrders :: [StandingOrder]
  } deriving (Show, Generic, ToJSON, FromJSON)

data TokenIdentity = TokenIdentity
  { accountHolderUid   :: AccountHolderUid
  , expiresAt          :: String
  , expiresInSeconds   :: Int
  , scopes             :: [String]
  , authenticated      :: Bool
  , applicationUserUid :: UUID
  } deriving (Show, Generic, ToJSON, FromJSON)

data TransactionDirection = In | Out deriving (Show, Generic)
instance FromJSON TransactionDirection where
  parseJSON = withText "accountType" $ \case
      "IN"  -> return In
      "OUT" -> return Out
      _     -> fail "string is not one of known enum values"
instance ToJSON TransactionDirection where
  toJSON In  = "IN"
  toJSON Out = "OUT"

-- Error
data ErrorResponse = ErrorResponse
  { errors  :: ErrorDetail
  , success :: Bool
  } deriving (Show, Generic, ToJSON, FromJSON)

newtype ErrorDetail = ErrorDetail
  { message :: String
  } deriving (Show, Generic, ToJSON, FromJSON)
