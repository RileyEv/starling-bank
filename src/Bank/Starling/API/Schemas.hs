{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE OverloadedStrings     #-}

module Bank.Starling.API.Schemas where

import           Bank.Core           (Currency)
import           Data.Aeson          (FromJSON (..), ToJSON (..), withText)
import           Data.Time.Calendar  (Day)
import           Data.Time.LocalTime (ZonedTime)
import           Data.UUID           (UUID)
import           GHC.Generics        (Generic)

data Account = Account
                 { accountUid      :: AccountUid
                 , accountType     :: AccountType
                 , defaultCategory :: CategoryUid
                 , currency        :: Currency
                 , createdAt       :: ZonedTime
                 , name            :: String
                 }
  deriving (FromJSON, Generic, Show, ToJSON)

data AccountHolder = AccountHolder
                       { accoundHolderUid  :: AccountHolderUid
                       , accountHolderType :: AccountHolderType
                       }
  deriving (FromJSON, Generic, Show, ToJSON)

newtype AccountHolderName = AccountHolderName { accountHolderName :: String }
  deriving (FromJSON, Generic, Show, ToJSON)

data AccountHolderType = IndividualAccount | BusinessAccount | SoleTraderAccount | JointAccount | BankingAsAService deriving
    ( Generic
    , Show
    )

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
                           }
  deriving (FromJSON, Generic, Show, ToJSON)

data AccountIdentifiers = AccountIdentifiers
                            { accountIdentifier  :: String
                            , bankIdentifier     :: String
                            , iban               :: String
                            , bic                :: String
                            , accountIdentifiers :: [AccountIdentifier]
                            }
  deriving (FromJSON, Generic, Show, ToJSON)

newtype Accounts = Accounts { accounts :: [Account] }
  deriving (FromJSON, Generic, Show, ToJSON)

data AccountStatementPeriod = AccountStatementPeriod
                                { period  :: String
                                , partial :: Bool
                                , endsAt  :: Maybe ZonedTime
                                }
  deriving (FromJSON, Generic, Show, ToJSON)

newtype AccountStatementPeriods = AccountStatementPeriods { periods :: [AccountStatementPeriod] }
  deriving (FromJSON, Generic, Show, ToJSON)

data AccountType = Primary | Additional | FixedTermDeposit | Loan deriving (Generic, Show)

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

newtype AccountUid = AccountUid UUID
  deriving (FromJSON, Generic, Show, ToJSON)

newtype AccountHolderUid = AccountHolderUid UUID
  deriving (FromJSON, Generic, Show, ToJSON)

data Address = Address
                 { line1       :: String
                 , line2       :: Maybe String
                 , line3       :: Maybe String
                 , postTown    :: String
                 , postCode    :: String
                 , countryCode :: String
                 }
  deriving (FromJSON, Generic, Show, ToJSON)

data Addresses = Addresses
                   { current  :: Address
                   , previous :: [Address]
                   }
  deriving (FromJSON, Generic, Show, ToJSON)

data Balance = Balance
                 { clearedBalance        :: SignedCurrencyAndAmount
                 , effectiveBalance      :: SignedCurrencyAndAmount
                 , pendingTransactions   :: SignedCurrencyAndAmount
                 , acceptedOverdraft     :: SignedCurrencyAndAmount
                 , amount                :: SignedCurrencyAndAmount
                 , totalClearedBalance   :: SignedCurrencyAndAmount
                 , totalEffectiveBalance :: SignedCurrencyAndAmount
                 }
  deriving (FromJSON, Generic, Show, ToJSON)

data BankIdentifierType = SORT_CODE | SWIFT | IBAN | ABA | ABA_WIRE | ABA_ACH deriving
    ( FromJSON
    , Generic
    , Show
    , ToJSON
    )

data Business = Business
                  { companyName               :: String
                  , companyType               :: String
                  , companyCategory           :: String
                  , companySubCategory        :: String
                  , companyRegistrationNumber :: String
                  , email                     :: String
                  , phone                     :: String
                  }
  deriving (FromJSON, Generic, Show, ToJSON)

newtype CategoryUid = CategoryUid UUID
  deriving (FromJSON, Generic, Show, ToJSON)

data ConfirmationOfFundsResponse = ConfirmationOfFundsResponse
                                     { requestedAmountAvailableToSpend                 :: Bool
                                     , accountWouldBeInOverdraftIfRequestedAmountSpent :: Bool
                                     }
  deriving (FromJSON, Generic, Show, ToJSON)

data CounterPartyType = Category | Cheque | Customer | PayeeCounterParty | Merchant | Sender | Starling | LoanCounterParty deriving
    ( Generic
    , Show
    )

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

data CountryCode = AC | AD | AE | AF | AG | AI | AL | AM | AN | AO | AQ | AR | AS | AT | AU | AW | AX | AZ | BA | BB | BD | BE | BF | BG | BH | BI | BJ | BL | BM | BN | BO | BQ | BR | BS | BT | BU | BV | BW | BY | BZ | CA | CC | CD | CF | CG | CH | CI | CK | CL | CM | CN | CO | CP | CR | CS | CU | CV | CW | CX | CY | CZ | DE | DG | DJ | DK | DM | DO | DZ | EA | EC | EE | EG | EH | ER | ES | ET | EU | EZ | FI | FJ | FK | FM | FO | FR | FX | GA | GB | GD | GE | GF | GG | GH | GI | GL | GM | GN | GP | GQ | GR | GS | GT | GU | GW | GY | HK | HM | HN | HR | HT | HU | IC | ID | IE | IL | IM | IN | IO | IQ | IR | IS | IT | JE | JM | JO | JP | KE | KG | KH | KI | KM | KN | KP | KR | KW | KY | KZ | LA | LB | LC | LI | LK | LR | LS | LT | LU | LV | LY | MA | MC | MD | ME | MF | MG | MH | MK | ML | MM | MN | MO | MP | MQ | MR | MS | MT | MU | MV | MW | MX | MY | MZ | NA | NC | NE | NF | NG | NI | NL | NO | NP | NR | NT | NU | NZ | OM | PA | PE | PF | PG | PH | PK | PL | PM | PN | PR | PS | PT | PW | PY | QA | RE | RO | RS | RU | RW | SA | SB | SC | SD | SE | SF | SG | SH | SI | SJ | SK | SL | SM | SN | SO | SR | SS | ST | SU | SV | SX | SY | SZ | TA | TC | TD | TF | TG | TH | TJ | TK | TL | TM | TN | TO | TP | TR | TT | TV | TW | TZ | UA | UG | UK | UM | US | UY | UZ | VA | VC | VE | VG | VI | VN | VU | WF | WS | XI | XU | XK | YE | YT | YU | ZA | ZM | ZR | ZW deriving
    ( FromJSON
    , Generic
    , Show
    , ToJSON
    )

data CurrencyAndAmount = CurrencyAndAmount
                           { currency   :: Currency
                           , minorUnits :: Int
                           }
  deriving (FromJSON, Generic, Show, ToJSON)

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
                            }
  deriving (FromJSON, Generic, Show, ToJSON)

newtype DirectDebitMandates = DirectDebitMandates { mandates :: [DirectDebitMandate] }
  deriving (FromJSON, Generic, Show, ToJSON)

newtype DirectDebitMandateUid = DirectDebitMandateUid UUID
  deriving (FromJSON, Generic, Show, ToJSON)

data IdentifierType = SortCode | IbanBic | AbaAch deriving (Generic, Show)

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
                    }
  deriving (FromJSON, Generic, Show, ToJSON)

data Joint = Joint
               { accountHolderUid :: AccountHolderUid
               , personOne        :: Individual
               , personTwo        :: Individual
               }
  deriving (FromJSON, Generic, Show, ToJSON)

data LastPayment = LastPayment
                     { lastDate   :: Day
                     , lastAmount :: CurrencyAndAmount
                     }
  deriving (FromJSON, Generic, Show, ToJSON)

newtype NextPaymentDatesResponse = NextPaymentDatesResponse { nextPaymentDates :: [Day] }
  deriving (FromJSON, Generic, Show, ToJSON)

data Payee = Payee
               { payeeUid     :: PayeeUid
               , payeeName    :: String
               , phoneNumber  :: String
               , payeeType    :: PayeeType
               , firstName    :: Maybe String
               , middleName   :: Maybe String
               , lastName     :: Maybe String
               , businessName :: Maybe String
               , dateOfBirth  :: Maybe Day
               , accounts     :: [PayeeAccount]
               }
  deriving (FromJSON, Generic, Show, ToJSON)

data PayeeAccount = PayeeAccount
                      { payeeAccountUid    :: PayeeAccountUid
                      , payeeChannelType   :: PayeeChannelType
                      , description        :: String
                      , defaultAccount     :: Bool
                      , countryCode        :: CountryCode
                      , accountIdentifier  :: String
                      , bankIdentifier     :: String
                      , bankIdentifierType :: BankIdentifierType
                      , lastReferences     :: [String]
                      }
  deriving (FromJSON, Generic, Show, ToJSON)

newtype PayeeAccountUid = PayeeAccountUid UUID
  deriving (FromJSON, Generic, Show, ToJSON)

data PayeeChannelType = BANK_ACCOUNT | SETTLE_UP | NEARBY deriving (FromJSON, Generic, Show, ToJSON)

data PayeePayment = PayeePayment
                      { paymentUid       :: PaymentUid
                      , amount           :: CurrencyAndAmount
                      , reference        :: String
                      , createdAt        :: ZonedTime
                      , spendingCategory :: SpendingCategory
                      , paymentAmount    :: CurrencyAndAmount
                      }
  deriving (FromJSON, Generic, Show, ToJSON)

newtype Payees = Payees { payees :: [Payee] }
  deriving (FromJSON, Generic, Show, ToJSON)

data PayeeType = BUSINESS | INDIVIDUAL deriving (FromJSON, Generic, Show, ToJSON)

newtype PayeeUid = PayeeUid UUID
  deriving (FromJSON, Generic, Show, ToJSON)

newtype Payments = Payments { payments :: [PayeePayment] }
  deriving (FromJSON, Generic, Show, ToJSON)

data PaymentOrder = PaymentOrder
                      { paymentOrderUid  :: PaymentOrderUid
                      , amount           :: CurrencyAndAmount
                      , reference        :: String
                      , payeeUid         :: PayeeUid
                      , payeeAccountUid  :: PayeeAccountUid
                      , spendingCategory :: Maybe String
                      }
  deriving (FromJSON, Generic, Show, ToJSON)

data PaymentOrderPayment = PaymentOrderPayment
                             { paymentUid           :: PaymentUid
                             , amount               :: CurrencyAndAmount
                             , reference            :: String
                             , payeeUid             :: PayeeUid
                             , payeeAccountUid      :: PayeeAccountUid
                             , createdAt            :: ZonedTime
                             , completedAt          :: Maybe ZonedTime
                             , rejectedAt           :: Maybe ZonedTime
                             , paymentStatusDetails :: PaymentStatusDetails
                             }
  deriving (FromJSON, Generic, Show, ToJSON)

newtype PaymentOrderPaymentsResponse = PaymentOrderPaymentsResponse { payments :: [PaymentOrderPayment] }
  deriving (FromJSON, Generic, Show, ToJSON)

newtype PaymentOrderUid = PaymentOrderUid UUID
  deriving (FromJSON, Generic, Show, ToJSON)

data PaymentStatus = Accepted | Rejected | Pending deriving (Generic, Show)

instance FromJSON PaymentStatus where
  parseJSON = withText "accountType" $ \case
    "ACCEPTED" -> return Accepted
    "REJECTED" -> return Rejected
    "PENDING"  -> return Pending
    _          -> fail "string is not one of known enum values"

instance ToJSON PaymentStatus where
  toJSON Accepted = "ACCEPTED"
  toJSON Rejected = "REJECTED"
  toJSON Pending  = "PENDING"

data PaymentStatusDescription = ACCEPTED | QUALIFIED_ACCEPT_WITHIN_TWO_HOURS | QUALIFIED_ACCEPT_UNSPECIFIED_DAY | QUALIFIED_ACCEPT_SAME_DAY | QUALIFIED_ACCEPT_NEXT_CALENDAR_DAY | QUALIFIED_ACCEPT_NEXT_WORKING_DAY | QUALIFIED_ACCEPT_AFTER_NEXT_WORKING_DAY | DESTINATION_ACCOUNT_INVALID | DESTINATION_ACCOUNT_NAME_MISMATCH | REFERENCE_INFORMATION_INCORRECT | DESTINATION_ACCOUNT_UNAVAILABLE | PENDING deriving
    ( FromJSON
    , Generic
    , Show
    , ToJSON
    )

data PaymentStatusDetails = PaymentStatusDetails
                              { paymentStatus :: PaymentStatus
                              , description   :: PaymentStatusDescription
                              }
  deriving (FromJSON, Generic, Show, ToJSON)

newtype PaymentUid = PaymentUid UUID
  deriving (FromJSON, Generic, Show, ToJSON)

data RecurrenceRule = RecurrenceRule
                        { startDate :: Day
                        , frequency :: String -- enum ?
                        , interval  :: Maybe Int
                        , count     :: Maybe Int
                        , untilDate :: Maybe Day
                        , weekStart :: Maybe String -- enum ?
                        , days      :: Maybe [String] -- enum ?
                        , monthDay  :: Maybe Int
                        , monthWeek :: Maybe Int
                        }
  deriving (FromJSON, Generic, Show, ToJSON)

data ScheduledPayment = ScheduledPayment
                          { accountHolderUid  :: AccountHolderUid
                          , paymentOrderUid   :: PaymentOrderUid
                          , categoryUid       :: CategoryUid
                          , nextPaymentAmount :: CurrencyAndAmount
                          , reference         :: String
                          , payeeUid          :: PayeeUid
                          , payeeAccountUid   :: PayeeAccountUid
                          , recepientName     :: Maybe String
                          , recurrenceRule    :: RecurrenceRule
                          , startDate         :: Maybe Day
                          , nextDate          :: Maybe Day
                          , endDate           :: Maybe Day
                          , paymentType       :: String
                          , spendingCategory  :: SpendingCategory
                          }
  deriving (FromJSON, Generic, Show, ToJSON)

newtype ScheduledPayments = ScheduledPayments { scheduledPayments :: [ScheduledPayment] }
  deriving (FromJSON, Generic, Show, ToJSON)

data SignedCurrencyAndAmount = SignedCurrencyAndAmount
                                 { currency   :: Currency
                                 , minorUnits :: Int
                                 }
  deriving (FromJSON, Generic, Show, ToJSON)

data SoleTrader = SoleTrader
                    { tradingAsName       :: String
                    , businessCategory    :: String
                    , businessSubCategory :: String
                    }
  deriving (FromJSON, Generic, Show, ToJSON)

-- TODO: Build from enum
newtype SpendingCategory = SpendingCategory String
  deriving (FromJSON, Generic, Show, ToJSON)

data SpendingCategoryBreakdown = SpendingCategoryBreakdown
                                   { spendingCategory :: SpendingCategory
                                   , totalSpend       :: Double
                                   , totalRecieved    :: Double
                                   , netSpend         :: Double
                                   , netDirection     :: TransactionDirection
                                   , currency         :: Currency
                                   , percentage       :: Double
                                   , transactionCount :: Int
                                   }
  deriving (FromJSON, Generic, Show, ToJSON)

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
                                 }
  deriving (FromJSON, Generic, Show, ToJSON)

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
                                       }
  deriving (FromJSON, Generic, Show, ToJSON)

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
                                     }
  deriving (FromJSON, Generic, Show, ToJSON)

data SpendingCountryBreakdown = SpendingCountryBreakdown
                                  { countryCode      :: CountryCode
                                  , totalSpend       :: Double
                                  , totalRecieved    :: Double
                                  , netSpend         :: Double
                                  , netDirection     :: TransactionDirection
                                  , currency         :: Currency
                                  , percentage       :: Double
                                  , transactionCount :: Int
                                  }
  deriving (FromJSON, Generic, Show, ToJSON)

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
                                }
  deriving (FromJSON, Generic, Show, ToJSON)

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
                       }
  deriving (FromJSON, Generic, Show, ToJSON)

data StandingOrderFrequency = DAILY | WEEKLY | MONTHLY | YEARLY deriving
    ( FromJSON
    , Generic
    , Show
    , ToJSON
    )

data StandingOrderRecurrence = StandingOrderRecurrence
                                 { startDate :: Day
                                 , frequency :: StandingOrderFrequency
                                 , interval  :: Maybe Int
                                 , count     :: Maybe Int
                                 , untilDate :: Maybe Day
                                 }
  deriving (FromJSON, Generic, Show, ToJSON)

newtype StandingOrdersResponse = StandingOrdersResponse { standingOrders :: [StandingOrder] }
  deriving (FromJSON, Generic, Show, ToJSON)

data TokenIdentity = TokenIdentity
                       { accountHolderUid   :: AccountHolderUid
                       , expiresAt          :: String
                       , expiresInSeconds   :: Int
                       , scopes             :: [String]
                       , authenticated      :: Bool
                       , applicationUserUid :: UUID
                       }
  deriving (FromJSON, Generic, Show, ToJSON)

data TransactionDirection = In | Out deriving (Generic, Show)

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
                       }
  deriving (FromJSON, Generic, Show, ToJSON)

newtype ErrorDetail = ErrorDetail { message :: String }
  deriving (FromJSON, Generic, Show, ToJSON)
