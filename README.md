# Starling Bank APIs

A well-typed Haskell client for [Starling Bank APIs](https://developer.starlingbank.com/docs#api-reference-1)

Haddock :fish: [starling.bank.rly.rocks](https://starling.bank.rly.rocks)


## Supported Features

✅ Access Token

✅ GET requests for: 
  - Account Holders
  - Accounts 
  - Addresses
  - Direct Debits
  - Payees
  - Payments
  - Spending Insights
  - User Identities
  
## Roadmap
- Signed message requests
- PUT, POST, DELETE requests
- GET requests for: 
   - Savings goals
   - Profile Images
   - Cards
   - Recurring Card Payments
   - Transaction Feeds
   - Feed Round-up
   - KYC
   - Settle-up
   - Spaces
- Generalised account interaction `->` [bank](https://github.com/RileyEv/bank)

## Usage
```haskell
import Bank.Starling.API.Core (AccessToken(..), Endpoint(Production), getApiEndpoint)
import qualified Bank.Starling.API.Accounts as Accounts

main :: IO ()
main = do
  let token = AccessToken (read "<token>")
  let endpoint = getApiEndpoint Production -- Or Sandbox
  
  accounts <- Accounts.accounts endpoint token
  print accounts
```
  
