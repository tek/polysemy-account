-- | Description: Account name data type
module Polysemy.Account.Data.AccountName where

-- | The name of an account.
newtype AccountName =
  AccountName { unAccountName :: Text }
  deriving stock (Eq, Show, Generic)
  deriving newtype (IsString, Ord)

json ''AccountName
