module Polysemy.Account.Data.AccountName where

newtype AccountName =
  AccountName { unAccountName :: Text }
  deriving stock (Eq, Show, Generic)
  deriving newtype (IsString, Ord)

json ''AccountName
