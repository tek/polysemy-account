module Polysemy.Account.Data.AccountPassword where

newtype AccountPassword =
  AccountPassword { unAccountPassword :: Text }
  deriving stock (Eq, Show, Generic)
  deriving newtype (IsString, Ord)

json ''AccountPassword
