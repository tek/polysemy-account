module Polysemy.Account.Data.AccountAuthDescription where

newtype AccountAuthDescription =
  AccountAuthDescription { unAccountAuthDescription :: Text }
  deriving stock (Eq, Show, Generic)
  deriving newtype (IsString, Ord)

json ''AccountAuthDescription
