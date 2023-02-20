module Polysemy.Account.Data.AccountsError where

data AccountsClientError =
  NoAccountId
  |
  InvalidAuth
  |
  NoAccountName
  |
  Conflict
  deriving stock (Eq, Show)

json ''AccountsClientError

data AccountsError =
  Client AccountsClientError
  |
  Internal Text
  deriving stock (Eq, Show)
