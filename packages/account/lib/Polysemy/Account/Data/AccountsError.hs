-- | Description: Error data types for the effect 'Polysemy.Account.Accounts'.
module Polysemy.Account.Data.AccountsError where

-- | Errors that indicate invalid client-supplied information.
data AccountsClientError =
  -- | No account was found for the given ID.
  NoAccountId
  |
  -- | Credentials did not match stored auth data.
  InvalidAuth
  |
  -- | No account was found for the given name.
  NoAccountName
  |
  -- | Name given for registration already exists in storage.
  Conflict
  deriving stock (Eq, Show)

json ''AccountsClientError

-- | Errors produced by the effect 'Polysemy.Account.Accounts'.
data AccountsError =
  -- | Errors that indicate invalid client-supplied information.
  Client AccountsClientError
  |
  -- | Error indicating storage backend failure.
  Internal Text
  deriving stock (Eq, Show)
