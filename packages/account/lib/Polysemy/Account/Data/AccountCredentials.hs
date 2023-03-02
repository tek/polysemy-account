-- | Description: Account credentials data type
module Polysemy.Account.Data.AccountCredentials where

import Polysemy.Account.Data.AccountName (AccountName)
import Polysemy.Account.Data.RawPassword (RawPassword)

-- | User-supplied credentials for login or registration.
data AccountCredentials =
  AccountCredentials {
    -- | Account name.
    username :: AccountName,
    -- | Account password.
    password :: RawPassword
  }
  deriving stock (Eq, Show)

json ''AccountCredentials
