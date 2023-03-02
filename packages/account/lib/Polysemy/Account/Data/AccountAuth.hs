-- | Description: Account auth data type
module Polysemy.Account.Data.AccountAuth where

import Chronos (Datetime)

import Polysemy.Account.Data.AccountAuthDescription (AccountAuthDescription)
import Polysemy.Account.Data.HashedPassword (HashedPassword)

-- | A hashed password associated with an account.
data AccountAuth i =
  AccountAuth {
    -- | The account ID belonging to this password.
    account :: i,
    -- | A description of the password.
    description :: AccountAuthDescription,
    -- | A password hash.
    password :: HashedPassword,
    -- | The date at which the password expires.
    expiry :: Maybe Datetime
  }
  deriving stock (Eq, Show, Generic)
