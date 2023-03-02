{-# language NoFieldSelectors #-}

-- | Description: Account data type
module Polysemy.Account.Data.Account where

import Polysemy.Account.Data.AccountName (AccountName)
import Polysemy.Account.Data.AccountStatus (AccountStatus)
import Polysemy.Account.Data.Privilege (Privilege)

-- | A basic user account, consisting of a name, activation status, and an arbitrary privilege type.
data Account p =
  Account {
     name :: AccountName,
     status :: AccountStatus,
     privileges :: p
  }
  deriving stock (Eq, Show, Generic)

json ''Account

-- | Convenience alias for using the default privilege type with 'Account'.
type AccountP = Account [Privilege]
