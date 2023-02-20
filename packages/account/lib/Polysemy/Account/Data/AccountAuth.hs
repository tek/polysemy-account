module Polysemy.Account.Data.AccountAuth where

import Chronos (Datetime)

import Polysemy.Account.Data.AccountAuthDescription (AccountAuthDescription)
import Polysemy.Account.Data.AccountPassword (AccountPassword)

data AccountAuth i =
  AccountAuth {
    account :: i,
    description :: AccountAuthDescription,
    password :: AccountPassword,
    expiry :: Maybe Datetime
  }
  deriving stock (Eq, Show, Generic)
