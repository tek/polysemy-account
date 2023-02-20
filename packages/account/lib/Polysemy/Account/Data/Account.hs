module Polysemy.Account.Data.Account where

import Polysemy.Account.Data.AccountName (AccountName)
import Polysemy.Account.Data.AccountStatus (AccountStatus)
import Polysemy.Account.Data.Privilege (Privilege)

data Account p =
  Account {
     name :: AccountName,
     status :: AccountStatus,
     privileges :: p
  }
  deriving stock (Eq, Show, Generic)

json ''Account

type AccountP = Account [Privilege]
