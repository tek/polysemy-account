module Polysemy.Account.Data.AccountCredentials where

import Polysemy.Account.Data.AccountName (AccountName)
import Polysemy.Account.Data.RawPassword (RawPassword)

data AccountCredentials =
  AccountCredentials {
     username :: AccountName,
     password :: RawPassword
  }
  deriving stock (Eq, Show)

json ''AccountCredentials
