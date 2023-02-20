module Polysemy.Account.Data.AuthedAccount where

import Polysemy.Account.Data.AccountName (AccountName)
import Polysemy.Account.Data.Privilege (Privilege)
import Polysemy.Account.Data.AccountStatus (AccountStatus)

data AuthedAccount i p =
  AuthedAccount {
    id :: i,
    authId :: i,
    name :: AccountName,
    status :: AccountStatus,
    privileges :: p
  }
  deriving stock (Eq, Show, Generic)

json ''AuthedAccount

type AuthedAccountP i = AuthedAccount i [Privilege]
