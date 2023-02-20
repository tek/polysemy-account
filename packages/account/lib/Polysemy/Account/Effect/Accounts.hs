module Polysemy.Account.Effect.Accounts where

import Chronos (Datetime)
import Sqel (Uid)

import Polysemy.Account.Data.Account (Account)
import Polysemy.Account.Data.AccountAuth (AccountAuth)
import Polysemy.Account.Data.AccountName (AccountName)
import Polysemy.Account.Data.AccountStatus (AccountStatus)
import Polysemy.Account.Data.AuthToken (AuthToken)
import Polysemy.Account.Data.AuthedAccount (AuthedAccount)
import Polysemy.Account.Data.Privilege (Privilege)
import Polysemy.Account.Data.RawPassword (RawPassword)

data Accounts i p :: Effect where
  Authenticate :: AccountName -> RawPassword -> Accounts i p m (Uid i (AccountAuth i))
  GenerateToken :: i -> Maybe Datetime -> Accounts i p m AuthToken
  Create :: AccountName -> Accounts i p m (Uid i (Account p))
  FinalizeCreate :: i -> Accounts i p m (Uid i (Account p))
  AddPassword :: i -> RawPassword -> Maybe Datetime -> Accounts i p m (Uid i (AccountAuth i))
  SetStatus :: i -> AccountStatus -> Accounts i p m ()
  ById :: i -> Accounts i p m (Uid i (Account p))
  ByName :: AccountName -> Accounts i p m (Uid i (Account p))
  Authed :: i -> Accounts i p m (AuthedAccount i p)
  Update :: Uid i (Account p) -> Accounts i p m ()
  Privileges :: i -> Accounts i p m p
  UpdatePrivileges :: i -> (p -> p) -> Accounts i p m ()
  All :: Accounts i p m [Uid i (Account p)]
  AllAuths :: Accounts i p m [Uid i (AccountAuth i)]

makeSem ''Accounts

type AccountsP i = Accounts i [Privilege]
