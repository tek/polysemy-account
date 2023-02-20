module Polysemy.Account.Login where

import Sqel (Uid (Uid))

import Polysemy.Account.Data.Account (Account (Account))
import Polysemy.Account.Data.AccountAuth (AccountAuth (AccountAuth))
import Polysemy.Account.Data.AccountCredentials (AccountCredentials (AccountCredentials))
import Polysemy.Account.Data.AuthedAccount (AuthedAccount (AuthedAccount))
import qualified Polysemy.Account.Effect.Accounts as Accounts
import Polysemy.Account.Effect.Accounts (Accounts)

login ::
  Member (Accounts i p) r =>
  AccountCredentials ->
  Sem r (AuthedAccount i p)
login (AccountCredentials username password) = do
  Uid authId (AccountAuth accountId _ _ _) <- Accounts.authenticate username password
  Uid _ (Account name status privs) <- Accounts.byId accountId
  pure (AuthedAccount accountId authId name status privs)
