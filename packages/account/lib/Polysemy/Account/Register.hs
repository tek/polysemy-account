module Polysemy.Account.Register where

import Sqel (Uid (Uid))

import Polysemy.Account.Data.Account (Account (Account))
import Polysemy.Account.Data.AccountAuth (AccountAuth (AccountAuth))
import Polysemy.Account.Data.AccountCredentials (AccountCredentials (AccountCredentials))
import Polysemy.Account.Data.AuthedAccount (AuthedAccount (AuthedAccount))
import qualified Polysemy.Account.Effect.Accounts as Accounts
import Polysemy.Account.Effect.Accounts (Accounts)

register ::
  Member (Accounts i p) r =>
  AccountCredentials ->
  Sem r (AuthedAccount i p)
register (AccountCredentials username password) = do
  Uid accountId _ <- Accounts.create username
  Uid authId (AccountAuth _ _ _ _) <- Accounts.addPassword accountId password Nothing
  Uid _ (Account name status privs) <- Accounts.finalizeCreate accountId
  pure (AuthedAccount accountId authId name status privs)
