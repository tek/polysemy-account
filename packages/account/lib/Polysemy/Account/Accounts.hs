module Polysemy.Account.Accounts where

import Sqel (Uid (Uid))

import Polysemy.Account.Data.AccountName (AccountName)
import Polysemy.Account.Data.AccountStatus (AccountStatus (Active))
import Polysemy.Account.Data.AccountsError (AccountsError)
import qualified Polysemy.Account.Effect.Accounts as Accounts
import Polysemy.Account.Effect.Accounts (Accounts)

unlockAccountName ::
  Members [Accounts i p, Stop AccountsError] r =>
  AccountName ->
  Sem r ()
unlockAccountName name = do
  Uid i _ <- Accounts.byName name
  Accounts.setStatus i Active
