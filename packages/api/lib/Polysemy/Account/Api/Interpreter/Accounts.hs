-- | Description: Interpreters for 'Accounts' and 'Password' using PostgreSQL backends
module Polysemy.Account.Api.Interpreter.Accounts where

import Data.UUID (UUID)
import Polysemy.Db (DbError, Id, InitDbError)
import Polysemy.Hasql (Database)
import Sqel (CheckedProjection, Dd, FullCodec)
import Sqel.Ext (Column, ReifyCodec, ReifyDd)

import Polysemy.Account.Api.Db.Dd (DdAccount, privileges)
import Polysemy.Account.Api.Db.Interpreter.AccountByName (interpretQueryAccountByNameDb)
import Polysemy.Account.Api.Db.Interpreter.AuthForAccount (interpretQueryAuthForAccountDb)
import Polysemy.Account.Api.Db.Interpreter.Store (
  interpretAccountAuthStore,
  interpretAccountAuthTable,
  interpretAccountStore,
  interpretAccountTable,
  )
import Polysemy.Account.Data.AccountsConfig (AccountsConfig, AccountsConfigP)
import Polysemy.Account.Data.AccountsError (AccountsError)
import Polysemy.Account.Effect.Accounts (Accounts, AccountsP)
import Polysemy.Account.Effect.Password (Password)
import Polysemy.Account.Interpreter.Accounts (interpretAccounts)
import Polysemy.Account.Interpreter.Password (interpretPassword)

-- | Interpret 'Accounts' and 'Password' using PostgreSQL as storage backend.
interpretAccountsDb ::
  ∀ p s r .
  Members [Database !! DbError, Id UUID, Log, Error InitDbError, Embed IO] r =>
  Column p "privileges" s s =>
  ReifyCodec FullCodec s p =>
  ReifyDd s =>
  CheckedProjection (DdAccount UUID p s) (DdAccount UUID p s) =>
  Dd s ->
  AccountsConfig p ->
  InterpretersFor [Accounts UUID p !! AccountsError, Password] r
interpretAccountsDb dd conf =
  interpretPassword .
  raiseResumable (runReader conf) .
  interpretAccountTable dd .
  interpretQueryAccountByNameDb dd .
  interpretAccountAuthTable .
  interpretQueryAuthForAccountDb .
  interpretAccountStore dd .
  interpretAccountAuthStore .
  interpretAccounts .
  insertAt @1

-- | Interpret 'AccountsP' and 'Password' using PostgreSQL as storage backend.
interpretAccountsPDb ::
  ∀ r .
  Members [Database !! DbError, Id UUID, Log, Error InitDbError, Embed IO] r =>
  AccountsConfigP ->
  InterpretersFor [AccountsP UUID !! AccountsError, Password] r
interpretAccountsPDb =
  interpretAccountsDb privileges
