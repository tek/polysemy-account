-- | Description: Interpreters for 'Accounts' and 'Password' using PostgreSQL backends
module Polysemy.Account.Api.Interpreter.Accounts where

import Data.UUID (UUID)
import Polysemy.Db (DbError, Id, InitDbError, Query, Store)
import Polysemy.Hasql (Database)
import Prelude hiding (Enum)
import Sqel (Def, Enum, Newtype, Uuid)
import Sqel.Exts (ReifySqel)
import Sqel.Migration (InitTable)

import Polysemy.Account.Api.Db.Dd (Table_Account)
import Polysemy.Account.Api.Db.Interpreter.AccountByName (interpretQueryAccountByNameDb)
import Polysemy.Account.Api.Db.Interpreter.AuthForAccount (interpretQueryAuthForAccountDb)
import Polysemy.Account.Api.Db.Interpreter.Store (
  interpretAccountAuthStore,
  interpretAccountAuthTable,
  interpretAccountStore,
  interpretAccountTable,
  )
import Polysemy.Account.Data.Account (Account, AccountP)
import Polysemy.Account.Data.AccountAuth (AccountAuth)
import Polysemy.Account.Data.AccountByName (AccountByName)
import Polysemy.Account.Data.AccountsConfig (AccountsConfig, AccountsConfigP)
import Polysemy.Account.Data.AccountsError (AccountsError)
import Polysemy.Account.Data.AuthForAccount (AuthForAccount)
import Polysemy.Account.Effect.Accounts (Accounts, AccountsP)
import Polysemy.Account.Effect.Password (Password)
import Polysemy.Account.Interpreter.Accounts (interpretAccounts)
import Polysemy.Account.Interpreter.Password (interpretPassword)

-- | Interpret 'Accounts' and 'Password' using PostgreSQL as storage backend, exposing Store and Query.
interpretAccountsDb' ::
  ∀ sp p r table_acc .
  table_acc ~ Table_Account UUID p sp =>
  ReifySqel table_acc =>
  InitTable Def table_acc =>
  Members [Database !! DbError, Id UUID, Log, Error InitDbError, Embed IO] r =>
  AccountsConfig p ->
  InterpretersFor [
    Accounts UUID p !! AccountsError,
    Store UUID (AccountAuth UUID) !! DbError,
    Store UUID (Account p) !! DbError,
    Query (AuthForAccount UUID) [Uuid (AccountAuth UUID)] !! DbError,
    Query AccountByName (Maybe (Uuid (Account p))) !! DbError,
    Password
  ] r
interpretAccountsDb' conf =
  interpretPassword .
  raiseResumable (runReader conf) .
  interpretAccountTable @sp .
  interpretAccountAuthTable .
  interpretQueryAccountByNameDb @sp .
  interpretQueryAuthForAccountDb .
  interpretAccountStore @sp .
  interpretAccountAuthStore .
  interpretAccounts .
  insertAt @5

-- | Interpret 'Accounts' and 'Password' using PostgreSQL as storage backend.
interpretAccountsDb ::
  ∀ sp p r table_acc .
  table_acc ~ Table_Account UUID p sp =>
  ReifySqel table_acc =>
  InitTable Def table_acc =>
  Members [Database !! DbError, Id UUID, Log, Error InitDbError, Embed IO] r =>
  AccountsConfig p ->
  InterpretersFor [Accounts UUID p !! AccountsError, Password] r
interpretAccountsDb conf =
  interpretAccountsDb' @sp conf .
  insertAt @1

-- | Interpret 'AccountsP' and 'Password' using PostgreSQL as storage backend, exposing Store and Query.
interpretAccountsPDb' ::
  ∀ r .
  Members [Database !! DbError, Id UUID, Log, Error InitDbError, Embed IO] r =>
  AccountsConfigP ->
  InterpretersFor [
    AccountsP UUID !! AccountsError,
    Store UUID (AccountAuth UUID) !! DbError,
    Store UUID AccountP !! DbError,
    Query (AuthForAccount UUID) [Uuid (AccountAuth UUID)] !! DbError,
    Query AccountByName (Maybe (Uuid AccountP)) !! DbError,
    Password
  ] r
interpretAccountsPDb' =
  interpretAccountsDb' @(Newtype Enum)

-- | Interpret 'AccountsP' and 'Password' using PostgreSQL as storage backend.
interpretAccountsPDb ::
  ∀ r .
  Members [Database !! DbError, Id UUID, Log, Error InitDbError, Embed IO] r =>
  AccountsConfigP ->
  InterpretersFor [AccountsP UUID !! AccountsError, Password] r
interpretAccountsPDb =
  interpretAccountsDb @(Newtype Enum)
