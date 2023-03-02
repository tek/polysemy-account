-- | Description: Interpreters for 'Accounts' and 'Password' using PostgreSQL backends
module Polysemy.Account.Api.Interpreter.Accounts where

import Data.UUID (UUID)
import Polysemy.Db (DbError, Id, InitDbError, Store)
import Polysemy.Hasql (Database, StoreTable, interpretStoreDb, interpretTable)
import Sqel (CheckedProjection, Dd, FullCodec, primIdQuery)
import Sqel.Codec (PrimColumn)
import Sqel.Ext (Column, ReifyCodec, ReifyDd)
import Sqel.Query (checkQuery)

import Polysemy.Account.Data.Account (Account)
import Polysemy.Account.Data.AccountAuth (AccountAuth)
import Polysemy.Account.Data.AccountsConfig (AccountsConfig)
import Polysemy.Account.Data.AccountsError (AccountsError)
import qualified Polysemy.Account.Db.Dd as Dd
import Polysemy.Account.Db.Dd (DdAccount)
import Polysemy.Account.Db.Interpreter.AccountByName (interpretQueryAccountByNameDb)
import Polysemy.Account.Db.Interpreter.AuthForAccount (interpretQueryAuthForAccountDb)
import Polysemy.Account.Effect.Accounts (Accounts)
import Polysemy.Account.Effect.Password (Password)
import Polysemy.Account.Interpreter.Accounts (interpretAccounts)
import Polysemy.Account.Interpreter.Password (interpretPassword)

-- | Interpret 'Polysemy.Hasql.DbTable' for 'Account'.
interpretAccountTable ::
  ∀ i p s r .
  PrimColumn i =>
  Column p "privileges" s s =>
  ReifyCodec FullCodec s p =>
  ReifyDd s =>
  Dd s ->
  Members [Database !! DbError, Log, Embed IO] r =>
  InterpreterFor (StoreTable i (Account p) !! DbError) r
interpretAccountTable priv =
  interpretTable (Dd.accountSchema priv)

-- | Interpret 'Polysemy.Hasql.DbTable' for 'AccountAuth'.
interpretAccountAuthTable ::
  ∀ i r .
  PrimColumn i =>
  Members [Database !! DbError, Log, Embed IO] r =>
  InterpreterFor (StoreTable i (AccountAuth i) !! DbError) r
interpretAccountAuthTable =
  interpretTable Dd.accountAuthSchema

-- | Interpret 'Store' for 'Account' as a 'Polysemy.Hasql.DbTable'.
interpretAccountStore ::
  ∀ i p s r .
  PrimColumn i =>
  Column p "privileges" s s =>
  ReifyCodec FullCodec s p =>
  ReifyDd s =>
  Dd s ->
  Member (StoreTable i (Account p) !! DbError) r =>
  InterpreterFor (Store i (Account p) !! DbError) r
interpretAccountStore priv =
  interpretStoreDb (Dd.accountSchema priv) (checkQuery primIdQuery (Dd.account priv))

-- | Interpret 'Store' for 'AccountAuth' as a 'Polysemy.Hasql.DbTable'.
interpretAccountAuthStore ::
  ∀ i r .
  PrimColumn i =>
  Member (StoreTable i (AccountAuth i) !! DbError) r =>
  InterpreterFor (Store i (AccountAuth i) !! DbError) r
interpretAccountAuthStore =
  interpretStoreDb Dd.accountAuthSchema (checkQuery primIdQuery Dd.accountAuth)

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
interpretAccountsDb priv conf =
  interpretPassword .
  raiseResumable (runReader conf) .
  interpretAccountTable priv .
  interpretQueryAccountByNameDb priv .
  interpretAccountAuthTable .
  interpretQueryAuthForAccountDb .
  interpretAccountStore priv .
  interpretAccountAuthStore .
  interpretAccounts .
  insertAt @1
