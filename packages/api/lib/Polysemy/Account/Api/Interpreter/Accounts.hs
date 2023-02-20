module Polysemy.Account.Api.Interpreter.Accounts where

import Data.UUID (UUID)
import Polysemy.Db.Data.DbError (DbError)
import Polysemy.Db.Data.InitDbError (InitDbError)
import Polysemy.Db.Effect.Store (Store)
import Polysemy.Db.Interpreter.Id (interpretIdUuidIO)
import Polysemy.Hasql.Effect.Database (Database)
import Polysemy.Hasql.Effect.DbTable (StoreTable)
import Polysemy.Hasql.Interpreter.DbTable (interpretDbTable)
import Polysemy.Hasql.Interpreter.Store (interpretStoreDb, primIdQuery)
import Sqel (Dd, FullCodec)
import Sqel.Codec (PrimColumn)
import Sqel.Ext (Column, ReifyCodec, ReifyDd)
import Sqel.Query (checkQuery)

import Polysemy.Account.Data.Account (Account)
import Polysemy.Account.Data.AccountAuth (AccountAuth)
import Polysemy.Account.Data.AccountsError (AccountsError)
import qualified Polysemy.Account.Db.Dd as Dd
import Polysemy.Account.Db.Interpreter.AccountByName (interpretQueryAccountByNameDb)
import Polysemy.Account.Db.Interpreter.AuthForAccount (interpretQueryAuthForAccountDb)
import Polysemy.Account.Effect.Accounts (Accounts)
import Polysemy.Account.Effect.Password (Password)
import Polysemy.Account.Interpreter.Accounts (interpretAccounts)
import Polysemy.Account.Interpreter.Password (interpretPassword)

interpretAccountDb ::
  ∀ i p s r .
  PrimColumn i =>
  Column p "privileges" s s =>
  ReifyCodec FullCodec s p =>
  ReifyDd s =>
  Dd s ->
  Members [Database !! DbError, Log, Embed IO] r =>
  InterpreterFor (StoreTable i (Account p) !! DbError) r
interpretAccountDb priv =
  interpretDbTable (Dd.accountSchema priv)

interpretAccountAuthDb ::
  ∀ i r .
  PrimColumn i =>
  Members [Database !! DbError, Log, Embed IO] r =>
  InterpreterFor (StoreTable i (AccountAuth i) !! DbError) r
interpretAccountAuthDb =
  interpretDbTable Dd.accountAuthSchema

interpretAccountStore ::
  ∀ i p t dt s r .
  PrimColumn i =>
  Column p "privileges" s s =>
  ReifyCodec FullCodec s p =>
  ReifyDd s =>
  Dd s ->
  Members [StoreTable i (Account p) !! DbError, Time t dt, Embed IO, Log] r =>
  InterpreterFor (Store i (Account p) !! DbError) r
interpretAccountStore priv =
  interpretStoreDb (Dd.accountSchema priv) (checkQuery primIdQuery (Dd.account priv))

interpretAccountAuthStore ::
  ∀ i t dt r .
  PrimColumn i =>
  Members [StoreTable i (AccountAuth i) !! DbError, Time t dt, Embed IO, Log] r =>
  InterpreterFor (Store i (AccountAuth i) !! DbError) r
interpretAccountAuthStore =
  interpretStoreDb Dd.accountAuthSchema (checkQuery primIdQuery Dd.accountAuth)

interpretAccountsPasswordDb ::
  ∀ p t dt s r .
  Members [Database !! DbError, Time t dt, Log, Error InitDbError, Embed IO] r =>
  Column p "privileges" s s =>
  ReifyCodec FullCodec s p =>
  ReifyDd s =>
  Dd s ->
  Bool ->
  p ->
  InterpretersFor [Accounts UUID p !! AccountsError, Password] r
interpretAccountsPasswordDb priv initActive defaultPerms =
  interpretPassword .
  interpretAccountDb priv .
  interpretQueryAccountByNameDb priv .
  interpretAccountAuthDb .
  interpretQueryAuthForAccountDb .
  interpretIdUuidIO .
  interpretAccountStore priv .
  interpretAccountAuthStore .
  interpretAccounts initActive defaultPerms .
  insertAt @1

interpretAccountsDb ::
  Members [Database !! DbError, Time t dt, Log, Error InitDbError, Embed IO] r =>
  Column p "privileges" s s =>
  ReifyCodec FullCodec s p =>
  ReifyDd s =>
  Dd s ->
  Bool ->
  p ->
  InterpreterFor (Accounts UUID p !! AccountsError) r
interpretAccountsDb priv initActive defaultPerms =
  interpretAccountsPasswordDb priv initActive defaultPerms . raiseUnder
