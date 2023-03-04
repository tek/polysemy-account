-- | Description: Interpreters for 'Store' and 'Polysemy.Hasql.DbTable' for 'Accounts and 'AccountAuth'.
module Polysemy.Account.Api.Db.Interpreter.Store where

import Polysemy.Db (DbError, Store)
import Polysemy.Hasql (Database, StoreTable, interpretStoreDb, interpretTable)
import Sqel (Dd, FullCodec, primIdQuery)
import Sqel.Codec (PrimColumn)
import Sqel.Ext (Column, ReifyCodec, ReifyDd)
import Sqel.Query (checkQuery)

import qualified Polysemy.Account.Api.Db.Dd as Dd
import Polysemy.Account.Data.Account (Account)
import Polysemy.Account.Data.AccountAuth (AccountAuth)

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
