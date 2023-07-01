-- | Description: Interpreters for 'Store' and 'Polysemy.Hasql.DbTable' for 'Accounts and 'AccountAuth'.
module Polysemy.Account.Api.Db.Interpreter.Store where

import Polysemy.Db (DbError, Store)
import Polysemy.Hasql (Database, DbTable, StoreTable, interpretStoreDb, interpretTable)
import Sqel (DdType, Def, IdQuery_Prim, query_idPrim)
import Sqel.Exts (Check1, ReifySqel)
import Sqel.Migration (InitTable)

import Polysemy.Account.Api.Db.Dd (Table_Account, Table_AccountAuth, table_Account, table_AccountAuth)
import Polysemy.Account.Data.Account (Account)
import Polysemy.Account.Data.AccountAuth (AccountAuth)

-- | Interpret 'Polysemy.Hasql.DbTable' for 'Account'.
interpretAccountTable ::
  ∀ sp i p table r .
  table ~ Table_Account i p sp =>
  ReifySqel table =>
  InitTable Def table =>
  Members [Database !! DbError, Log, Embed IO] r =>
  InterpreterFor (DbTable (DdType table) !! DbError) r
interpretAccountTable =
  interpretTable (table_Account @sp)

-- | Interpret 'Polysemy.Hasql.DbTable' for 'AccountAuth'.
interpretAccountAuthTable ::
  ∀ i table r .
  table ~ Table_AccountAuth i =>
  ReifySqel table =>
  InitTable Def table =>
  Members [Database !! DbError, Log, Embed IO] r =>
  InterpreterFor (DbTable (DdType table) !! DbError) r
interpretAccountAuthTable =
  interpretTable (table_AccountAuth @i)

-- | Interpret 'Store' for 'Account' as a 'Polysemy.Hasql.DbTable'.
interpretAccountStore ::
  ∀ sp i p table r .
  table ~ Table_Account i p sp =>
  ReifySqel table =>
  ReifySqel (IdQuery_Prim i) =>
  Check1 table (IdQuery_Prim i) =>
  Member (StoreTable i (Account p) !! DbError) r =>
  InterpreterFor (Store i (Account p) !! DbError) r
interpretAccountStore =
  interpretStoreDb query_idPrim (table_Account @sp)

-- | Interpret 'Store' for 'AccountAuth' as a 'Polysemy.Hasql.DbTable'.
interpretAccountAuthStore ::
  ∀ i table r .
  table ~ Table_AccountAuth i =>
  ReifySqel table =>
  ReifySqel (IdQuery_Prim i) =>
  Check1 table (IdQuery_Prim i) =>
  Member (StoreTable i (AccountAuth i) !! DbError) r =>
  InterpreterFor (Store i (AccountAuth i) !! DbError) r
interpretAccountAuthStore =
  interpretStoreDb query_idPrim (table_AccountAuth @i)
