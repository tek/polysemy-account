-- | Description: PostgreSQL interpreters for the query for an account by name
module Polysemy.Account.Api.Db.Interpreter.AccountByName where

import Data.UUID (UUID)
import Polysemy.Db (DbError, InitDbError)
import Polysemy.Db.Ext (Query (..))
import Polysemy.Hasql (DbTable, interpretQuery)
import Sqel (DdType, Uuid)
import Sqel.Exts (ReifySqel)

import Polysemy.Account.Api.Db.Dd (Table_Account, query_accountByName, table_Account, table_AccountP)
import Polysemy.Account.Data.Account (Account, AccountP)
import Polysemy.Account.Data.AccountByName (AccountByName)

-- | Interpret @'Query' 'AccountByName'@ with [Polysemy.Hasql]("Polysemy.Hasql").
interpretQueryAccountByNameDb ::
  ∀ sp p r table .
  table ~ Table_Account UUID p sp =>
  ReifySqel table =>
  Members [DbTable (DdType table) !! DbError, Error InitDbError] r =>
  InterpreterFor (Query AccountByName (Maybe (Uuid (Account p))) !! DbError) r
interpretQueryAccountByNameDb =
  interpretQuery query_accountByName (table_Account @sp)

-- | Interpret @'Query' 'AccountByName'@ with [Polysemy.Hasql]("Polysemy.Hasql").
--
-- Convenience specialization for the default privilege type.
interpretQueryAccountPByNameDb ::
  Members [DbTable (Uuid AccountP) !! DbError, Error InitDbError] r =>
  InterpreterFor (Query AccountByName (Maybe (Uuid AccountP)) !! DbError) r
interpretQueryAccountPByNameDb =
  interpretQuery query_accountByName table_AccountP
