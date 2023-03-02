-- | Description: PostgreSQL interpreters for the query for an account by name
module Polysemy.Account.Db.Interpreter.AccountByName where

import Data.UUID (UUID)
import Polysemy.Db (DbError)
import Polysemy.Db (InitDbError)
import Polysemy.Db.Ext (Query (..))
import Polysemy.Hasql (interpretQueryDd)
import Polysemy.Hasql (DbTable)
import Sqel (CheckedProjection, Dd, FullCodec, Uuid, primNewtype, prod)
import Sqel.Ext (Column, ReifyCodec, ReifyDd)

import Polysemy.Account.Data.Account (Account, AccountP)
import Polysemy.Account.Data.AccountByName (AccountByName)
import Polysemy.Account.Db.Dd (DdAccount, account, accountP)

-- | Interpret @'Query' 'AccountByName'@ with [Polysemy.Hasql]("Polysemy.Hasql").
interpretQueryAccountByNameDb ::
  ∀ p s r .
  Column p "privileges" s s =>
  ReifyCodec FullCodec s p =>
  ReifyDd s =>
  CheckedProjection (DdAccount UUID p s) (DdAccount UUID p s) =>
  Members [DbTable (Uuid (Account p)) !! DbError, Error InitDbError] r =>
  Dd s ->
  InterpreterFor (Query AccountByName (Maybe (Uuid (Account p))) !! DbError) r
interpretQueryAccountByNameDb p =
  interpretQueryDd table table query
  where
    table = account p
    query = prod primNewtype

-- | Interpret @'Query' 'AccountByName'@ with [Polysemy.Hasql]("Polysemy.Hasql").
--
-- Convenience specialization for the default privilege type.
interpretQueryAccountPByNameDb ::
  Members [DbTable (Uuid AccountP) !! DbError, Error InitDbError] r =>
  InterpreterFor (Query AccountByName (Maybe (Uuid AccountP)) !! DbError) r
interpretQueryAccountPByNameDb =
  interpretQueryDd table table query
  where
    table = accountP
    query = prod primNewtype
