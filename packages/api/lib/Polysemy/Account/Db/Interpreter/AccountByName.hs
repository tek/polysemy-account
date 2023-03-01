module Polysemy.Account.Db.Interpreter.AccountByName where

import Data.UUID (UUID)
import Polysemy.Db.Data.DbError (DbError)
import Polysemy.Db.Data.InitDbError (InitDbError)
import Polysemy.Db.Effect.Query (Query (..))
import Polysemy.Hasql (interpretQueryDd)
import Polysemy.Hasql.Effect.DbTable (DbTable)
import Sqel (CheckedProjection, Dd, FullCodec, Uuid, primNewtype, prod)
import Sqel.Ext (Column, ReifyCodec, ReifyDd)

import Polysemy.Account.Data.Account (Account, AccountP)
import Polysemy.Account.Data.AccountByName (AccountByName)
import Polysemy.Account.Db.Dd (DdAccount, account, accountP)

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

interpretQueryAccountPByNameDb ::
  Members [DbTable (Uuid AccountP) !! DbError, Error InitDbError] r =>
  InterpreterFor (Query AccountByName (Maybe (Uuid AccountP)) !! DbError) r
interpretQueryAccountPByNameDb =
  interpretQueryDd table table query
  where
    table = accountP
    query = prod primNewtype
