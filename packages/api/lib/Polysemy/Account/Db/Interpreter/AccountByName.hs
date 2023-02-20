module Polysemy.Account.Db.Interpreter.AccountByName where

import Polysemy.Db.Data.DbError (DbError)
import Polysemy.Db.Data.InitDbError (InitDbError)
import Polysemy.Db.Effect.Query (Query (..))
import Polysemy.Hasql.Effect.DbTable (DbTable)
import Polysemy.Hasql.Interpreter.Query (interpretQueryDd)
import Sqel (Dd, Uuid, primNewtype, prod)
import Sqel.Comp (Column)
import Sqel.Data.Codec (FullCodec)
import Sqel.ReifyCodec (ReifyCodec)
import Sqel.ReifyDd (ReifyDd)

import Polysemy.Account.Data.Account (Account, AccountP)
import Polysemy.Account.Data.AccountByName (AccountByName)
import Polysemy.Account.Db.Dd (account, accountP)

interpretQueryAccountByNameDb ::
  ∀ p s r .
  Column p "privileges" s s =>
  ReifyCodec FullCodec s p =>
  ReifyDd s =>
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
