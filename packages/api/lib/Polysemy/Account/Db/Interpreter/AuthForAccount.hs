module Polysemy.Account.Db.Interpreter.AuthForAccount where

import Data.UUID (UUID)
import Polysemy.Db.Data.DbError (DbError)
import Polysemy.Db.Effect.Query (Query)
import Polysemy.Hasql.Effect.DbTable (DbTable)
import Polysemy.Hasql.Interpreter.Query (interpretQueryDd)
import Sqel (Uuid, prim, prod)

import Polysemy.Account.Data.AccountAuth (AccountAuth)
import Polysemy.Account.Data.AuthForAccount (AuthForAccount)
import Polysemy.Account.Db.Dd (accountAuth)

interpretQueryAuthForAccountDb ::
  Member (DbTable (Uuid (AccountAuth UUID)) !! DbError) r =>
  InterpreterFor (Query (AuthForAccount UUID) [Uuid (AccountAuth UUID)] !! DbError) r
interpretQueryAuthForAccountDb =
  interpretQueryDd table table query
  where
    table = accountAuth
    query = prod prim
