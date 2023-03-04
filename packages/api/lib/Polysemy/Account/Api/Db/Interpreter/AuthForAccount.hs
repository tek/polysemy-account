-- | Description: PostgreSQL interpreters for the query for auth info by account ID
module Polysemy.Account.Api.Db.Interpreter.AuthForAccount where

import Data.UUID (UUID)
import Polysemy.Db (DbError, Query)
import Polysemy.Hasql (DbTable, interpretQueryDd)
import Sqel (Uuid, prim, prod)

import Polysemy.Account.Api.Db.Dd (accountAuth)
import Polysemy.Account.Data.AccountAuth (AccountAuth)
import Polysemy.Account.Data.AuthForAccount (AuthForAccount)

-- | Interpret @'Query' 'AuthForAccount'@ with [Polysemy.Hasql]("Polysemy.Hasql").
interpretQueryAuthForAccountDb ::
  Member (DbTable (Uuid (AccountAuth UUID)) !! DbError) r =>
  InterpreterFor (Query (AuthForAccount UUID) [Uuid (AccountAuth UUID)] !! DbError) r
interpretQueryAuthForAccountDb =
  interpretQueryDd table table query
  where
    table = accountAuth
    query = prod prim
