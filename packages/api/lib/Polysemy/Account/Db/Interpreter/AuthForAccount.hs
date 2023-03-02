-- | Description: PostgreSQL interpreters for the query for auth info by account ID
module Polysemy.Account.Db.Interpreter.AuthForAccount where

import Data.UUID (UUID)
import Polysemy.Db (DbError)
import Polysemy.Db (Query)
import Polysemy.Hasql (DbTable)
import Polysemy.Hasql (interpretQueryDd)
import Sqel (Uuid, prim, prod)

import Polysemy.Account.Data.AccountAuth (AccountAuth)
import Polysemy.Account.Data.AuthForAccount (AuthForAccount)
import Polysemy.Account.Db.Dd (accountAuth)

-- | Interpret @'Query' 'AuthForAccount'@ with [Polysemy.Hasql]("Polysemy.Hasql").
interpretQueryAuthForAccountDb ::
  Member (DbTable (Uuid (AccountAuth UUID)) !! DbError) r =>
  InterpreterFor (Query (AuthForAccount UUID) [Uuid (AccountAuth UUID)] !! DbError) r
interpretQueryAuthForAccountDb =
  interpretQueryDd table table query
  where
    table = accountAuth
    query = prod prim
