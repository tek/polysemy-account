-- | Description: PostgreSQL interpreters for the query for auth info by account ID
module Polysemy.Account.Api.Db.Interpreter.AuthForAccount where

import Data.UUID (UUID)
import Polysemy.Db (DbError, Query)
import Polysemy.Hasql (DbTable, interpretQuery)
import Sqel (Uuid)

import Polysemy.Account.Api.Db.Dd (query_authForAccount, table_AccountAuth)
import Polysemy.Account.Data.AccountAuth (AccountAuth)
import Polysemy.Account.Data.AuthForAccount (AuthForAccount)

-- | Interpret @'Query' 'AuthForAccount'@ with [Polysemy.Hasql]("Polysemy.Hasql").
interpretQueryAuthForAccountDb ::
  Member (DbTable (Uuid (AccountAuth UUID)) !! DbError) r =>
  InterpreterFor (Query (AuthForAccount UUID) [Uuid (AccountAuth UUID)] !! DbError) r
interpretQueryAuthForAccountDb =
  interpretQuery query_authForAccount table_AccountAuth
