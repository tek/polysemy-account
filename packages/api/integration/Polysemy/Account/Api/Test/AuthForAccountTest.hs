module Polysemy.Account.Api.Test.AuthForAccountTest where

import Data.UUID (UUID)
import qualified Polysemy.Db as Db
import Polysemy.Db (DbError, QStore, Query)
import qualified Polysemy.Db.Effect.Store as Store
import Polysemy.Hasql (interpretStoreDb, interpretTable)
import Polysemy.Hasql.Test.Run (integrationTest)
import Polysemy.Test (UnitTest, assertEq)
import Sqel (Uid (Uid), Uuid, query_UUID)
import qualified Sqel.Data.Uid as Uid

import Polysemy.Account.Api.Db.Dd (table_AccountAuth)
import Polysemy.Account.Api.Db.Interpreter.AuthForAccount (interpretQueryAuthForAccountDb)
import Polysemy.Account.Data.AccountAuth (AccountAuth (AccountAuth))
import Polysemy.Account.Data.AuthForAccount (AuthForAccount (AuthForAccount))

test_authForAccount :: UnitTest
test_authForAccount =
  integrationTest "polysemy_account" $
  interpretTable table_AccountAuth $
  interpretStoreDb query_UUID table_AccountAuth $
  interpretQueryAuthForAccountDb do
    restop @DbError @(QStore _ _ _) $ restop @DbError @(Query _ _) do
      Store.insert (Uid (u 1) (AccountAuth (u 5) "account 1" "password 1" Nothing))
      Store.insert auth2
      Store.insert auth3
      assertEq target =<< Db.query (AuthForAccount (u 6))
  where
    target :: [Uuid (AccountAuth UUID)]
    target = [auth2, auth3]
    auth2 = Uid (u 2) (AccountAuth (u 6) "account 2" "password 2" Nothing)
    auth3 = Uid (u 3) (AccountAuth (u 6) "account 2" "password 3" Nothing)
    u = Uid.intUUID
