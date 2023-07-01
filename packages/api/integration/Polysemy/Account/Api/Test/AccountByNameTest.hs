module Polysemy.Account.Api.Test.AccountByNameTest where

import qualified Polysemy.Db as Db
import Polysemy.Db (DbError, QStore, Query)
import qualified Polysemy.Db.Effect.Store as Store
import Polysemy.Hasql (interpretStoreDb, interpretTable)
import Polysemy.Hasql.Test.Run (integrationTest)
import Polysemy.Test (UnitTest, assertJust)
import Sqel (Uid (Uid), query_UUID)
import Sqel.Exts (intUUID)

import Polysemy.Account.Api.Db.Dd (table_AccountP)
import Polysemy.Account.Api.Db.Interpreter.AccountByName (interpretQueryAccountPByNameDb)
import Polysemy.Account.Data.Account (Account (Account))
import Polysemy.Account.Data.AccountByName (AccountByName (AccountByName))
import Polysemy.Account.Data.AccountStatus (AccountStatus (Active))
import Polysemy.Account.Data.Privilege (Privilege (Api, Web))

test_accountByName :: UnitTest
test_accountByName =
  integrationTest "polysemy_account" $
  interpretTable table_AccountP $
  interpretStoreDb query_UUID table_AccountP $
  interpretQueryAccountPByNameDb do
    restop @DbError @(QStore _ _ _) $ restop @DbError @(Query _ _) do
      Store.insert (Uid (u 1) (Account "user1" Active [Web, Api]))
      Store.insert (Uid (u 2) (Account "user2" Active [Web, Api]))
      Store.insert user3
      assertJust user3 =<< Db.query (AccountByName "user3")
    unit
  where
    user3 = Uid (u 3) (Account "user3" Active [Web, Api])
    u = intUUID
