module Polysemy.Account.Api.Test.AccountByNameTest where

import qualified Polysemy.Db as Db
import Polysemy.Db (Query)
import Polysemy.Db.Data.DbError (DbError)
import qualified Polysemy.Db.Effect.Store as Store
import Polysemy.Db.Effect.Store (QStore)
import Polysemy.Hasql.Interpreter.DbTable (interpretDbTable)
import Polysemy.Hasql.Interpreter.Store (interpretStoreDb)
import Polysemy.Hasql.Test.Run (integrationTest)
import Polysemy.Test (UnitTest, assertJust)
import Sqel (Uid (Uid), primAs)
import qualified Sqel.Data.Uid as Uid
import Sqel.Query (checkQuery)

import Polysemy.Account.Data.Account (Account (Account))
import Polysemy.Account.Data.AccountByName (AccountByName (AccountByName))
import Polysemy.Account.Data.Privilege (Privilege (Api, Web))
import Polysemy.Account.Data.AccountStatus (AccountStatus (Active))
import qualified Polysemy.Account.Db.Dd as Dd
import Polysemy.Account.Db.Interpreter.AccountByName (interpretQueryAccountPByNameDb)

test_accountByName :: UnitTest
test_accountByName =
  integrationTest "polysemy_account_db" "polysemy-account" $
  interpretDbTable ts $
  interpretStoreDb ts (checkQuery (primAs @"id") Dd.accountP) $
  interpretQueryAccountPByNameDb do
    restop @DbError @(QStore _ _ _) $ restop @DbError @(Query _ _) do
      Store.insert (Uid (u 1) (Account "user1" Active [Web, Api]))
      Store.insert (Uid (u 2) (Account "user2" Active [Web, Api]))
      Store.insert user3
      assertJust user3 =<< Db.query (AccountByName "user3")
    unit
  where
    user3 = Uid (u 3) (Account "user3" Active [Web, Api])
    u = Uid.intUUID
    ts = Dd.accountSchemaP
