module Polysemy.Account.Api.Test.AccountByNameTest where

import qualified Polysemy.Db as Db
import Polysemy.Db (DbError, QStore, Query)
import qualified Polysemy.Db.Effect.Store as Store
import Polysemy.Hasql (interpretStoreDb, interpretTable)
import Polysemy.Hasql.Test.Run (integrationTest)
import Polysemy.Test (UnitTest, assertJust)
import Sqel (Uid (Uid), primAs)
import qualified Sqel.Data.Uid as Uid
import Sqel.Query (checkQuery)

import Polysemy.Account.Data.Account (Account (Account))
import Polysemy.Account.Data.AccountByName (AccountByName (AccountByName))
import Polysemy.Account.Data.AccountStatus (AccountStatus (Active))
import Polysemy.Account.Data.Privilege (Privilege (Api, Web))
import qualified Polysemy.Account.Api.Db.Dd as Dd
import Polysemy.Account.Api.Db.Interpreter.AccountByName (interpretQueryAccountPByNameDb)

test_accountByName :: UnitTest
test_accountByName =
  integrationTest "polysemy_account_db" "polysemy-account" $
  interpretTable ts $
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
