-- | Description: Test request functions.
module Polysemy.Account.Api.Test.Request where

import qualified Sqel
import Sqel (Uid (Uid))
import Zeugma (TestError (TestError), resumeTest)

import Polysemy.Account.Accounts (register)
import Polysemy.Account.Api.Test.Data.Request (Headers, Method)
import qualified Polysemy.Account.Api.Test.Effect.TestClient as TestClient
import Polysemy.Account.Api.Test.Effect.TestClient (Response, TestClient)
import qualified Polysemy.Account.Data.Account as Account
import Polysemy.Account.Data.Account (Account)
import Polysemy.Account.Data.AccountCredentials (AccountCredentials (AccountCredentials))
import Polysemy.Account.Data.AccountStatus (AccountStatus (Active))
import Polysemy.Account.Data.AccountsError (AccountsError)
import qualified Polysemy.Account.Data.AuthedAccount as AuthedAccount
import Polysemy.Account.Data.AuthedAccount (AuthedAccount)
import Polysemy.Account.Data.RawPassword (rawPassword)
import qualified Polysemy.Account.Effect.Accounts as Accounts
import Polysemy.Account.Effect.Accounts (Accounts)

-- | Make an authenticated test request with the given 'AuthedAccount'.
requestWithAuth ::
  Members [TestClient i p, Accounts i p !! AccountsError, Error TestError] r =>
  AuthedAccount i p ->
  Method ->
  Text ->
  Headers ->
  LByteString ->
  Sem r Response
requestWithAuth authed method path headers body = do
  (_, authHeader) <- TestClient.makeToken authed
  TestClient.rawRequest method path (authHeader : headers) body

-- | Make an authenticated test request with the account corresponding to the given ID.
requestWithId ::
  Members [TestClient i p, Accounts i p !! AccountsError, Error TestError] r =>
  i ->
  Method ->
  Text ->
  Headers ->
  LByteString ->
  Sem r Response
requestWithId i method path headers body = do
  authed <- resumeTest do
    auth <- note (TestError "No auths for this account") . head =<< Accounts.auths i
    Accounts.authed auth.id
  requestWithAuth authed method path headers body

-- | Make an authenticated test request with the given 'Account'.
requestWith ::
  Members [TestClient i p, Accounts i p !! AccountsError, Error TestError] r =>
  Account p ->
  Method ->
  Text ->
  Headers ->
  LByteString ->
  Sem r Response
requestWith account method path headers body = do
  Uid i _ <- resumeTest (Accounts.byName account.name)
  requestWithId i method path headers body

-- | Make an authenticated test request with the default account @user@, creating it if necessary.
request ::
  Members [TestClient i p, Accounts i p !! AccountsError, Error TestError] r =>
  Method ->
  Text ->
  LByteString ->
  Sem r Response
request method path body = do
  i <- ((.id) <$> Accounts.byName "user") !>> resumeTest do
    acc <- register (AccountCredentials "user" (rawPassword "user"))
    Accounts.setStatus acc.id Active
    pure acc.id
  requestWithId i method path [] body
