module Polysemy.Account.Api.Test.AccountWebTest where

import qualified Data.Aeson as Aeson
import Exon (exon)
import Network.HTTP.Types (Status (Status))
import Network.Wai.Test (SResponse (SResponse))
import Polysemy.Db (Id)
import Polysemy.Db (Query)
import Polysemy.Test (UnitTest, assertRight, (===))
import Sqel (Uid (Uid))

import Polysemy.Account.Api.Routes (AuthApiP)
import Polysemy.Account.Api.Server.Auth (authServer)
import Polysemy.Account.Api.Server.Error (ClientError (ClientError))
import Polysemy.Account.Api.Test.Data.Request (Method (Post))
import qualified Polysemy.Account.Api.Test.Effect.TestClient as TestClient
import Polysemy.Account.Api.Test.Effect.TestClient (TestClient)
import Polysemy.Account.Api.Test.Request (runApiTest)
import Polysemy.Account.Data.Account (Account (Account), AccountP)
import Polysemy.Account.Data.AccountAuth (AccountAuth (AccountAuth))
import Polysemy.Account.Data.AccountByName (AccountByName)
import Polysemy.Account.Data.AccountName (AccountName (AccountName))
import Polysemy.Account.Data.HashedPassword (HashedPassword (HashedPassword))
import qualified Polysemy.Account.Data.AccountStatus as AccountStatus
import Polysemy.Account.Data.AuthForAccount (AuthForAccount)
import Polysemy.Account.Data.Privilege (Privilege (Admin, Web))
import Polysemy.Account.Effect.Accounts (AccountsP)

user1 :: Text
user1 =
  "user1"

user2 :: Text
user2 =
  "user2"

password :: Text
password =
  "password1"

accounts :: [Uid Int AccountP]
accounts =
  [
    Uid 1 (Account (AccountName user1) AccountStatus.Active [Web]),
    Uid 2 (Account (AccountName user2) AccountStatus.Pending [Web]),
    Uid 3 (Account "user3" AccountStatus.Active [Admin, Web])
  ]

auths :: [Uid Int (AccountAuth Int)]
auths =
  [
    Uid 1 (AccountAuth 2 "desc" (HashedPassword password) Nothing),
    Uid 2 (AccountAuth 2 "desc" (HashedPassword password) Nothing),
    Uid 3 (AccountAuth 1 "desc" (HashedPassword password) Nothing)
  ]

type Api = AuthApiP Int

type AccountQuery = Query AccountByName (Maybe (Uid Int AccountP)) !! ()

type AuthQuery = Query (AuthForAccount Int) [Uid Int (AccountAuth Int)] !! ()

type Effects =
  [
    AccountsP Int,
    Id Int,
    Log
  ]

-- server ::
--   Members [Jwt (AuthedAccount Int [Privilege]) !! (), Async, Race, Resource] r =>
--   Server (AuthApiP Int) [JWTSettings, CookieSettings] r =>
--   TestServer (AuthApiP Int) r
-- server =
--   hoistServerWithContext (Proxy @(AuthApiP Int)) ctx interpretEndpoint (authServer @Int)
--   where
--     ctx =
--       Proxy @[JWTSettings, CookieSettings]

target :: Text
target =
  [exon|"token"|]

postAccountBody :: LByteString
postAccountBody =
  [exon|{"username":"#{encodeUtf8 user1}","password":"#{encodeUtf8 password}"}|]

post ::
  Member TestClient r =>
  Text ->
  LByteString ->
  Sem r SResponse
post endpoint =
  TestClient.request Post [exon|auth/#{endpoint}|] []

test_loginUser :: UnitTest
test_loginUser =
  runApiTest @(AuthApiP Int) accounts auths authServer do
    SResponse (Status code _) _ _ <- post "login" postAccountBody
    205 === code

postFailAccountBody :: LByteString
postFailAccountBody =
  [exon|{"username":"#{encodeUtf8 user1}","password":"wrong"}|]

test_failLoginUser :: UnitTest
test_failLoginUser =
  runApiTest @(AuthApiP Int) accounts auths authServer do
    SResponse (Status code _) _ _ <- post "login" postFailAccountBody
    401 === code

regBody :: LByteString
regBody =
  [exon|{"username":"new-user","password":"#{encodeUtf8 password}"}|]

test_registerUser :: UnitTest
test_registerUser =
  runApiTest @(AuthApiP Int) accounts auths authServer do
    SResponse (Status code _) _ _ <- post "register" regBody
    201 === code

regFailBody :: LByteString
regFailBody =
  [exon|{"username":"#{encodeUtf8 user1}","password":"#{encodeUtf8 password}"}|]

test_registerFailUser :: UnitTest
test_registerFailUser =
  runApiTest @(AuthApiP Int) accounts auths authServer do
    SResponse (Status code _) _ body <- post "register" regFailBody
    409 === code
    assertRight (ClientError "Multiple accounts with same name") (first toText (Aeson.eitherDecode body))
