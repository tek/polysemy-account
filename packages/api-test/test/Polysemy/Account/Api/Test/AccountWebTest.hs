module Polysemy.Account.Api.Test.AccountWebTest where

import qualified Data.Aeson as Aeson
import Exon (exon)
import Polysemy.Test (UnitTest, assertRight, (===))
import Sqel (Uid (Uid))

import Polysemy.Account.Api.Server.Auth (authServer)
import Polysemy.Account.Api.Server.Error (ClientError (ClientError))
import Polysemy.Account.Api.Test.Data.Request (Method (Post))
import Polysemy.Account.Api.Test.Effect.TestClient (Response (Response), TestClientP, rawRequest)
import Polysemy.Account.Api.Test.Run (runApiTestWith)
import Polysemy.Account.Data.Account (Account (Account), AccountP)
import Polysemy.Account.Data.AccountAuth (AccountAuth (AccountAuth))
import Polysemy.Account.Data.AccountName (AccountName (AccountName))
import qualified Polysemy.Account.Data.AccountStatus as AccountStatus
import Polysemy.Account.Data.HashedPassword (HashedPassword (HashedPassword))
import Polysemy.Account.Data.Privilege (Privilege (Admin, Web))
import Polysemy.Account.Routes (AuthApiP)

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

target :: Text
target =
  [exon|"token"|]

postAccountBody :: LByteString
postAccountBody =
  [exon|{"username":"#{encodeUtf8 user1}","password":"#{encodeUtf8 password}"}|]

post ::
  Member (TestClientP i) r =>
  Text ->
  LByteString ->
  Sem r Response
post endpoint =
  rawRequest Post [exon|auth/#{endpoint}|] []

test_loginUser :: UnitTest
test_loginUser =
  runApiTestWith @(AuthApiP Int) authServer accounts auths do
    Response code _ _ <- post "login" postAccountBody
    205 === code

postFailAccountBody :: LByteString
postFailAccountBody =
  [exon|{"username":"#{encodeUtf8 user1}","password":"wrong"}|]

test_failLoginUser :: UnitTest
test_failLoginUser =
  runApiTestWith @(AuthApiP Int) authServer accounts auths do
    Response code _ _ <- post "login" postFailAccountBody
    401 === code

regBody :: LByteString
regBody =
  [exon|{"username":"new-user","password":"#{encodeUtf8 password}"}|]

test_registerUser :: UnitTest
test_registerUser =
  runApiTestWith @(AuthApiP Int) authServer accounts auths do
    Response code _ _ <- post "register" regBody
    201 === code

regFailBody :: LByteString
regFailBody =
  [exon|{"username":"#{encodeUtf8 user1}","password":"#{encodeUtf8 password}"}|]

test_registerFailUser :: UnitTest
test_registerFailUser =
  runApiTestWith @(AuthApiP Int) authServer accounts auths do
    Response code _ body <- post "register" regFailBody
    409 === code
    assertRight (ClientError "Multiple accounts with same name") (first toText (Aeson.eitherDecode body))
