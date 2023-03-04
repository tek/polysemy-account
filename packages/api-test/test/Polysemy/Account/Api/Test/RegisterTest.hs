module Polysemy.Account.Api.Test.RegisterTest where

import qualified Data.Aeson as Aeson
import Exon (exon)
import Polysemy.Test (TestError (TestError), UnitTest, (===))
import Servant (ServerT, (:<|>) ((:<|>)))
import Servant.Server (ServerError)
import Sqel (Uid (Uid))
import Zeugma (resumeTest)

import Polysemy.Account.Accounts (registerAdmin)
import Polysemy.Account.Api.Effect.Jwt (Jwt)
import Polysemy.Account.Api.Server.Account (accountServer)
import Polysemy.Account.Api.Server.Auth (authServer)
import Polysemy.Account.Api.Test.Data.Request (Method (Get, Post, Put))
import qualified Polysemy.Account.Api.Test.Effect.TestClient as TestClient
import Polysemy.Account.Api.Test.Effect.TestClient (Response (Response), rawRequest)
import Polysemy.Account.Api.Test.Interpreter.TestClient (runApiTest)
import Polysemy.Account.Data.Account (Account)
import Polysemy.Account.Data.AccountCredentials (AccountCredentials (AccountCredentials))
import qualified Polysemy.Account.Data.AccountStatus as AccountStatus
import Polysemy.Account.Data.AccountsError (AccountsError)
import Polysemy.Account.Data.AuthedAccount (AuthedAccount)
import Polysemy.Account.Data.Privilege (Privileges)
import Polysemy.Account.Data.RawPassword (rawPassword)
import Polysemy.Account.Effect.Accounts (Accounts)
import Polysemy.Account.Effect.Authorize (AuthorizeP)
import Polysemy.Account.Routes (AccountApi, AuthApi)

type TestApi =
  AccountApi Int Privileges :<|> AuthApi Int Privileges

type TestEffects =
  [
    Accounts Int Privileges,
    Stop ServerError,
    Jwt (AuthedAccount Int Privileges) !! (),
    Error Text
  ]

testServer ::
  Members [Accounts Int Privileges !! AccountsError, AuthorizeP Int] r =>
  Members [Jwt (AuthedAccount Int Privileges) !! (), Log, Stop ServerError] r =>
  ServerT TestApi (Sem r)
testServer =
  accountServer :<|> authServer

username :: Text
username =
  "user-1"

password :: Text
password =
  "dog"

registerPayload :: LByteString
registerPayload =
  [exon|{"username": "#{encodeUtf8 username}", "password": "#{encodeUtf8 password}"}|]

test_register :: UnitTest
test_register =
  runApiTest @TestApi testServer [] [] $ resumeTest @AccountsError do
    root <- registerAdmin (AccountCredentials "root" (rawPassword "root"))
    (_, rootAuth) <- TestClient.makeToken root
    Response statusCreated _ _ <- rawRequest Post "auth/register" [] registerPayload
    statusCreated === 201
    Response _ _ (Aeson.eitherDecode -> decodeResult) <- rawRequest Get "account/3" [rootAuth] ""
    Uid (accountId :: Int) (account :: Account Privileges) <- either (throw . TestError . show) pure decodeResult
    let updated = account & #status .~ AccountStatus.Active
    _ <- rawRequest Put [exon|account/#{show accountId}|] [rootAuth] (Aeson.encode updated)
    Response statusReset _ _ <- rawRequest Post "auth/login" [] registerPayload
    statusReset === 205
