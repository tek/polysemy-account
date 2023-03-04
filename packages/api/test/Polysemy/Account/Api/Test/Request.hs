module Polysemy.Account.Api.Test.Request where

import qualified Data.CaseInsensitive as CaseInsensitive
import Exon.Quote (exon)
import Network.Wai (Request, requestHeaders, requestMethod)
import Network.Wai.Test (SRequest (SRequest), SResponse, Session, defaultRequest, runSession, setPath, srequest)
import Polysemy.Db (interpretIdNumFrom)
import Polysemy.Test (TestError (TestError), UnitTest)
import Prelude hiding (get, put)
import Servant (
  Context (EmptyContext, (:.)),
  DefaultErrorFormatters,
  ErrorFormatters,
  HasContextEntry,
  HasServer,
  ServerError,
  ServerT,
  type (.++),
  )
import Servant.Auth.Server (IsSecure (NotSecure), cookieIsSecure, defaultCookieSettings)
import Sqel (Uid)
import Zeugma (TestStack, resumeTest, runTestFrozen)

import Polysemy.Account.Accounts (register)
import qualified Polysemy.Account.Api.Effect.Jwt as Jwt
import Polysemy.Account.Api.Effect.Jwt (Jwt)
import Polysemy.Account.Api.Interpreter.Authorize (interpretAuthorizeP)
import Polysemy.Account.Api.Interpreter.Jwt (interpretJwt)
import Polysemy.Account.Api.Native (AuthContext)
import Polysemy.Account.Api.NativeContext (runServerSem)
import Polysemy.Account.Api.Test.Data.Request (Headers, Method, methodUpper)
import qualified Polysemy.Account.Api.Test.Effect.TestClient as TestClient
import Polysemy.Account.Api.Test.Effect.TestClient (TestClient)
import Polysemy.Account.Data.Account (Account)
import Polysemy.Account.Data.AccountAuth (AccountAuth)
import Polysemy.Account.Data.AccountCredentials (AccountCredentials (AccountCredentials))
import qualified Polysemy.Account.Data.AccountStatus as AccountStatus
import Polysemy.Account.Data.AccountsError (AccountsError)
import Polysemy.Account.Data.AuthToken (AuthToken (AuthToken))
import Polysemy.Account.Data.AuthedAccount (AuthedAccount (AuthedAccount))
import Polysemy.Account.Data.Privilege (Privilege (Admin, Web), Privileges, RequiredPrivileges)
import Polysemy.Account.Data.RawPassword (rawPassword)
import qualified Polysemy.Account.Effect.Accounts as Accounts
import Polysemy.Account.Effect.Accounts (Accounts)
import Polysemy.Account.Effect.Authorize (Authorize)
import Polysemy.Account.Interpreter.Accounts (interpretAccountsState)

type ServerCtx (api :: Type) context =
  (
    HasServer api context,
    HasContextEntry (context .++ DefaultErrorFormatters) ErrorFormatters
  )

type Server (api :: Type) context r =
  (
    Members [Log, Error Text, Embed IO, Final IO] r,
    ServerCtx api context
  )

type TestServer (api :: Type) r =
  ServerT api (Sem (Stop ServerError : r))

req :: Text -> Headers -> Request
req path headers =
  setPath defaultRequest { requestHeaders = (first CaseInsensitive.mk <$> headers) } . encodeUtf8 $ path

runSessionSem ::
  ∀ (api :: Type) context r a .
  Server api context r =>
  TestServer api r ->
  Context context ->
  Session a ->
  Sem r a
runSessionSem srv context =
  runServerSem @api srv context . runSession

runSessionSemJwtCtx ::
  ∀ (api :: Type) ctx i p r a .
  Member (Jwt (AuthedAccount i p) !! ()) r =>
  Server api (AuthContext ++ ctx) r =>
  Context ctx ->
  TestServer api r ->
  Session a ->
  Sem r a
runSessionSemJwtCtx ctx srv session = do
  jwtSettings <- Jwt.settings @(AuthedAccount i p) !>> throw "jwt"
  runSessionSem @api @(AuthContext ++ ctx) srv (context jwtSettings) session
  where
    context jwtSettings =
      jwtSettings :. defaultCookieSettings { cookieIsSecure = NotSecure } :. ctx

runSessionSemJwt ::
  ∀ (api :: Type) i p r a .
  Member (Jwt (AuthedAccount i p) !! ()) r =>
  Server api AuthContext r =>
  TestServer api r ->
  Session a ->
  Sem r a
runSessionSemJwt =
  runSessionSemJwtCtx @api EmptyContext

jsonType :: (ByteString, ByteString)
jsonType =
  ("content-type", "application/json")

authHeader :: (ByteString, ByteString)
authHeader =
  ("authorization", "Basic QWxhZGRpbjpPcGVuU2VzYW1l")

reqJson :: Method -> Text -> Headers -> LByteString -> SRequest
reqJson method path headers body =
  SRequest (req path (jsonType : headers)) { requestMethod = methodUpper method } body

defaultAccount :: AuthedAccount Int Privileges
defaultAccount =
  AuthedAccount 1 1 "user" AccountStatus.Active [Web]

run ::
  ∀ (api :: Type) ctx r .
  Member (Jwt (AuthedAccount Int Privileges) !! ()) r =>
  Server api (AuthContext ++ ctx) r =>
  Context ctx ->
  TestServer api r ->
  Method ->
  Text ->
  Headers ->
  LByteString ->
  Sem r SResponse
run ctx srv method path headers body =
  runSessionSemJwtCtx @api ctx srv (srequest (reqJson method path headers body))

makeAccount ::
  Members [Accounts Int Privileges !! AccountsError, Jwt (AuthedAccount Int Privileges) !! (), Error TestError] r =>
  AccountCredentials ->
  Privileges ->
  Sem r (Text, (ByteString, ByteString))
makeAccount creds privs =
  resumeTest @AccountsError do
    root <- register creds
    let i = root ^. #id
    Accounts.setStatus i AccountStatus.Active
    Accounts.updatePrivileges i (const privs)
    AuthToken token <- Jwt.makeToken root !>> throw (TestError "jwt")
    pure (token, ("authorization", [exon|Bearer #{encodeUtf8 token}|]))

interpretTestClientCtx ::
  ∀ (api :: Type) ctx r .
  Members [Accounts Int Privileges !! AccountsError, Jwt (AuthedAccount Int Privileges) !! (), Error TestError] r =>
  Server api (AuthContext ++ ctx) r =>
  Context ctx ->
  TestServer api r ->
  InterpreterFor TestClient r
interpretTestClientCtx ctx server =
  interpret \case
    TestClient.Request method path headers body ->
      run @api ctx server method path headers body
    TestClient.MakeAdmin ->
      makeAccount (AccountCredentials "root" (rawPassword "root")) [Admin]
    TestClient.MakeUser ->
      makeAccount (AccountCredentials "user" (rawPassword "user")) [Web]

interpretTestClient ::
  ∀ (api :: Type) r .
  Members [Accounts Int Privileges !! AccountsError, Jwt (AuthedAccount Int Privileges) !! (), Error TestError] r =>
  Server api AuthContext r =>
  TestServer api r ->
  InterpreterFor TestClient r
interpretTestClient =
  interpretTestClientCtx @api EmptyContext

type TestEffects =
  [
    Authorize Int RequiredPrivileges Privileges,
    Accounts Int Privileges !! AccountsError,
    Stop ServerError,
    Jwt (AuthedAccount Int Privileges) !! (),
    Error Text
  ]

interpretAccounts ::
  Members [Log, Error TestError, Embed IO] r =>
  [Uid Int (Account Privileges)] ->
  [Uid Int (AccountAuth Int)] ->
  InterpreterFor (Accounts Int Privileges !! AccountsError) r
interpretAccounts accounts auths =
  mapError TestError .
  interpretIdNumFrom 3 .
  interpretAccountsState def accounts auths .
  raiseUnder3

runServer ::
  Members [Error TestError, Stop Text, Log, Resource, Async, Race, Embed IO] r =>
  [Uid Int (Account Privileges)] ->
  [Uid Int (AccountAuth Int)] ->
  InterpretersFor TestEffects r
runServer accounts auths =
  mapError TestError .
  raiseResumable interpretJwt .
  showStop @ServerError .
  interpretAccounts accounts auths .
  interpretAuthorizeP

runApiTestCtx ::
  ∀ (api :: Type) ctx .
  ServerCtx api (AuthContext ++ ctx) =>
  Context ctx ->
  [Uid Int (Account Privileges)] ->
  [Uid Int (AccountAuth Int)] ->
  TestServer api (TestEffects ++ TestStack) ->
  Sem (TestClient : TestEffects ++ TestStack) () ->
  UnitTest
runApiTestCtx ctx accounts auths server =
  runTestFrozen .
  runServer accounts auths .
  interpretTestClientCtx @api ctx server

runApiTest ::
  ∀ (api :: Type) .
  ServerCtx api AuthContext =>
  [Uid Int (Account Privileges)] ->
  [Uid Int (AccountAuth Int)] ->
  TestServer api (TestEffects ++ TestStack) ->
  Sem (TestClient : TestEffects ++ TestStack) () ->
  UnitTest
runApiTest =
  runApiTestCtx @api EmptyContext
