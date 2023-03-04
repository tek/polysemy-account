{-# options_haddock prune #-}

-- | Interpreters for 'TestClient' and Servant API test runners.
module Polysemy.Account.Api.Test.Interpreter.TestClient where

import qualified Data.CaseInsensitive as CaseInsensitive
import Exon.Quote (exon)
import Lens.Micro.Extras (view)
import Log (Severity (Error))
import qualified Network.HTTP.Types.Status
import Network.Wai (Request, requestHeaders, requestMethod)
import qualified Network.Wai.Test as Wai
import Network.Wai.Test (SRequest (SRequest), Session, defaultRequest, setPath, srequest)
import Polysemy.Db (DbError, Id, interpretIdNumFrom)
import Polysemy.Test (TestError (TestError), UnitTest)
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
import Zeugma (TestStack, resumeTest, runTestLevel)

import Polysemy.Account.Accounts (register)
import qualified Polysemy.Account.Api.Effect.Jwt as Jwt
import Polysemy.Account.Api.Effect.Jwt (Jwt)
import Polysemy.Account.Api.Interpreter.Authorize (interpretAuthorizeP)
import Polysemy.Account.Api.Interpreter.Jwt (interpretJwt)
import Polysemy.Account.Api.Native (AuthContext)
import Polysemy.Account.Api.NativeContext (runServerSem)
import Polysemy.Account.Api.Test.Data.Request (Headers, Method, methodUpper)
import qualified Polysemy.Account.Api.Test.Effect.TestClient as TestClient
import Polysemy.Account.Api.Test.Effect.TestClient (Response (Response), TestClient)
import Polysemy.Account.Data.Account (Account)
import Polysemy.Account.Data.AccountAuth (AccountAuth)
import Polysemy.Account.Data.AccountCredentials (AccountCredentials)
import qualified Polysemy.Account.Data.AccountStatus as AccountStatus
import Polysemy.Account.Data.AccountsConfig (AccountsConfig)
import Polysemy.Account.Data.AccountsError (AccountsError)
import Polysemy.Account.Data.AuthToken (AuthToken (AuthToken))
import qualified Polysemy.Account.Data.AuthedAccount as AuthedAccount
import Polysemy.Account.Data.AuthedAccount (AuthedAccount (AuthedAccount))
import Polysemy.Account.Data.Privilege (Privilege (Web), Privileges, RequiredPrivileges)
import qualified Polysemy.Account.Effect.Accounts as Accounts
import Polysemy.Account.Effect.Accounts (Accounts)
import Polysemy.Account.Effect.Authorize (Authorize)
import Polysemy.Account.Effect.Password (Password)
import Polysemy.Account.Interpreter.Accounts (interpretAccountsState)

-- | Constraints for a Servant API with error formatting context.
type ServerCtx (api :: Type) context =
  (
    HasServer api context,
    HasContextEntry (context .++ DefaultErrorFormatters) ErrorFormatters
  )

-- | Constraints for a Servent API test runner.
type TestServer (api :: Type) context r =
  (
    Members [Log, Error TestError, Embed IO, Final IO] r,
    ServerCtx api context
  )

-- | 'ServerT' specialized to 'Sem'.
type TestServerT (api :: Type) r =
  ServerT api (Sem (Stop ServerError : r))

req :: Text -> Headers -> Request
req path headers =
  setPath defaultRequest { requestHeaders = (first CaseInsensitive.mk <$> headers) } . encodeUtf8 $ path

runSession ::
  ∀ (api :: Type) context r a .
  TestServer api context r =>
  TestServerT api r ->
  Context context ->
  Session a ->
  Sem r a
runSession srv context =
  runServerSem @api srv context . Wai.runSession

runSessionJwtCtx ::
  ∀ (api :: Type) ctx i p r a .
  Member (Jwt (AuthedAccount i p) !! ()) r =>
  TestServer api (AuthContext ++ ctx) r =>
  Context ctx ->
  TestServerT api r ->
  Session a ->
  Sem r a
runSessionJwtCtx ctx srv session = do
  jwtSettings <- Jwt.settings @(AuthedAccount i p) !>> throw "jwt"
  runSession @api @(AuthContext ++ ctx) srv (context jwtSettings) session
  where
    context jwtSettings =
      jwtSettings :. defaultCookieSettings { cookieIsSecure = NotSecure } :. ctx

runSessionJwt ::
  ∀ (api :: Type) i p r a .
  Member (Jwt (AuthedAccount i p) !! ()) r =>
  TestServer api AuthContext r =>
  TestServerT api r ->
  Session a ->
  Sem r a
runSessionJwt =
  runSessionJwtCtx @api EmptyContext

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
  ∀ (api :: Type) ctx i p r .
  Member (Jwt (AuthedAccount i p) !! ()) r =>
  TestServer api (AuthContext ++ ctx) r =>
  Context ctx ->
  TestServerT api r ->
  Method ->
  Text ->
  Headers ->
  LByteString ->
  Sem r Response
run ctx srv method path headers body = do
  res <- runSessionJwtCtx @api ctx srv (srequest (reqJson method path headers body))
  pure (Response res.simpleStatus.statusCode res.simpleHeaders res.simpleBody)

makeToken ::
  ∀ i p r .
  Members [Accounts i p !! AccountsError, Jwt (AuthedAccount i p) !! (), Error TestError] r =>
  AuthedAccount i p ->
  Sem r (Text, (ByteString, ByteString))
makeToken acc =
  resumeTest @AccountsError @(Accounts _ _) do
    Accounts.setStatus acc.id AccountStatus.Active
    AuthToken token <- Jwt.makeToken acc !>> throw (TestError "jwt")
    pure (token, ("authorization", [exon|Bearer #{encodeUtf8 token}|]))

makeUser ::
  Members [Accounts i p !! AccountsError, Jwt (AuthedAccount i p) !! (), Error TestError] r =>
  AccountCredentials ->
  Sem r (Text, (ByteString, ByteString))
makeUser creds = do
  acc <- resumeTest @AccountsError (register creds)
  makeToken acc

-- | Interpret 'TestClient' using a Servant server, run via @wai-test@ in-memory.
interpretTestClient ::
  ∀ (api :: Type) ctx i p r .
  Member (Error TestError) r =>
  Members [Accounts i p !! AccountsError, Jwt (AuthedAccount i p) !! (), Reader (AccountsConfig p) !! DbError] r =>
  TestServer api (AuthContext ++ ctx) r =>
  Context ctx ->
  TestServerT api r ->
  InterpreterFor (TestClient i p) r
interpretTestClient ctx server =
  interpret \case
    TestClient.RawRequest method path headers body ->
      run @api ctx server method path headers body
    TestClient.MakeToken acc ->
      makeToken acc
    TestClient.MakeUser creds -> do
      acc <- resumeTest @AccountsError (register creds)
      makeToken acc

-- | Interpret 'TestClient' with a test server, using the given Servant context.
interpretTestServer ::
  ∀ (api :: Type) ctx i p r .
  ToJSON i =>
  ToJSON p =>
  ServerCtx api (AuthContext ++ ctx) =>
  Members [Accounts i p !! AccountsError, Reader (AccountsConfig p) !! DbError] r =>
  Members [Id i, Error TestError, Log, Embed IO, Final IO] r =>
  Context ctx ->
  TestServerT api (Jwt (AuthedAccount i p) !! () : r) ->
  InterpretersFor [TestClient i p, Jwt (AuthedAccount i p) !! ()] r
interpretTestServer ctx server =
  mapError TestError .
  raiseResumable interpretJwt .
  raiseUnder .
  interpretTestClient @api ctx server

-- | Effects used by the interpreter for 'TestClient'.
type TestEffects i p =
  [
    Jwt (AuthedAccount i p) !! (),
    Accounts i p,
    Accounts i p !! AccountsError,
    Password,
    Reader (AccountsConfig p) !! DbError
  ]

-- | Interpret 'TestClient' and 'Accounts' with a test server, using the given Servant context and account data.
--
-- It is recommended to create local specializations of this function for your app's types and stack.
interpretTestServerAccounts ::
  ∀ (api :: Type) ctx i p r .
  Ord i =>
  Show i =>
  ToJSON i =>
  ToJSON p =>
  ServerCtx api (AuthContext ++ ctx) =>
  Members [Id i, Error TestError, Log, Embed IO, Final IO] r =>
  Context ctx ->
  TestServerT api (TestEffects i p ++ r) ->
  AccountsConfig p ->
  [Uid i (Account p)] ->
  [Uid i (AccountAuth i)] ->
  InterpretersFor (TestClient i p : TestEffects i p) r
interpretTestServerAccounts ctx server conf accounts auths =
  interpretAccountsState conf accounts auths .
  resumeTest .
  interpretTestServer @api ctx server

apiTest :: ()
apiTest =
  undefined

-- | The stack used by the basic test runner.
type ApiTestStack =
  TestEffects Int Privileges ++ Authorize Int RequiredPrivileges Privileges : Id Int : TestStack

-- | Run the lower server test stack specialized to 'Int' and 'Privileges', using the given log level.
runTestPLevel ::
  Severity ->
  Int ->
  Sem (Authorize Int RequiredPrivileges Privileges : Id Int : TestStack) () ->
  UnitTest
runTestPLevel level idStart =
  runTestLevel level .
  interpretIdNumFrom idStart .
  interpretAuthorizeP

-- | Run the lower server test stack specialized to 'Int' and 'Privileges'.
runTestP ::
  Int ->
  Sem (Authorize Int RequiredPrivileges Privileges : Id Int : TestStack) () ->
  UnitTest
runTestP =
  runTestPLevel Error

-- | Run a basic API test with the specified server, log level, and accounts.
--
-- This uses the default config and a JWT context.
--
-- The ID and privileges types are fixed as 'Int' and 'Privileges'.
-- If you want to use a different types or additional contexts, or run additional interpreters, use
-- 'interpretTestServer' and 'Zeugma.runTest' manually.
runApiTestLevel ::
  ∀ (api :: Type) .
  ServerCtx api AuthContext =>
  Severity ->
  TestServerT api ApiTestStack ->
  [Uid Int (Account Privileges)] ->
  [Uid Int (AccountAuth Int)] ->
  Sem (TestClient Int Privileges : ApiTestStack) () ->
  UnitTest
runApiTestLevel level server accounts auths =
  runTestPLevel level (fromMaybe 0 (maximum ((view #id <$> accounts) ++ (view #id <$> auths))) + 1) .
  interpretTestServerAccounts @api EmptyContext server def accounts auths

-- | Run a basic API test with the specified server.
--
-- This uses the default config and a JWT context.
--
-- The ID and privileges types are fixed as 'Int' and 'Privileges'.
-- If you want to use a different types or additional contexts, or run additional interpreters, use
-- 'interpretTestServer' and 'Zeugma.runTest' manually.
runApiTest ::
  ∀ (api :: Type) .
  ServerCtx api AuthContext =>
  TestServerT api ApiTestStack ->
  [Uid Int (Account Privileges)] ->
  [Uid Int (AccountAuth Int)] ->
  Sem (TestClient Int Privileges : ApiTestStack) () ->
  UnitTest
runApiTest =
  runApiTestLevel @api Error
