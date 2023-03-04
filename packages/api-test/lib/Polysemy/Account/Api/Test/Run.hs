-- | Auth-enabled Servant API test runners.
module Polysemy.Account.Api.Test.Run where

import Lens.Micro.Extras (view)
import Log (Severity (Error))
import Polysemy.Db (DbError, Id, interpretIdNumFrom)
import Polysemy.Test (TestError (TestError), UnitTest)
import Servant (Context (EmptyContext), HasServer)
import Sqel (Uid)
import Zeugma (TestStack, resumeTest, runTestLevel)

import Polysemy.Account.Api.Effect.Jwt (Jwt)
import Polysemy.Account.Api.Interpreter.Authorize (interpretAuthorizeP)
import Polysemy.Account.Api.Interpreter.Jwt (interpretJwt)
import Polysemy.Account.Api.Native (AuthContext)
import Polysemy.Account.Api.Test.Effect.TestClient (TestClient)
import Polysemy.Account.Api.Test.Interpreter.TestClient (ServerCtx, TestServer, TestServerT, interpretTestClient)
import Polysemy.Account.Data.Account (Account, AccountP)
import Polysemy.Account.Data.AccountAuth (AccountAuth)
import Polysemy.Account.Data.AccountsConfig (AccountsConfig, AccountsConfigP)
import Polysemy.Account.Data.AccountsError (AccountsError)
import Polysemy.Account.Data.AuthedAccount (AuthedAccount)
import Polysemy.Account.Data.Privilege (Privileges)
import Polysemy.Account.Effect.Accounts (Accounts)
import Polysemy.Account.Effect.Authorize (AuthorizeP)
import Polysemy.Account.Effect.Password (Password)
import Polysemy.Account.Interpreter.Accounts (interpretAccountsState)

-- | Effects used by the interpreter for 'TestClient'.
type TestEffects i p =
  [
    Jwt (AuthedAccount i p) !! (),
    Accounts i p,
    Accounts i p !! AccountsError,
    Password,
    Reader (AccountsConfig p) !! DbError
  ]

-- | Interpret 'TestClient' with the given context and server.
apiTestWith ::
  ∀ api ctx i p r .
  Members (TestEffects i p) r =>
  TestServer api (AuthContext ++ ctx) r =>
  Context ctx ->
  TestServerT api r ->
  InterpreterFor (TestClient i p) r
apiTestWith =
  interpretTestClient @api

-- | Interpret 'TestClient' with the given server.
apiTest ::
  ∀ api i p r .
  Members (TestEffects i p) r =>
  TestServer api AuthContext r =>
  TestServerT api r ->
  InterpreterFor (TestClient i p) r
apiTest =
  apiTestWith @api EmptyContext

-- | Interpret the accounts test stack , 'TestEffects', using the given account data.
interpretTestAccounts ::
  ∀ i p r .
  Ord i =>
  Show i =>
  ToJSON i =>
  ToJSON p =>
  Members [Id i, Error TestError, Log, Embed IO, Final IO] r =>
  AccountsConfig p ->
  [Uid i (Account p)] ->
  [Uid i (AccountAuth i)] ->
  InterpretersFor (TestEffects i p) r
interpretTestAccounts conf accounts auths =
  interpretAccountsState conf accounts auths .
  resumeTest .
  mapError TestError .
  raiseResumable interpretJwt .
  raiseUnder

-- | The stack used by the basic test runner.
type ApiTestStack =
  AuthorizeP Int : TestEffects Int Privileges ++ Id Int : TestStack

-- | Run the API test stack specialized to 'Int' and 'Privileges', using the given account data.
--
-- If you want to use a different ID type and 'Polysemy.Account.Authorize' interpreter, use 'Zeugma.runTest',
-- 'interpretTestAccounts' and 'apiTest'.
runTestAccountsP ::
  Severity ->
  AccountsConfigP ->
  [Uid Int AccountP] ->
  [Uid Int (AccountAuth Int)] ->
  Sem ApiTestStack () ->
  UnitTest
runTestAccountsP level conf accounts auths =
  runTestLevel level .
  interpretIdNumFrom startId .
  interpretTestAccounts conf accounts auths .
  interpretAuthorizeP
  where
    startId = fromMaybe 0 (maximum ((view #id <$> accounts) ++ (view #id <$> auths))) + 1

-- | Run the API test stack specialized to 'Int' and 'Privileges'.
--
-- This interprets 'Polysemy.Account.Authorize' with the default implementation.
runTestP ::
  Sem ApiTestStack () ->
  UnitTest
runTestP =
  runTestAccountsP Error def [] []

-- | Run a basic API test with the specified server, context, log level, and accounts.
--
-- This uses the default config.
--
-- The ID and privileges types are fixed as 'Int' and 'Privileges'.
-- If you want to use a different ID type and 'Polysemy.Account.Authorize' interpreter, use 'Zeugma.runTest',
-- 'interpretTestAccounts' and 'apiTest'.
runApiTestLevel ::
  ∀ (api :: Type) ctx .
  ServerCtx api (AuthContext ++ ctx) =>
  Severity ->
  Context ctx ->
  TestServerT api ApiTestStack ->
  [Uid Int AccountP] ->
  [Uid Int (AccountAuth Int)] ->
  Sem (TestClient Int Privileges : ApiTestStack) () ->
  UnitTest
runApiTestLevel level ctx server accounts auths =
  runTestAccountsP level def accounts auths .
  apiTestWith @api ctx server

-- | Run a basic API test with the specified server, context, and accounts.
--
-- This uses the default config.
--
-- The ID and privileges types are fixed as 'Int' and 'Privileges'.
-- If you want to use a different ID type and 'Polysemy.Account.Authorize' interpreter, use 'Zeugma.runTest',
-- 'interpretTestAccounts' and 'apiTest'.
runApiTestCtx ::
  ∀ (api :: Type) ctx .
  ServerCtx api (AuthContext ++ ctx) =>
  Context ctx ->
  TestServerT api ApiTestStack ->
  [Uid Int AccountP] ->
  [Uid Int (AccountAuth Int)] ->
  Sem (TestClient Int Privileges : ApiTestStack) () ->
  UnitTest
runApiTestCtx =
  runApiTestLevel @api Error

-- | Run a basic API test with the specified server and accounts.
--
-- This uses the default config and a JWT context.
--
-- The ID and privileges types are fixed as 'Int' and 'Privileges'.
-- If you want to use a different ID type and 'Polysemy.Account.Authorize' interpreter, use 'Zeugma.runTest',
-- 'interpretTestAccounts' and 'apiTest'.
runApiTestWith ::
  ∀ (api :: Type) .
  HasServer api AuthContext =>
  TestServerT api ApiTestStack ->
  [Uid Int AccountP] ->
  [Uid Int (AccountAuth Int)] ->
  Sem (TestClient Int Privileges : ApiTestStack) () ->
  UnitTest
runApiTestWith server accounts auths =
  runTestAccountsP Error def accounts auths .
  apiTest @api server

-- | Run a basic API test with the specified server.
--
-- This uses the default config and a JWT context.
--
-- The ID and privileges types are fixed as 'Int' and 'Privileges'.
-- If you want to use a different ID type and 'Polysemy.Account.Authorize' interpreter, use 'Zeugma.runTest',
-- 'interpretTestAccounts' and 'apiTest'.
runApiTest ::
  ∀ (api :: Type) .
  HasServer api AuthContext =>
  TestServerT api ApiTestStack ->
  Sem (TestClient Int Privileges : ApiTestStack) () ->
  UnitTest
runApiTest server =
  runApiTestLevel @api Error EmptyContext server [] []
