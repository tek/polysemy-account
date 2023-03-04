{-# options_haddock prune #-}

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
import Polysemy.Account.Api.Test.Interpreter.TestClient (ServerCtx, TestServerT, interpretTestClient)
import Polysemy.Account.Data.Account (Account)
import Polysemy.Account.Data.AccountAuth (AccountAuth)
import Polysemy.Account.Data.AccountsConfig (AccountsConfig)
import Polysemy.Account.Data.AccountsError (AccountsError)
import Polysemy.Account.Data.AuthedAccount (AuthedAccount)
import Polysemy.Account.Data.Privilege (DefaultPrivileges, Privileges, RequiredPrivileges)
import Polysemy.Account.Effect.Accounts (Accounts)
import Polysemy.Account.Effect.Authorize (Authorize)
import Polysemy.Account.Effect.Password (Password)
import Polysemy.Account.Interpreter.Accounts (interpretAccountsState)

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

-- | Interpret 'TestClient' and 'Accounts' with a test server using the given Servant context and config.
apiTestWith ::
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
  InterpretersFor (TestClient i p : TestEffects i p) r
apiTestWith ctx server conf =
  interpretTestServerAccounts @api ctx server conf [] []

-- | Interpret 'TestClient' and 'Accounts' with a test server using the default config.
apiTest ::
  ∀ (api :: Type) i p r .
  Ord i =>
  Show i =>
  ToJSON i =>
  ToJSON p =>
  DefaultPrivileges p =>
  HasServer api AuthContext =>
  Members [Id i, Error TestError, Log, Embed IO, Final IO] r =>
  TestServerT api (TestEffects i p ++ r) ->
  InterpretersFor (TestClient i p : TestEffects i p) r
apiTest server =
  interpretTestServerAccounts @api EmptyContext server def [] []

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
