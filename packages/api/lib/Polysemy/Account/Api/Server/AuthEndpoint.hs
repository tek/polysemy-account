module Polysemy.Account.Api.Server.AuthEndpoint where

import Exon (exon)
import qualified Polysemy.Log as Log
import Servant (ServerError)
import Servant.Auth.Server (AuthResult (Authenticated))

import Polysemy.Account.Api.Error (unauthorized)
import qualified Polysemy.Account.Data.AccountStatus as AccountStatus
import Polysemy.Account.Data.AccountsError (AccountsError)
import Polysemy.Account.Data.AuthedAccount (AuthedAccount (AuthedAccount))
import Polysemy.Account.Data.Privilege (AccountIsAdmin (accountIsAdmin))
import qualified Polysemy.Account.Effect.Accounts as Accounts
import Polysemy.Account.Effect.Accounts (Accounts)

pattern Active :: AuthedAccount i p -> AuthedAccount i p
pattern Active acc <- acc@(AuthedAccount _ _ _ AccountStatus.Active _)

pattern Admin :: AccountIsAdmin p => () => AuthedAccount i p -> AuthedAccount i p
pattern Admin acc <- acc@(AuthedAccount _ _ _ _ (accountIsAdmin -> True))

insufficient ::
  Show (AuthedAccount i p) =>
  Members [Log, Stop ServerError] r =>
  AuthedAccount i p ->
  Text ->
  Sem r a
insufficient account extra = do
  Log.error [exon|auth with insufficient privileges: #{show account}#{extra}|]
  stop (unauthorized "insufficient privileges")

checkAccount ::
  Show (AuthedAccount i p) =>
  Members [Log, Stop ServerError] r =>
  (AuthedAccount i p -> Sem r (Maybe Text)) ->
  (AuthedAccount i p -> Sem r a) ->
  AuthedAccount i p ->
  Sem r a
checkAccount check f (Active account) =
  check account >>= \case
    Just msg ->
      insufficient account [exon| (#{msg})|]
    Nothing ->
      f account
checkAccount _ _ account =
  insufficient account ""

-- TODO use 500 for internal errors
-- TODO move to Accounts interpreter
authorize ::
  Show (AuthedAccount i p) =>
  Members [Accounts i p !! AccountsError, Log, Stop ServerError] r =>
  (AuthedAccount i p -> Sem r (Maybe Text)) ->
  (AuthedAccount i p -> Sem r a) ->
  AuthResult (AuthedAccount i p) ->
  Sem r a
authorize check f (Authenticated payload@(AuthedAccount _ authId _ _ _)) = do
  account <- Accounts.authed authId !>> insufficient payload " (token expired)"
  checkAccount check f account
authorize _ _ _ =
  stop (unauthorized "no valid auth data")

ensureUser ::
  Show (AuthedAccount i p) =>
  Members [Accounts i p !! AccountsError, Log, Stop ServerError] r =>
  (AuthedAccount i p -> Sem r a) ->
  AuthResult (AuthedAccount i p) ->
  Sem r a
ensureUser =
  authorize (const (pure Nothing))

ensureUser_ ::
  Show (AuthedAccount i p) =>
  Members [Accounts i p !! AccountsError, Log, Stop ServerError] r =>
  Sem r a ->
  AuthResult (AuthedAccount i p) ->
  Sem r a
ensureUser_ f =
  ensureUser (const f)

ensureAdmin ::
  Show (AuthedAccount i p) =>
  AccountIsAdmin p =>
  Members [Accounts i p !! AccountsError, Log, Stop ServerError] r =>
  (AuthedAccount i p -> Sem r a) ->
  AuthResult (AuthedAccount i p) ->
  Sem r a
ensureAdmin =
  authorize \case
    Admin _ -> pure Nothing
    _ -> pure (Just "Not an admin")

ensureAdmin_ ::
  Show (AuthedAccount i p) =>
  AccountIsAdmin p =>
  Members [Accounts i p !! AccountsError, Log, Stop ServerError] r =>
  Sem r a ->
  AuthResult (AuthedAccount i p) ->
  Sem r a
ensureAdmin_ f =
  ensureAdmin (const f)

accountOnly ::
  Show (AuthedAccount i p) =>
  Members [Accounts i p !! AccountsError, Log, Stop ServerError] r =>
  (AuthedAccount i p -> Sem r a) ->
  AuthResult (AuthedAccount i p) ->
  Sem r a
accountOnly f =
  ensureUser f

accountOnly_ ::
  Show (AuthedAccount i p) =>
  Members [Accounts i p !! AccountsError, Log, Stop ServerError] r =>
  Sem r a ->
  AuthResult (AuthedAccount i p) ->
  Sem r a
accountOnly_ f =
  ensureUser_ f

accountOnly1 ::
  Show (AuthedAccount i p) =>
  Members [Accounts i p !! AccountsError, Log, Stop ServerError] r =>
  (AuthedAccount i p -> a -> Sem r b) ->
  AuthResult (AuthedAccount i p) ->
  a ->
  Sem r b
accountOnly1 f authResult a =
  ensureUser (\ acc -> f acc a) authResult

accountOnly1_ ::
  Show (AuthedAccount i p) =>
  Members [Accounts i p !! AccountsError, Log, Stop ServerError] r =>
  (a -> Sem r b) ->
  AuthResult (AuthedAccount i p) ->
  a ->
  Sem r b
accountOnly1_ f authResult a =
  ensureUser_ (f a) authResult

accountOnly2 ::
  Show (AuthedAccount i p) =>
  Members [Accounts i p !! AccountsError, Log, Stop ServerError] r =>
  (AuthedAccount i p -> a -> b -> Sem r c) ->
  AuthResult (AuthedAccount i p) ->
  a ->
  b ->
  Sem r c
accountOnly2 f authResult a b =
  ensureUser (\ acc -> f acc a b) authResult

accountOnly2_ ::
  Show (AuthedAccount i p) =>
  Members [Accounts i p !! AccountsError, Log, Stop ServerError] r =>
  (a -> b -> Sem r c) ->
  AuthResult (AuthedAccount i p) ->
  a ->
  b ->
  Sem r c
accountOnly2_ f authResult a b =
  ensureUser_ (f a b) authResult

adminOnly ::
  Show (AuthedAccount i p) =>
  AccountIsAdmin p =>
  Members [Accounts i p !! AccountsError, Log, Stop ServerError] r =>
  (AuthedAccount i p -> Sem r a) ->
  AuthResult (AuthedAccount i p) ->
  Sem r a
adminOnly f =
  ensureAdmin f

adminOnly_ ::
  Show (AuthedAccount i p) =>
  AccountIsAdmin p =>
  Members [Accounts i p !! AccountsError, Log, Stop ServerError] r =>
  Sem r a ->
  AuthResult (AuthedAccount i p) ->
  Sem r a
adminOnly_ f =
  ensureAdmin_ f

adminOnly1 ::
  Show (AuthedAccount i p) =>
  AccountIsAdmin p =>
  Members [Accounts i p !! AccountsError, Log, Stop ServerError] r =>
  (AuthedAccount i p -> a -> Sem r b) ->
  AuthResult (AuthedAccount i p) ->
  a ->
  Sem r b
adminOnly1 f authResult a =
  ensureAdmin (\ acc -> f acc a) authResult

adminOnly1_ ::
  Show (AuthedAccount i p) =>
  AccountIsAdmin p =>
  Members [Accounts i p !! AccountsError, Log, Stop ServerError] r =>
  (a -> Sem r b) ->
  AuthResult (AuthedAccount i p) ->
  a ->
  Sem r b
adminOnly1_ f authResult a =
  ensureAdmin_ (f a) authResult

adminOnly2 ::
  Show (AuthedAccount i p) =>
  AccountIsAdmin p =>
  Members [Accounts i p !! AccountsError, Log, Stop ServerError] r =>
  (AuthedAccount i p -> a -> b -> Sem r c) ->
  AuthResult (AuthedAccount i p) ->
  a ->
  b ->
  Sem r c
adminOnly2 f authResult a b =
  ensureAdmin (\ acc -> f acc a b) authResult

adminOnly2_ ::
  Show (AuthedAccount i p) =>
  AccountIsAdmin p =>
  Members [Accounts i p !! AccountsError, Log, Stop ServerError] r =>
  (a -> b -> Sem r c) ->
  AuthResult (AuthedAccount i p) ->
  a ->
  b ->
  Sem r c
adminOnly2_ f authResult a b =
  ensureAdmin_ (f a b) authResult
