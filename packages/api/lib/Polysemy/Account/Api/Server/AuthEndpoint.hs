{-# options_haddock prune #-}

-- | Combinators for guarding Servant handlers with authentication and authorization
module Polysemy.Account.Api.Server.AuthEndpoint where

import Exon (exon)
import qualified Polysemy.Log as Log
import Servant (ServerError)
import Servant.Auth.Server (AuthResult (Authenticated))

import Polysemy.Account.Api.Effect.Authorize (Authorize, authorize)
import qualified Polysemy.Account.Data.AccountStatus as AccountStatus
import Polysemy.Account.Data.AccountsError (AccountsError (Client, Internal))
import Polysemy.Account.Data.AuthedAccount (AuthedAccount (AuthedAccount))
import qualified Polysemy.Account.Data.Privilege as Privilege
import Polysemy.Account.Data.Privilege (Privilege)
import qualified Polysemy.Account.Effect.Accounts as Accounts
import Polysemy.Account.Effect.Accounts (Accounts)
import Polysemy.Account.Api.Server.Error (unauthorized, internal)

-- | Basic values for describing the requirements of an endpoint for either "any user" or "admin".
class AuthEndpointParam param where
  authEndpointUser :: param
  authEndpointAdmin :: param

instance AuthEndpointParam [Privilege] where
  authEndpointUser = []
  authEndpointAdmin = [Privilege.Admin]

pattern Active :: AuthedAccount i p -> AuthedAccount i p
pattern Active acc <- acc@(AuthedAccount _ _ _ AccountStatus.Active _)

insufficient ::
  ∀ i p r a .
  Show (AuthedAccount i p) =>
  Members [Log, Stop ServerError] r =>
  AuthedAccount i p ->
  Text ->
  Sem r a
insufficient account extra = do
  Log.error [exon|auth with insufficient privileges: #{show account}#{extra}|]
  stop (unauthorized "insufficient privileges")

checkAccount ::
  ∀ i param p r a .
  Show (AuthedAccount i p) =>
  Members [Authorize i param p, Log, Stop ServerError] r =>
  param ->
  (AuthedAccount i p -> Sem r a) ->
  AuthedAccount i p ->
  Sem r a
checkAccount param f (Active account) =
  authorize param account >>= \case
    Just msg ->
      insufficient account [exon| (#{msg})|]
    Nothing ->
      f account
checkAccount _ _ account =
  insufficient account ""

type AuthEndpoint i param p r =
  (Members [Authorize i param p, Accounts i p !! AccountsError, Log, Stop ServerError] r, AuthEndpointParam param)

-- | Wrap an authenticated handler function with an authorization check.
--
-- Sends the account information and given endpoint param to the 'Authorize' effect if the authentication material is
-- valid.
authorizeEndpoint ::
  ∀ i param p r a .
  Show (AuthedAccount i p) =>
  AuthEndpoint i param p r =>
  param ->
  (AuthedAccount i p -> Sem r a) ->
  AuthResult (AuthedAccount i p) ->
  Sem r a
authorizeEndpoint param f (Authenticated payload@(AuthedAccount _ authId _ _ _)) = do
  account <- Accounts.authed authId !! \case
    Client _ -> insufficient payload " (token expired)"
    Internal _ -> stop (internal "Fatal error")
  checkAccount param f account
authorizeEndpoint _ _ _ =
  stop (unauthorized "no valid auth data")

-- | Require that the authentication material belongs to an active account.
accountOnly ::
  ∀ i param p r a .
  Show (AuthedAccount i p) =>
  AuthEndpoint i param p r =>
  (AuthedAccount i p -> Sem r a) ->
  AuthResult (AuthedAccount i p) ->
  Sem r a
accountOnly =
  authorizeEndpoint authEndpointUser

-- | Require that the authentication material belongs to an active account.
accountOnly_ ::
  ∀ i param p r a .
  Show (AuthedAccount i p) =>
  AuthEndpoint i param p r =>
  Sem r a ->
  AuthResult (AuthedAccount i p) ->
  Sem r a
accountOnly_ f =
  accountOnly (const f)

-- | Require that the authentication material belongs to an active account.
accountOnly1 ::
  ∀ i param p r a b .
  Show (AuthedAccount i p) =>
  AuthEndpoint i param p r =>
  (AuthedAccount i p -> a -> Sem r b) ->
  AuthResult (AuthedAccount i p) ->
  a ->
  Sem r b
accountOnly1 f authResult a =
  accountOnly (\ acc -> f acc a) authResult

-- | Require that the authentication material belongs to an active account.
accountOnly1_ ::
  ∀ i param p r a b .
  Show (AuthedAccount i p) =>
  AuthEndpoint i param p r =>
  (a -> Sem r b) ->
  AuthResult (AuthedAccount i p) ->
  a ->
  Sem r b
accountOnly1_ f authResult a =
  accountOnly_ (f a) authResult

-- | Require that the authentication material belongs to an active account.
accountOnly2 ::
  ∀ i param p r a b c .
  Show (AuthedAccount i p) =>
  AuthEndpoint i param p r =>
  (AuthedAccount i p -> a -> b -> Sem r c) ->
  AuthResult (AuthedAccount i p) ->
  a ->
  b ->
  Sem r c
accountOnly2 f authResult a b =
  accountOnly (\ acc -> f acc a b) authResult

-- | Require that the authentication material belongs to an active account.
accountOnly2_ ::
  ∀ i param p r a b c .
  Show (AuthedAccount i p) =>
  AuthEndpoint i param p r =>
  (a -> b -> Sem r c) ->
  AuthResult (AuthedAccount i p) ->
  a ->
  b ->
  Sem r c
accountOnly2_ f authResult a b =
  accountOnly_ (f a b) authResult

-- | Require that the authentication material belongs to an active admin account.
adminOnly ::
  ∀ i param p r a .
  Show (AuthedAccount i p) =>
  AuthEndpoint i param p r =>
  (AuthedAccount i p -> Sem r a) ->
  AuthResult (AuthedAccount i p) ->
  Sem r a
adminOnly =
  authorizeEndpoint authEndpointAdmin

-- | Require that the authentication material belongs to an active admin account.
adminOnly_ ::
  ∀ i param p r a .
  Show (AuthedAccount i p) =>
  AuthEndpoint i param p r =>
  Sem r a ->
  AuthResult (AuthedAccount i p) ->
  Sem r a
adminOnly_ f =
  adminOnly (const f)

-- | Require that the authentication material belongs to an active admin account.
adminOnly1 ::
  ∀ i param p r a b .
  Show (AuthedAccount i p) =>
  AuthEndpoint i param p r =>
  (AuthedAccount i p -> a -> Sem r b) ->
  AuthResult (AuthedAccount i p) ->
  a ->
  Sem r b
adminOnly1 f authResult a =
  adminOnly (\ acc -> f acc a) authResult

-- | Require that the authentication material belongs to an active admin account.
adminOnly1_ ::
  ∀ i param p r a b .
  Show (AuthedAccount i p) =>
  AuthEndpoint i param p r =>
  (a -> Sem r b) ->
  AuthResult (AuthedAccount i p) ->
  a ->
  Sem r b
adminOnly1_ f authResult a =
  adminOnly_ (f a) authResult

-- | Require that the authentication material belongs to an active admin account.
adminOnly2 ::
  ∀ i param p r a b c .
  Show (AuthedAccount i p) =>
  AuthEndpoint i param p r =>
  (AuthedAccount i p -> a -> b -> Sem r c) ->
  AuthResult (AuthedAccount i p) ->
  a ->
  b ->
  Sem r c
adminOnly2 f authResult a b =
  adminOnly (\ acc -> f acc a b) authResult

-- | Require that the authentication material belongs to an active admin account.
adminOnly2_ ::
  ∀ i param p r a b c .
  Show (AuthedAccount i p) =>
  AuthEndpoint i param p r =>
  (a -> b -> Sem r c) ->
  AuthResult (AuthedAccount i p) ->
  a ->
  b ->
  Sem r c
adminOnly2_ f authResult a b =
  adminOnly_ (f a b) authResult
