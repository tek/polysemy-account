module Polysemy.Account.Api.Server.Auth where

import Servant (ServerError, ServerT, err401, (:<|>) ((:<|>)))
import Servant.Auth.Server (AuthResult)
import qualified Servant.Auth.Server as AuthResult (AuthResult (..))
import Sqel (Uid (Uid))

import qualified Polysemy.Account.Api.Effect.Jwt as Jwt
import Polysemy.Account.Api.Effect.Jwt (Jwt)
import Polysemy.Account.Api.Error (serverError)
import Polysemy.Account.Api.Routes (AuthApi)
import Polysemy.Account.Api.Server.Error (accountsError, jwtError)
import Polysemy.Account.Data.Account (Account (Account))
import Polysemy.Account.Data.AccountCredentials (AccountCredentials)
import Polysemy.Account.Data.AccountsError (AccountsError)
import Polysemy.Account.Data.AuthToken (AuthToken)
import Polysemy.Account.Data.AuthedAccount (AuthedAccount (AuthedAccount))
import qualified Polysemy.Account.Effect.Accounts as Accounts
import Polysemy.Account.Effect.Accounts (Accounts)
import qualified Polysemy.Account.Login as Login
import qualified Polysemy.Account.Register as Register

type AuthServer i p r =
  ServerT (AuthApi i p) (Sem r)

authAccount ::
  Members [Accounts i p !! AccountsError, Log, Stop ServerError] r =>
  AuthResult (AuthedAccount i p) ->
  Sem r (AuthedAccount i p)
authAccount (AuthResult.Authenticated (AuthedAccount accountId authId _ _ _)) = do
  Uid dbId (Account name roles accountStatus) <- Accounts.byId accountId !! accountsError
  pure (AuthedAccount dbId authId name roles accountStatus)
authAccount _ =
  stop (serverError err401 "Invalid credentials")

login ::
  ∀ e i p r .
  Show e =>
  Members [Jwt (AuthedAccount i p) !! e, Accounts i p !! AccountsError, Log, Stop ServerError] r =>
  AccountCredentials ->
  Sem r AuthToken
login cred = do
  account <- Login.login cred !! accountsError
  Jwt.makeToken account !! jwtError

register ::
  Show e =>
  Members [Jwt (AuthedAccount i p) !! e, Accounts i p !! AccountsError, Log, Stop ServerError] r =>
  AccountCredentials ->
  Sem r AuthToken
register cred = do
  account <- Register.register cred !! accountsError
  Jwt.makeToken account !! jwtError

authServer ::
  Show e =>
  Members [Jwt (AuthedAccount i p) !! e, Accounts i p !! AccountsError, Log, Stop ServerError] r =>
  AuthServer i p r
authServer =
  authAccount
  :<|>
  login
  :<|>
  register
