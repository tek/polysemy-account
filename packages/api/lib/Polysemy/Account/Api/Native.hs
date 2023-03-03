-- | Description: Server runners using 'Jwt' for authentication
module Polysemy.Account.Api.Native where

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
import Servant.Auth.Server (CookieSettings, JWTSettings, defaultCookieSettings)

import qualified Polysemy.Account.Api.Effect.Jwt as Jwt
import Polysemy.Account.Api.Effect.Jwt (Jwt)
import Polysemy.Account.Api.NativeContext (ServerReady, runServer)
import Polysemy.Account.Data.Port (Port)

-- | The Servant context for 'Jwt' servers.
type AuthContext =
  [JWTSettings, CookieSettings]

-- | Servant constraint for servers using JWT.
type ServerAuth api =
  HasServer api AuthContext

-- | Run a Servant server with JSON Web Token authentication using settings from 'Jwt'.
--
-- This variant allows supplying additional 'Context's.
runServerJwtWith ::
  ∀ (api :: Type) (context :: [Type]) a e r .
  HasContextEntry (context .++ DefaultErrorFormatters) ErrorFormatters =>
  HasServer api (AuthContext ++ context) =>
  Members [Sync ServerReady, Jwt a !! e, Log, Interrupt, Error Text, Final IO] r =>
  Context context ->
  ServerT api (Sem (Stop ServerError : r)) ->
  Port ->
  Sem r ()
runServerJwtWith ctx srv port = do
  jwtSettings <- Jwt.settings !>> throw "Jwt initialization failed"
  runServer @api srv (jwtSettings :. defaultCookieSettings :. ctx) port

-- | Run a Servant server with JSON Web Token authentication using settings from 'Jwt'.
runServerJwt ::
  ∀ (api :: Type) a e r .
  ServerAuth api =>
  Members [Sync ServerReady, Jwt a !! e, Log, Interrupt, Error Text, Final IO] r =>
  ServerT api (Sem (Stop ServerError : r)) ->
  Port ->
  Sem r ()
runServerJwt =
  runServerJwtWith @api EmptyContext
