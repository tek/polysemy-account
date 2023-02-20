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

type AuthContext =
  [JWTSettings, CookieSettings]

type ServerAuth api =
  HasServer api AuthContext

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
  runServer @api srv (context jwtSettings) port
  where
    context jwtSettings = jwtSettings :. defaultCookieSettings :. ctx

runServerJwt ::
  ∀ (api :: Type) a e r .
  ServerAuth api =>
  Members [Sync ServerReady, Jwt a !! e, Log, Interrupt, Error Text, Final IO] r =>
  ServerT api (Sem (Stop ServerError : r)) ->
  Port ->
  Sem r ()
runServerJwt =
  runServerJwtWith @api EmptyContext
