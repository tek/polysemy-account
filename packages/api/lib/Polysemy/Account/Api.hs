-- | Description: Account management with Servant and Polysemy
module Polysemy.Account.Api (
  -- * Effects
  Jwt,
  key,
  settings,
  makeToken,

  GenJwk,
  genJwk,

  -- * Interpreters
  interpretJwt,
  interpretJwtDb,
  interpretJwtPersistent,
  interpretJwtState,

  interpretGenJwk,

  interpretAuthorizeWith,
  interpretAuthorizeP,

  interpretAccountsDb,
  interpretAccountStore,
  interpretAccountAuthStore,
  interpretAccountTable,
  interpretAccountAuthTable,

  -- * Servant
  accountServer,
  authServer,
  runServerJwt,
  runServerJwtWith,
  runServer,
  runServerSem,
  ServerReady (ServerReady),
  authorizeEndpoint,
  AuthEndpointParam (..),
  accountOnly,
  accountOnly_,
  accountOnly1,
  accountOnly1_,
  accountOnly2,
  accountOnly2_,
  adminOnly,
  adminOnly_,
  adminOnly1,
  adminOnly1_,
  adminOnly2,
  adminOnly2_,
) where

import Polysemy.Account.Api.Effect.Jwt (GenJwk, Jwt, genJwk, key, makeToken, settings)
import Polysemy.Account.Api.Interpreter.Accounts (
  interpretAccountAuthStore,
  interpretAccountAuthTable,
  interpretAccountStore,
  interpretAccountTable,
  interpretAccountsDb,
  )
import Polysemy.Account.Api.Interpreter.Authorize (interpretAuthorizeP, interpretAuthorizeWith)
import Polysemy.Account.Api.Interpreter.Jwt (
  interpretGenJwk,
  interpretJwt,
  interpretJwtDb,
  interpretJwtPersistent,
  interpretJwtState,
  )
import Polysemy.Account.Api.Native (runServerJwt, runServerJwtWith)
import Polysemy.Account.Api.NativeContext (ServerReady (ServerReady), runServer, runServerSem)
import Polysemy.Account.Api.Server.Account (accountServer)
import Polysemy.Account.Api.Server.Auth (authServer)
import Polysemy.Account.Api.Server.AuthEndpoint (
  AuthEndpointParam (..),
  accountOnly,
  accountOnly1,
  accountOnly1_,
  accountOnly2,
  accountOnly2_,
  accountOnly_,
  adminOnly,
  adminOnly1,
  adminOnly1_,
  adminOnly2,
  adminOnly2_,
  adminOnly_,
  authorizeEndpoint,
  )
