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
  interpretAccountsDb,
  interpretAccountsPasswordDb,

  -- * Servant
  Authed,
  AccountApi,
  AuthApi,
  accountServer,
  authServer,
  AuthedP,
  AccountApiP,
  AuthApiP,
  runServerJwt,
  runServerJwtWith,
  runServerSem,
  ServerReady (ServerReady),
  authorize,
  ensureUser,
  ensureUser_,
  ensureAdmin,
  ensureAdmin_,
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
  pattern Active,
  pattern Admin,
) where

import Polysemy.Account.Api.Data.Authed (Authed, AuthedP)
import Polysemy.Account.Api.Effect.Jwt (GenJwk, Jwt, genJwk, key, makeToken, settings)
import Polysemy.Account.Api.Interpreter.Accounts (interpretAccountsDb, interpretAccountsPasswordDb)
import Polysemy.Account.Api.Interpreter.Jwt (
  interpretGenJwk,
  interpretJwt,
  interpretJwtDb,
  interpretJwtPersistent,
  interpretJwtState,
  )
import Polysemy.Account.Api.Native (runServerJwt, runServerJwtWith)
import Polysemy.Account.Api.NativeContext (ServerReady (ServerReady), runServerSem)
import Polysemy.Account.Api.Routes (AccountApi, AccountApiP, AuthApi, AuthApiP)
import Polysemy.Account.Api.Server.Account (accountServer)
import Polysemy.Account.Api.Server.Auth (authServer)
import Polysemy.Account.Api.Server.AuthEndpoint (
  pattern Active,
  pattern Admin,
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
  authorize,
  ensureAdmin,
  ensureAdmin_,
  ensureUser,
  ensureUser_,
  )
