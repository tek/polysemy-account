-- | Description: Account management with Servant and Polysemy
module Polysemy.Account (
  -- * Effects
  Accounts,
  AccountsP,
  authenticate,
  generatePassword,
  create,
  finalizeCreate,
  addPassword,
  setStatus,
  byId,
  byName,
  update,
  Polysemy.Account.Effect.Accounts.privileges,
  updatePrivileges,
  all,
  allAuths,

  Password,
  hash,
  check,
  generate,

  Authorize (Authorize),
  AuthorizeP,
  authorize,

  -- * Interpreters
  interpretAccounts,
  interpretAccountsState,

  interpretPassword,
  interpretPasswordId,

  -- * Servant
  Authed,
  AuthedP,
  AccountApi,
  AccountApiP,
  AuthApi,
  AuthApiP,

  -- * Misc combinators
  register,
  login,
  unlockAccountName,

  -- * Data types
  Account (..),
  AuthedAccount (..),
  AccountAuth (..),
  AccountsConfig (..),
  AccountsConfigP,
  AccountsError (..),
  AccountsClientError (..),
  AccountCredentials (..),
  AccountName (..),
  AccountAuthDescription (..),
  RawPassword,
  rawPassword,
  GeneratedPassword (..),
  HashedPassword (..),
  AccountStatus (..),
  Privilege (..),
  AccountP,
  AuthedAccountP,
  AuthToken (..),
  Port (Port),
  AuthQuery,
  AccountQuery,
) where

import Prelude hiding (all)

import Polysemy.Account.Accounts (login, register, unlockAccountName)
import Polysemy.Account.Data.Account (Account (..), AccountP)
import Polysemy.Account.Data.AccountAuth (AccountAuth (..))
import Polysemy.Account.Data.AccountAuthDescription (AccountAuthDescription (..))
import Polysemy.Account.Data.AccountCredentials (AccountCredentials (..))
import Polysemy.Account.Data.AccountName (AccountName (..))
import Polysemy.Account.Data.AccountStatus (AccountStatus (..))
import Polysemy.Account.Data.AccountsConfig (AccountsConfig (..), AccountsConfigP)
import Polysemy.Account.Data.AccountsError (AccountsClientError (..), AccountsError (..))
import Polysemy.Account.Data.AuthToken (AuthToken (..))
import Polysemy.Account.Data.Authed (Authed, AuthedP)
import Polysemy.Account.Data.AuthedAccount (AuthedAccount (..), AuthedAccountP)
import Polysemy.Account.Data.GeneratedPassword (GeneratedPassword (..))
import Polysemy.Account.Data.HashedPassword (HashedPassword (..))
import Polysemy.Account.Data.Port (Port (..))
import Polysemy.Account.Data.Privilege (Privilege (..))
import Polysemy.Account.Data.RawPassword (RawPassword (..), rawPassword)
import Polysemy.Account.Effect.Accounts (
  Accounts,
  AccountsP,
  addPassword,
  all,
  allAuths,
  authenticate,
  byId,
  byName,
  create,
  finalizeCreate,
  generatePassword,
  privileges,
  setStatus,
  update,
  updatePrivileges,
  )
import Polysemy.Account.Effect.Authorize (Authorize (Authorize), AuthorizeP, authorize)
import Polysemy.Account.Effect.Password (Password, check, generate, hash)
import Polysemy.Account.Interpreter.AccountByName (AccountQuery)
import Polysemy.Account.Interpreter.Accounts (interpretAccounts, interpretAccountsState)
import Polysemy.Account.Interpreter.AuthForAccount (AuthQuery)
import Polysemy.Account.Interpreter.Password (interpretPassword, interpretPasswordId)
import Polysemy.Account.Routes (AccountApi, AccountApiP, AuthApi, AuthApiP)
