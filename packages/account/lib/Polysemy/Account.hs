-- | Description: Account management with Servant and Polysemy
module Polysemy.Account (
  -- * Accounts
  -- ** Effects
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
  authed,
  auths,
  update,
  Polysemy.Account.Effect.Accounts.privileges,
  updatePrivileges,
  all,
  allAuths,

  -- ** Interpreters
  interpretAccounts,
  interpretAccountsState,
  interpretAccountsState',
  interpretAccountByNameState,
  interpretAuthForAccountState,

  -- ** Data types
  Account (..),
  AccountP,
  AuthedAccount (..),
  AuthedAccountP,
  AccountAuth (..),
  AccountsConfig (..),
  AccountsConfigP,
  AccountsError (..),
  AccountsClientError (..),
  AccountCredentials (..),
  AccountName (..),
  AccountStatus (..),
  AccountAuthDescription (..),

  -- ** Privileges
  Privilege (..),
  Privileges (..),
  RequiredPrivileges (..),
  satisfiesPrivilege,
  satisfiesPrivileges,
  unsatisfiedPrivileges,
  DefaultPrivileges (..),

  -- * Passwords
  -- ** Effects
  Password,
  hash,
  check,
  generate,

  -- * Data types
  RawPassword,
  rawPassword,
  GeneratedPassword (..),
  HashedPassword (..),

  -- ** Interpreters
  interpretPassword,
  interpretPasswordId,

  -- * Servant auth
  -- ** Effects
  Authorize (Authorize),
  AuthorizeP,
  authorize,

  AccountByName (AccountByName),
  AuthForAccount (AuthForAccount),

  -- ** API
  Authed,
  AuthedP,
  AccountApi,
  AccountApiP,
  AuthApi,
  AuthApiP,

  -- ** Data types
  AuthToken (..),
  Port (Port),
  AuthQuery,
  AccountQuery,

  -- * Convenience actions
  register,
  registerAdmin,
  registerAs,
  login,
  unlockAccountName,
) where

import Prelude hiding (all)

import Polysemy.Account.Accounts (login, register, registerAdmin, registerAs, unlockAccountName)
import Polysemy.Account.Data.Account (Account (..), AccountP)
import Polysemy.Account.Data.AccountAuth (AccountAuth (..))
import Polysemy.Account.Data.AccountAuthDescription (AccountAuthDescription (..))
import Polysemy.Account.Data.AccountByName (AccountByName (AccountByName))
import Polysemy.Account.Data.AccountCredentials (AccountCredentials (..))
import Polysemy.Account.Data.AccountName (AccountName (..))
import Polysemy.Account.Data.AccountStatus (AccountStatus (..))
import Polysemy.Account.Data.AccountsConfig (AccountsConfig (..), AccountsConfigP)
import Polysemy.Account.Data.AccountsError (AccountsClientError (..), AccountsError (..))
import Polysemy.Account.Data.AuthForAccount (AuthForAccount (AuthForAccount))
import Polysemy.Account.Data.AuthToken (AuthToken (..))
import Polysemy.Account.Data.Authed (Authed, AuthedP)
import Polysemy.Account.Data.AuthedAccount (AuthedAccount (..), AuthedAccountP)
import Polysemy.Account.Data.GeneratedPassword (GeneratedPassword (..))
import Polysemy.Account.Data.HashedPassword (HashedPassword (..))
import Polysemy.Account.Data.Port (Port (..))
import Polysemy.Account.Data.Privilege (
  DefaultPrivileges (..),
  Privilege (..),
  Privileges (..),
  RequiredPrivileges (..),
  satisfiesPrivilege,
  satisfiesPrivileges,
  unsatisfiedPrivileges,
  )
import Polysemy.Account.Data.RawPassword (RawPassword (..), rawPassword)
import Polysemy.Account.Effect.Accounts (
  Accounts,
  AccountsP,
  addPassword,
  all,
  allAuths,
  authed,
  authenticate,
  auths,
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
import Polysemy.Account.Interpreter.AccountByName (AccountQuery, interpretAccountByNameState)
import Polysemy.Account.Interpreter.Accounts (interpretAccounts, interpretAccountsState, interpretAccountsState')
import Polysemy.Account.Interpreter.AuthForAccount (AuthQuery, interpretAuthForAccountState)
import Polysemy.Account.Interpreter.Password (interpretPassword, interpretPasswordId)
import Polysemy.Account.Routes (AccountApi, AccountApiP, AuthApi, AuthApiP)
