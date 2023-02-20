module Polysemy.Account (
  -- * Effects
  Accounts,
  authenticate,
  generateToken,
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
  token,

  -- * Interpreters
  interpretAccounts,
  interpretAccountsState,
  interpretAccountsStateWith,
  interpretAccountsPasswordState,
  interpretAccountsPasswordStateWith,

  -- * Misc combinators
  register,
  login,

  -- * Data types
  Account (..),
  AuthedAccount (..),
  AccountsError (..),
  AccountsClientError (..),
  AccountCredentials (..),
  AccountName (..),
  RawPassword,
  rawPassword,
  AccountStatus (..),
  Privilege (..),
  AccountP,
  AuthedAccountP,
  AuthToken (..),
  AccountIsAdmin (..),
  Port (Port),
) where

import Prelude hiding (all)

import Polysemy.Account.Data.Account (Account (..), AccountP)
import Polysemy.Account.Data.AccountCredentials (AccountCredentials (..))
import Polysemy.Account.Data.AccountName (AccountName (..))
import Polysemy.Account.Data.AccountStatus (AccountStatus (..))
import Polysemy.Account.Data.AccountsError (AccountsClientError (..), AccountsError (..))
import Polysemy.Account.Data.AuthToken (AuthToken (..))
import Polysemy.Account.Data.AuthedAccount (AuthedAccount (..), AuthedAccountP)
import Polysemy.Account.Data.Port (Port (..))
import Polysemy.Account.Data.Privilege (AccountIsAdmin (..), Privilege (..))
import Polysemy.Account.Data.RawPassword (RawPassword (..), rawPassword)
import Polysemy.Account.Effect.Accounts (
  Accounts,
  addPassword,
  all,
  allAuths,
  authenticate,
  byId,
  byName,
  create,
  finalizeCreate,
  generateToken,
  privileges,
  setStatus,
  update,
  updatePrivileges,
  )
import Polysemy.Account.Effect.Password (Password, check, hash, token)
import Polysemy.Account.Interpreter.Accounts (
  interpretAccounts,
  interpretAccountsPasswordState,
  interpretAccountsPasswordStateWith,
  interpretAccountsState,
  interpretAccountsStateWith,
  )
import Polysemy.Account.Login (login)
import Polysemy.Account.Register (register)
