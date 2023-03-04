-- | Description: Misc combinators
module Polysemy.Account.Accounts where

import Sqel (Uid (Uid))

import Polysemy.Account.Data.Account (Account (Account))
import Polysemy.Account.Data.AccountAuth (AccountAuth (AccountAuth))
import Polysemy.Account.Data.AccountCredentials (AccountCredentials (AccountCredentials))
import Polysemy.Account.Data.AccountName (AccountName)
import Polysemy.Account.Data.AccountStatus (AccountStatus (Active))
import Polysemy.Account.Data.AccountsError (AccountsError)
import Polysemy.Account.Data.AuthedAccount (AuthedAccount (AuthedAccount))
import Polysemy.Account.Data.Privilege (DefaultPrivileges (defaultAdminPrivileges))
import qualified Polysemy.Account.Effect.Accounts as Accounts
import Polysemy.Account.Effect.Accounts (Accounts)

-- | Convenience function for unlocking the account matching the given name.
unlockAccountName ::
  Members [Accounts i p, Stop AccountsError] r =>
  AccountName ->
  Sem r ()
unlockAccountName name = do
  Uid i _ <- Accounts.byName name
  Accounts.setStatus i Active

-- | Authenticate the given credentials against the storage backend and return the matched account's information.
login ::
  Member (Accounts i p) r =>
  AccountCredentials ->
  Sem r (AuthedAccount i p)
login (AccountCredentials username password) = do
  Uid authId (AccountAuth accountId _ _ _) <- Accounts.authenticate username password
  Uid _ (Account name status privs) <- Accounts.byId accountId
  pure (AuthedAccount accountId authId name status privs)

-- | Register an account with the given privileges and credentials.
--
-- Create the account in the storage backend, hash the password and store it, then mark the account as created.
registerAs' ::
  Member (Accounts i p) r =>
  Maybe p ->
  AccountCredentials ->
  Sem r (AuthedAccount i p)
registerAs' privs (AccountCredentials username password) = do
  Uid accountId _ <- Accounts.create username privs
  Uid authId (AccountAuth _ _ _ _) <- Accounts.addPassword accountId password Nothing
  Uid _ (Account name status privs') <- Accounts.finalizeCreate accountId
  pure (AuthedAccount accountId authId name status privs')

-- | Register an account with the given privileges and credentials.
--
-- Create the account in the storage backend, hash the password and store it, then mark the account as created.
registerAs ::
  Member (Accounts i p) r =>
  p ->
  AccountCredentials ->
  Sem r (AuthedAccount i p)
registerAs privs =
  registerAs' (Just privs)

-- | Register an account with the given credentials using the default privileges.
--
-- Create the account in the storage backend, hash the password and store it, then mark the account as created.
register ::
  Member (Accounts i p) r =>
  AccountCredentials ->
  Sem r (AuthedAccount i p)
register =
  registerAs' Nothing

-- | Register an admin account with the given credentials using the default privileges.
--
-- Create the account in the storage backend, hash the password and store it, then mark the account as created.
registerAdmin ::
  DefaultPrivileges p =>
  Member (Accounts i p) r =>
  AccountCredentials ->
  Sem r (AuthedAccount i p)
registerAdmin =
  registerAs' (Just defaultAdminPrivileges)
