-- | Description: Accounts effect
module Polysemy.Account.Effect.Accounts where

import Chronos (Datetime)
import Prelude hiding (all)
import Sqel (Uid)

import Polysemy.Account.Data.Account (Account)
import Polysemy.Account.Data.AccountAuth (AccountAuth)
import Polysemy.Account.Data.AccountName (AccountName)
import Polysemy.Account.Data.AccountStatus (AccountStatus)
import Polysemy.Account.Data.AuthedAccount (AuthedAccount)
import Polysemy.Account.Data.GeneratedPassword (GeneratedPassword)
import qualified Polysemy.Account.Data.Privilege as Privilege
import Polysemy.Account.Data.RawPassword (RawPassword)

-- | This effect provides common operations for account and password management.
--
-- The first parameter is the ID type for both accounts and authentication data, which might be 'Data.UUID.UUID' or
-- @Int@.
--
-- The second parameter encodes an accounts basic privileges, mainly used for API authorization.
data Accounts i p :: Effect where
  -- | Check credentials against the storage backend.
  Authenticate :: AccountName -> RawPassword -> Accounts i p m (Uid i (AccountAuth i))
  -- | Generate a fresh password.
  GeneratePassword :: i -> Maybe Datetime -> Accounts i p m GeneratedPassword
  -- | Add an account to the storage backend, without authentication.
  Create :: AccountName -> Maybe p -> Accounts i p m (Uid i (Account p))
  -- | Mark an account as fully created.
  FinalizeCreate :: i -> Accounts i p m (Uid i (Account p))
  -- | Associate an account with a new password, with optional expiry time.
  AddPassword :: i -> RawPassword -> Maybe Datetime -> Accounts i p m (Uid i (AccountAuth i))
  -- | Update the status of an account.
  SetStatus :: i -> AccountStatus -> Accounts i p m ()
  -- | Look up an account by its ID.
  ById :: i -> Accounts i p m (Uid i (Account p))
  -- | Look up an account by its name.
  ByName :: AccountName -> Accounts i p m (Uid i (Account p))
  -- | Look up an account and return its auth info.
  Authed :: i -> Accounts i p m (AuthedAccount i p)
  -- | Look up auths for an account
  Auths :: i -> Accounts i p m [Uid i (AccountAuth i)]
  -- | Overwrite an existing account.
  Update :: Uid i (Account p) -> Accounts i p m ()
  -- | Look up an account's privileges.
  Privileges :: i -> Accounts i p m p
  -- | Update an account's privileges.
  UpdatePrivileges :: i -> (p -> p) -> Accounts i p m ()
  -- | Fetch all accounts.
  All :: Accounts i p m [Uid i (Account p)]
  -- | Fetch all auth records.
  AllAuths :: Accounts i p m [Uid i (AccountAuth i)]

makeSem ''Accounts

-- | Convenience alias for 'Accounts' using the default 'Privilege.Privileges' type.
type AccountsP i = Accounts i Privilege.Privileges
