{-# language NoFieldSelectors #-}

-- | Description: Config data type for the effect 'Polysemy.Account.Accounts'.
module Polysemy.Account.Data.AccountsConfig where

import Polysemy.Account.Data.Privilege (DefaultPrivileges (defaultAdminPrivileges, defaultPrivileges), Privileges)

-- | The configuration for the interpreter for 'Polysemy.Account.Accounts'.
--
-- The defaults, when using 'Privileges', are:
--
-- - Length 20
-- - Don't activate accounts right away
-- - 'Polysemy.Account.Web' privileges
data AccountsConfig p =
  AccountsConfig {
    -- | Length of generated passwords.
    passwordLength :: Word,
    -- | Whether new accounts should immediately be marked as active rather than pending, allowing login.
    initActive :: Bool,
    -- | The privileges assigned to a new account.
    defaultPrivileges :: p,
    -- | The privileges assigned to a new admin account (only for tests).
    defaultAdminPrivileges :: p
  }
  deriving stock (Eq, Show, Generic)

json ''AccountsConfig

-- | Convenience alias for using the default privilege type with 'AccountsConfig'.
type AccountsConfigP = AccountsConfig Privileges

instance {-# overlappable #-} (
    DefaultPrivileges p
  ) => Default (AccountsConfig p) where
  def = AccountsConfig {
    passwordLength = 20,
    initActive = False,
    defaultPrivileges = defaultPrivileges,
    defaultAdminPrivileges = defaultAdminPrivileges
  }
