-- | Description: Account status data type
module Polysemy.Account.Data.AccountStatus where

-- | Basic account status.
data AccountStatus =
  -- | The account was added to storage, but not processed fully.
  Creating
  |
  -- | The account was fully created, but not approved by an admin.
  Pending
  |
  -- | The account is fully operational.
  Active
  |
  -- | An admin has disabled the account.
  Locked
  deriving stock (Eq, Show, Generic)

json ''AccountStatus
