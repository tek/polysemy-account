-- | Description: Authed account data type
module Polysemy.Account.Data.AuthedAccount where

import Polysemy.Account.Data.AccountName (AccountName)
import Polysemy.Account.Data.AccountStatus (AccountStatus)
import Polysemy.Account.Data.Privilege (Privilege)

-- | An account an the ID of the password used to authenticate it.
data AuthedAccount i p =
  AuthedAccount {
    id :: i,
    authId :: i,
    name :: AccountName,
    status :: AccountStatus,
    privileges :: p
  }
  deriving stock (Eq, Show, Generic)

json ''AuthedAccount

-- | Convenience alias for using the default privilege type with 'AuthedAccount'.
type AuthedAccountP i = AuthedAccount i [Privilege]
