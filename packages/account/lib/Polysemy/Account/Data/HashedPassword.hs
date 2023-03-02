-- | Description: Hasned password data type
module Polysemy.Account.Data.HashedPassword where

-- | An internally used type containing a hash produced by "Data.Password".
newtype HashedPassword =
  HashedPassword { unAccountPassword :: Text }
  deriving stock (Eq, Show, Generic)
  deriving newtype (IsString, Ord)

json ''HashedPassword
