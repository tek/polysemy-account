-- | Description: Account auth description data type
module Polysemy.Account.Data.AccountAuthDescription where

-- | A freeform text used to describe the nature of an 'Polysemy.Account.AccountAuth', like whether it's a generated or
-- user-supplied password.
newtype AccountAuthDescription =
  AccountAuthDescription { unAccountAuthDescription :: Text }
  deriving stock (Eq, Show, Generic)
  deriving newtype (IsString, Ord)

json ''AccountAuthDescription
