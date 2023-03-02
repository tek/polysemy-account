-- | Description: Generated password data type
module Polysemy.Account.Data.GeneratedPassword where

-- | A password that was generated, intended to be shown to the user, and therefore permitted to be 'show'n, as opposed
-- to 'Polysemy.Account.RawPassword'.
newtype GeneratedPassword =
  GeneratedPassword { unGeneratedPassword :: Text }
  deriving stock (Eq, Show, Generic)
  deriving newtype (IsString, Ord)

json ''GeneratedPassword
