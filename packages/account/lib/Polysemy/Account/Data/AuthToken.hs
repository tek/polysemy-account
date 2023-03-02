-- | Description: Auth token data type
module Polysemy.Account.Data.AuthToken where

-- | An auth token, used by the JWT tools in @polysemy-account-api@.
newtype AuthToken =
  AuthToken { unAuthToken :: Text }
  deriving stock (Eq, Show)

json ''AuthToken
