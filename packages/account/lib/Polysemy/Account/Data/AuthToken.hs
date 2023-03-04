-- | Description: Auth token data type
module Polysemy.Account.Data.AuthToken where

import Servant.Auth.JWT (FromJWT, ToJWT)

-- | An auth token, used by the JWT tools in @polysemy-account-api@.
newtype AuthToken =
  AuthToken { unAuthToken :: Text }
  deriving stock (Eq, Show)

json ''AuthToken

instance FromJWT AuthToken where
instance ToJWT AuthToken where
