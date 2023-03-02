-- | Description: Effect for API authorization
module Polysemy.Account.Api.Effect.Authorize where

import Polysemy.Account.Data.AuthedAccount (AuthedAccount)

-- | This effect is used by the combinators in "Polysemy.Account.Api.Server.AuthEndpoint" to decide whether an account
-- is authorized to access an endpoint.
--
-- The type parameters signify:
--
-- [@i@]: The storage ID type.
-- [@param@]: Identifies the authorization requirements of the endpoint.
-- [@priv@]: The privilege type stored in the database.
data Authorize i param priv :: Effect where
  -- | Decide whether the given account is authorized to use the endpoint characterized by the param.
  -- Return 'Just' an error message if access is denied.
  Authorize :: param -> AuthedAccount i priv -> Authorize i param priv m (Maybe Text)

makeSem ''Authorize
