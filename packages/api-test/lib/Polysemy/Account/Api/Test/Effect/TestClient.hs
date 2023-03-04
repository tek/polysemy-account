-- | Description: Test effect for Servant APIs with auth.
module Polysemy.Account.Api.Test.Effect.TestClient where

import Network.HTTP.Types (ResponseHeaders)

import Polysemy.Account.Api.Test.Data.Request (Header, Headers, Method)
import Polysemy.Account.Data.AccountCredentials (AccountCredentials)
import Polysemy.Account.Data.AuthedAccount (AuthedAccount)
import Polysemy.Account.Data.Privilege (Privileges)

-- | A simple test response.
data Response =
  Response {
    status :: Int,
    headers :: ResponseHeaders,
    body :: LByteString
  }
  deriving stock (Eq, Show, Generic)

-- | This effect provides convenience actions for testing a Servant API with auth without running a server,
-- using @wai-test@.
data TestClient i p :: Effect where

  -- | Make a request against the API.
  RawRequest :: Method -> Text -> Headers -> LByteString -> TestClient i p m Response

  -- | Create a JWT token header for an account.
  -- The account has to be registered, using combinators like 'Polysemy.Account.register'.
  MakeToken :: AuthedAccount i p -> TestClient i p m (Text, Header)

  -- | Create a user with default privileges and return an auth header for it.
  MakeUser :: AccountCredentials -> TestClient i p m (Text, Header)

makeSem ''TestClient

-- | Convenience alias for using the default privilege type with 'TestClient'.
type TestClientP i = TestClient i Privileges
