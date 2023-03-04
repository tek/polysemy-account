-- | Description: Data types for the test client effect.
module Polysemy.Account.Api.Test.Data.Request where

import qualified Data.Text as Text

-- | Basic HTTP methods.
data Method =
  Get
  |
  Post
  |
  Put
  |
  Delete
  deriving stock (Show, Eq)

-- | Convenience alias.
type Header =
  (ByteString, ByteString)

-- | Convenience alias.
type Headers =
  [Header]

-- | Request type used by the test client effect.
data TestRequest =
  TestRequest {
    method :: Method,
    path :: Text,
    headers :: Headers,
    body :: LByteString
  }
  deriving stock (Eq, Show)

-- | Transform a 'Method' for use with @wai@.
methodUpper :: Method -> ByteString
methodUpper =
  encodeUtf8 . Text.toUpper . show
