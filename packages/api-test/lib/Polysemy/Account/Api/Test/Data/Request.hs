-- | Description: Data types for the test client effect.
module Polysemy.Account.Api.Test.Data.Request where

import Data.Text (toUpper)

-- | Basic HTTP methods.
data Method =
  Get
  |
  Post
  |
  Put
  |
  Delete
  |
  Method Text
  deriving stock (Show, Eq)

-- | Convenience alias.
type Header =
  (ByteString, ByteString)

-- | Convenience alias.
type Headers =
  [Header]

-- | Transform a 'Method' for use with @wai@.
methodUpper :: Method -> ByteString
methodUpper =
  encodeUtf8 . \case
    Get -> "GET"
    Post -> "POST"
    Put -> "PUT"
    Delete -> "DELETE"
    Method s -> toUpper s
