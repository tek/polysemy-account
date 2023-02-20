module Polysemy.Account.Api.Test.Data.Request where

import qualified Data.Text as Text

data Method =
  Get
  |
  Post
  |
  Put
  |
  Delete
  deriving stock (Show, Eq)

type Headers =
  [(ByteString, ByteString)]

data TestRequest =
  TestRequest {
    method :: Method,
    path :: Text,
    headers :: Headers,
    body :: LByteString
  }
  deriving stock (Eq, Show)

methodUpper :: Method -> ByteString
methodUpper =
  encodeUtf8 . Text.toUpper . show
