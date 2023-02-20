module Polysemy.Account.Api.Test.Effect.TestClient where

import Network.Wai.Test (SResponse)

import Polysemy.Account.Api.Test.Data.Request (Headers, Method)

data TestClient :: Effect where
  Request :: Method -> Text -> Headers -> LByteString -> TestClient m SResponse
  MakeAdmin :: TestClient m (Text, (ByteString, ByteString))
  MakeUser :: TestClient m (Text, (ByteString, ByteString))

makeSem ''TestClient
