{-# options_ghc -Wno-orphans #-}

module Polysemy.Account.Api.Test.AuthTest where

import qualified Data.Text as Text
import Exon (exon)
import Network.HTTP.Types (Status (Status))
import Network.Wai.Test (SResponse (SResponse))
import Polysemy.Test (UnitTest, evalMaybe, (===))
import Servant (BasicAuthData, Get, JSON, (:<|>) ((:<|>)), (:>))
import Servant.Auth.Server (
  Auth,
  AuthResult (Authenticated),
  BasicAuth,
  BasicAuthCfg,
  Cookie,
  FromBasicAuthData (..),
  FromJWT,
  JWT,
  ToJWT,
  )
import "servant-server" Servant.Server (Context (EmptyContext, (:.)), ServerError, ServerT, err401)

import Polysemy.Account.Api.Test.Data.Request (Method (Get))
import qualified Polysemy.Account.Api.Test.Effect.TestClient as TestClient
import Polysemy.Account.Data.AuthToken (AuthToken (AuthToken))
import Polysemy.Account.Api.Test.Request (runApiTestCtx)

instance FromJWT AuthToken where
instance ToJWT AuthToken where

type instance BasicAuthCfg =
  BasicAuthData -> IO (AuthResult AuthToken)

instance FromBasicAuthData AuthToken where
  fromBasicAuthData authData check =
    check authData

type TestApi =
  (
    Auth [JWT, Cookie, BasicAuth] AuthToken :> "first" :> Get '[JSON] ()
    :<|>
    Auth '[JWT] AuthToken :> "second" :> Get '[JSON] Int
  )

serverFirst ::
  AuthResult AuthToken ->
  Sem r ()
serverFirst _ =
  unit

serverSecond ::
  Member (Stop ServerError) r =>
  AuthResult AuthToken ->
  Sem r Int
serverSecond (Authenticated _) =
  pure 6
serverSecond _ =
  stop err401

server ::
  Member (Stop ServerError) r =>
  ServerT TestApi (Sem r)
server =
  serverFirst :<|> serverSecond

auth ::
  BasicAuthData ->
  IO (AuthResult AuthToken)
auth _ = do
  pure (Authenticated (AuthToken "foo"))

authHeader :: (ByteString, ByteString)
authHeader =
  ("authorization", "Basic QWxhZGRpbjpPcGVuU2VzYW1l")

test_authApi :: UnitTest
test_authApi =
  runApiTestCtx @TestApi (auth :. EmptyContext) [] [] server do
    SResponse (Status _ _) headers _ <- TestClient.request Get "first" [authHeader] ""
    cookie <- evalMaybe (firstJust isJwtHeader headers)
    SResponse (Status statusOk _) _ body <- TestClient.request Get "second" [jwtHeader cookie] ""
    statusOk === 200
    body === "6"
  where
    isJwtHeader =
      Text.stripPrefix "JWT-Cookie=" . Text.takeWhile (';' /=) . decodeUtf8 . snd
    jwtHeader cookie =
      ("authorization", [exon|Bearer #{encodeUtf8 cookie}|])
