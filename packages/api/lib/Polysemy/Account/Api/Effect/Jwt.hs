module Polysemy.Account.Api.Effect.Jwt where

import Crypto.JOSE (JWK)
import Servant.Auth.Server (JWTSettings)

import Polysemy.Account.Data.AuthToken (AuthToken)

data GenJwk :: Effect where
   GenJwk :: GenJwk m JWK

makeSem ''GenJwk

data Jwt a :: Effect where
  Key :: Jwt a m JWK
  Settings :: Jwt a m JWTSettings
  MakeToken :: a -> Jwt a m AuthToken

makeSem ''Jwt
