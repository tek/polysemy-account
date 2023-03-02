-- | Description: JWT effects
module Polysemy.Account.Api.Effect.Jwt where

import Crypto.JOSE (JWK)
import Servant.Auth.Server (JWTSettings)

import Polysemy.Account.Data.AuthToken (AuthToken)

-- | Effect for generating JSON Web Keys.
data GenJwk :: Effect where
  -- | Generate a JSON Web Key.
  GenJwk :: GenJwk m JWK

makeSem ''GenJwk

-- | Effect for managing JSON Web Token generation.
data Jwt a :: Effect where
  -- | Generate a new JSON Web Key for signing tokens.
  Key :: Jwt a m JWK
  -- | Obtain the settings used to sign and validate tokens.
  Settings :: Jwt a m JWTSettings
  -- | Create a new JSON Web Token.
  MakeToken :: a -> Jwt a m AuthToken

makeSem ''Jwt
