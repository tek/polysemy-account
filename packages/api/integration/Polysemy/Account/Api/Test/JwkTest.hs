module Polysemy.Account.Api.Test.JwkTest where

import qualified Data.Text as Text
import Polysemy.Db.Data.DbError (DbError)
import Polysemy.Hasql (Database)
import Polysemy.Hasql.Interpreter.DbTable (tableColumns)
import Polysemy.Hasql.Test.Run (integrationTest)
import Polysemy.Test (UnitTest, assert, assertEq)
import Servant.Auth.JWT (ToJWT)
import Servant.Auth.Server (FromJWT)

import qualified Polysemy.Account.Api.Effect.Jwt as Jwt
import Polysemy.Account.Data.AuthToken (AuthToken (AuthToken))
import Polysemy.Account.Api.Interpreter.Jwt (interpretJwtDb)

newtype Tok =
  Tok { unTok :: Text }
  deriving stock (Eq, Show, Generic)
  deriving newtype (IsString, Ord)

json ''Tok

instance FromJWT Tok where
instance ToJWT Tok where

test_jwk :: UnitTest
test_jwk =
  integrationTest "polysemy_account_db" "polysemy-account" $
  interpretJwtDb @Tok do
    assert . (> 20) . Text.length . coerce =<< restop @DbError (Jwt.makeToken (Tok "tok"))
    assertEq 1 . length =<< restop @DbError @Database (tableColumns "jwk")
