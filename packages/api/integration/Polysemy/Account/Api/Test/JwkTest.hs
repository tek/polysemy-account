module Polysemy.Account.Api.Test.JwkTest where

import qualified Data.Text as Text
import Polysemy.Db (DbError)
import Polysemy.Hasql (Database)
import Polysemy.Hasql.Data.MigrateSem (unMigrateSem)
import Polysemy.Hasql.Test.Run (integrationTest)
import Polysemy.Test (UnitTest, assert, assertEq)
import Servant.Auth.JWT (ToJWT)
import Servant.Auth.Server (FromJWT)
import Sqel.Migration.Metadata (tableColumns, unDbCols)

import qualified Polysemy.Account.Api.Effect.Jwt as Jwt
import Polysemy.Account.Api.Interpreter.Jwt (interpretJwtDb)
import Polysemy.Account.Data.AuthToken (AuthToken (AuthToken))

newtype Tok =
  Tok { unTok :: Text }
  deriving stock (Eq, Show, Generic)
  deriving newtype (IsString, Ord)

json ''Tok

instance FromJWT Tok where
instance ToJWT Tok where

test_jwk :: UnitTest
test_jwk = do
  integrationTest "polysemy_account" $
    interpretJwtDb @Tok do
      assert . (> 20) . Text.length . coerce =<< restop @DbError (Jwt.makeToken (Tok "tok"))
      assertEq 1 . length . (.unDbCols) =<< subsume (restop @DbError @Database (unMigrateSem (tableColumns "jwk")))
