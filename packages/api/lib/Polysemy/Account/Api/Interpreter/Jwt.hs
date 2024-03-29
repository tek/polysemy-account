{-# options_haddock prune #-}
{-# options_ghc -fno-warn-orphans #-}

-- | Interpreters for 'Jwt'
module Polysemy.Account.Api.Interpreter.Jwt where

import Conc (interpretAtomic)
import qualified Crypto.JOSE as JOSE
import Crypto.JOSE (JWK, KeyMaterialGenParam (OKPGenParam), OKPCrv (Ed25519), genJWK)
import Polysemy.Db (DbError, InitDbError)
import Polysemy.Hasql (Database, interpretAtomicStateDb, interpretTable)
import Servant.Auth.JWT (ToJWT)
import Servant.Auth.Server (FromJWT, JWTSettings, defaultJWTSettings, makeJWT)
import Sqel (Json, Name, Sqel, Table, sqel)

import Polysemy.Account.Api.Effect.Jwt (GenJwk (GenJwk), Jwt (..), genJwk)
import Polysemy.Account.Data.AuthToken (AuthToken (AuthToken))
import Polysemy.Account.Data.AuthedAccount (AuthedAccount)

instance (FromJSON i, FromJSON p) => FromJWT (AuthedAccount i p)
instance (ToJSON i, ToJSON p) => ToJWT (AuthedAccount i p)

generateKey ::
  Member (Embed IO) r =>
  Sem r JWK
generateKey =
  embed (genJWK (OKPGenParam Ed25519))

generateAndStoreKey ::
  Members [AtomicState (Maybe JWK), Embed IO] r =>
  Sem r JWK
generateAndStoreKey = do
  k <- embed (genJWK (OKPGenParam Ed25519))
  k <$ atomicPut (Just k)

-- | Interpret 'GenJwk' using 'Ed25519'.
interpretGenJwk ::
  Member (Embed IO) r =>
  InterpreterFor GenJwk r
interpretGenJwk =
  interpret \ GenJwk -> generateKey

key ::
  Members [AtomicState (Maybe JWK), Embed IO] r =>
  Sem r JWK
key =
  maybe generateAndStoreKey pure =<< atomicGet

settings ::
  Members [AtomicState (Maybe JWK), Embed IO] r =>
  Sem r JWTSettings
settings =
  defaultJWTSettings <$> key

authToken ::
  Member (Error Text) r =>
  Either JOSE.Error LByteString ->
  Sem r AuthToken
authToken = \case
  Right bytes ->
    pure (AuthToken (decodeUtf8 bytes))
  Left err ->
    throw (show err)

-- | Interpret 'Jwt' by storing the key in 'AtomicState', generating it on the fly if absent.
--
-- Generates 'Ed25519' keys.
--
-- Errors originating from the token generator are critical.
interpretJwtState ::
  Members [GenJwk, AtomicState (Maybe JWK), Error Text, Embed IO] r =>
  ToJWT a =>
  InterpreterFor (Jwt a) r
interpretJwtState =
  interpret \case
    Key ->
      genJwk
    Settings ->
      settings
    MakeToken a -> do
      sett <- settings
      authToken =<< embed (makeJWT a sett Nothing)

-- | Interpret 'Jwt' by storing the key in 'AtomicState' in memory.
interpretJwt ::
  ∀ a r .
  Members [Error Text, Embed IO] r =>
  ToJWT a =>
  InterpreterFor (Jwt a) r
interpretJwt =
  interpretAtomic Nothing .
  interpretGenJwk .
  interpretJwtState .
  raiseUnder2

settingsPersistent ::
  Member (AtomicState JWK) r =>
  Sem r JWTSettings
settingsPersistent =
  defaultJWTSettings <$> atomicGet

-- | Interpret 'Jwt' by storing the key in 'AtomicState', requiring the key to be present from the start.
-- This is intended to be used with a database backing the 'AtomicState', the key being generated when starting the app.
--
-- Generates 'Ed25519' keys.
--
-- Errors originating from the token generator are critical.
interpretJwtPersistent ::
  ∀ a e r .
  Members [AtomicState JWK !! e, Error Text, Embed IO] r =>
  ToJWT a =>
  InterpreterFor (Jwt a !! e) r
interpretJwtPersistent =
  interpretResumable \case
    Key ->
      restop atomicGet
    Settings ->
      restop settingsPersistent
    MakeToken a -> do
      sett <- restop settingsPersistent
      authToken =<< embed (makeJWT a sett Nothing)

type Table_Jwk = Table "jwk" JWK (Name "payload" Json)

table_Jwk :: Sqel Table_Jwk
table_Jwk = sqel

-- | Interpret 'Jwt' using 'interpretJwtPersistent' and interpret 'AtomicState' as a PostgreSQL table using
-- @polysemy-hasql@, generating the JWK when it is not found in the database.
interpretJwtDb ::
  ∀ a r .
  Members [Database !! DbError, Error InitDbError, Error Text, Log, Mask, Resource, Race, Embed IO] r =>
  ToJWT a =>
  InterpreterFor (Jwt a !! DbError) r
interpretJwtDb =
  interpretGenJwk .
  interpretTable table_Jwk .
  interpretAtomicStateDb table_Jwk genJwk .
  interpretJwtPersistent .
  insertAt @1
