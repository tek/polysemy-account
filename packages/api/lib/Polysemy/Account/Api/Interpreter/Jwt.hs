{-# options_ghc -fno-warn-orphans #-}

module Polysemy.Account.Api.Interpreter.Jwt where

import qualified Crypto.JOSE as JOSE
import Crypto.JOSE (JWK, KeyMaterialGenParam (OKPGenParam), OKPCrv (Ed25519), genJWK)
import Polysemy.Conc.AtomicState (interpretAtomic)
import Polysemy.Db.Data.DbError (DbError)
import Polysemy.Db.Data.InitDbError (InitDbError)
import Polysemy.Hasql (Database, interpretAtomicStateDb, interpretTable)
import Servant.Auth.JWT (ToJWT)
import Servant.Auth.Server (FromJWT, JWTSettings, defaultJWTSettings, makeJWT)
import Sqel (tableName)
import Sqel.Data.TableSchema (TableSchema)
import Sqel.Names (named)
import Sqel.PgType (tableSchema)
import qualified Sqel.Prim as Sqel

import Polysemy.Account.Api.Effect.Jwt (GenJwk (GenJwk), Jwt (..), genJwk)
import Polysemy.Account.Data.AuthToken (AuthToken (AuthToken))
import Polysemy.Account.Data.AuthedAccount (AuthedAccount)

instance (FromJSON i, FromJSON p) => FromJWT (AuthedAccount i p) where
instance (ToJSON i, ToJSON p) => ToJWT (AuthedAccount i p) where

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

-- errors in this interpreter are critical
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

interpretJwtDb ::
  ∀ a r .
  Members [Database !! DbError, Error InitDbError, Error Text, Log, Mask, Resource, Race, Embed IO] r =>
  ToJWT a =>
  InterpreterFor (Jwt a !! DbError) r
interpretJwtDb =
  interpretGenJwk .
  interpretTable ts .
  interpretAtomicStateDb ts genJwk .
  interpretJwtPersistent .
  insertAt @1
  where
    ts :: TableSchema JWK
    ts = tableSchema (tableName "jwk" (named @"payload" Sqel.json))
