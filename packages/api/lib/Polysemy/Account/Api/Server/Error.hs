{-# options_haddock prune #-}

-- | Description: Internal error combinators
module Polysemy.Account.Api.Server.Error where

import qualified Data.Aeson as Aeson
import Exon.Quote (exon)
import qualified Log
import Servant (ServerError, err401, err409, err500, errBody)

import Polysemy.Account.Data.AccountsError (
  AccountsClientError (Conflict, InvalidAuth, NoAccountId, NoAccountName),
  AccountsError (Client, Internal),
  )

serverErrorLBS :: ServerError -> LByteString -> ServerError
serverErrorLBS err e =
  err { errBody = e }

serverError :: ServerError -> Text -> ServerError
serverError err =
  serverErrorLBS err . encodeUtf8

unauthorized :: Text -> ServerError
unauthorized =
  serverError err401

internal :: Text -> ServerError
internal =
  serverError err500

errJson ::
  ToJSON e =>
  ServerError ->
  e ->
  ServerError
errJson err =
  serverErrorLBS err . Aeson.encode

data ClientError =
  ClientError {
    message :: Text
  }
  deriving stock (Eq, Show, Generic)

json ''ClientError

internalAccountsError ::
  Members [Log, Stop ServerError] r =>
  Text ->
  Sem r a
internalAccountsError err = do
  Log.error [exon|Internal accounts error: #{err}|]
  stop (internal "Internal error")

respondError ::
  Member (Stop ServerError) r =>
  ServerError ->
  Text ->
  Sem r a
respondError err msg =
  stop (errJson err (ClientError msg))

accountsError ::
  Members [Log, Stop ServerError] r =>
  AccountsError ->
  Sem r a
accountsError = \case
  Client NoAccountId ->
    respondError err401 "No such account"
  Client InvalidAuth ->
    respondError err401 "Invalid credentials"
  Client NoAccountName ->
    respondError err401 "No such account"
  Client Conflict ->
    respondError err409 "Multiple accounts with same name"
  Internal err ->
    internalAccountsError err

jwtError ::
  Members [Stop ServerError, Log] r =>
  Show e =>
  e ->
  Sem r a
jwtError e = do
  Log.error [exon|JWT generation failed: #{show e}|]
  respondError err500 "Internal error"
