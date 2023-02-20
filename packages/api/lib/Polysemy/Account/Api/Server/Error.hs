module Polysemy.Account.Api.Server.Error where

import Exon.Quote (exon)
import qualified Polysemy.Log as Log
import Servant (ServerError, err401, err409, err500)

import Polysemy.Account.Api.Error (errJson, internal)
import Polysemy.Account.Data.AccountsError (
  AccountsClientError (Conflict, InvalidAuth, NoAccountId, NoAccountName),
  AccountsError (Client, Internal),
  )

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
    respondError err409 "Account already exists"
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
