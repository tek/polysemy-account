module Polysemy.Account.Api.Error where

import qualified Data.Aeson as Aeson
import Servant (ServerError, err401, err500, errBody)

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
