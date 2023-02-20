module Polysemy.Account.Data.AuthenticateError where

import Polysemy.Account.Data.AccountName (AccountName)

data AuthenticateError =
  NoSuchAccount AccountName
  |
  Backend Text
  |
  InvalidCredentials AccountName
  |
  Locked AccountName
  deriving stock (Eq, Show)
