module Polysemy.Account.Data.AuthToken where

newtype AuthToken =
  AuthToken { unAuthToken :: Text }
  deriving stock (Eq, Show)

json ''AuthToken
