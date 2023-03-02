-- | Description: Account by name query data type
module Polysemy.Account.Data.AuthForAccount where

-- | Query payload for looking up auth info by account ID.
data AuthForAccount i =
  AuthForAccount { account :: i }
  deriving stock (Eq, Show, Generic, Ord)
