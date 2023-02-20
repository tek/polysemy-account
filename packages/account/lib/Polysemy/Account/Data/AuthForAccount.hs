module Polysemy.Account.Data.AuthForAccount where

data AuthForAccount i =
  AuthForAccount { account :: i }
  deriving stock (Eq, Show, Generic, Ord)
