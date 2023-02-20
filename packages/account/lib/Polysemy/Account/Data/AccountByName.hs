module Polysemy.Account.Data.AccountByName where

import Polysemy.Account.Data.AccountName (AccountName)

data AccountByName =
  AccountByName { name :: AccountName }
  deriving stock (Eq, Show, Generic, Ord)

instance IsString AccountByName where
  fromString =
    AccountByName . fromString
