-- | Description: Account by name query data type
module Polysemy.Account.Data.AccountByName where

import Polysemy.Account.Data.AccountName (AccountName)

-- | Query payload for looking up accounts by name.
data AccountByName =
  AccountByName { name :: AccountName }
  deriving stock (Eq, Show, Generic, Ord)

instance IsString AccountByName where
  fromString =
    AccountByName . fromString
