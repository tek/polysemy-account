module Polysemy.Account.Data.AccountStatus where

data AccountStatus =
  Creating
  |
  Pending
  |
  Active
  |
  Locked
  deriving stock (Eq, Show, Generic)

json ''AccountStatus
