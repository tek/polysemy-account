module Polysemy.Account.Effect.Password where

import Polysemy.Account.Data.AccountPassword (AccountPassword)
import Polysemy.Account.Data.RawPassword (RawPassword)

data Password :: Effect where
  Hash :: RawPassword -> Password m AccountPassword
  Check :: RawPassword -> AccountPassword -> Password m Bool
  Token :: Password m RawPassword

makeSem ''Password
