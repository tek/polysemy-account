-- | Description: Password effect
module Polysemy.Account.Effect.Password where

import Polysemy.Account.Data.GeneratedPassword (GeneratedPassword)
import Polysemy.Account.Data.HashedPassword (HashedPassword)
import Polysemy.Account.Data.RawPassword (RawPassword)

-- | This effect provides password hashing, validation, and generation.
data Password :: Effect where
  -- | Hash a clear text password.
  Hash :: RawPassword -> Password m HashedPassword
  -- | Validate a password against a hash.
  Check :: RawPassword -> HashedPassword -> Password m Bool
  -- | Generate a new clear text password of the specified length.
  Generate :: Word -> Password m GeneratedPassword

makeSem ''Password
