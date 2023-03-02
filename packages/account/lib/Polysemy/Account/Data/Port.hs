-- | Description: Port data type
module Polysemy.Account.Data.Port where

-- | An API port, used by the Servant tools in @polysemy-account-api@.
newtype Port =
  Port { unPort :: Word }
  deriving stock (Eq, Show)
  deriving newtype (Num, Ord, Enum, Real, Integral, Read)

json ''Port
