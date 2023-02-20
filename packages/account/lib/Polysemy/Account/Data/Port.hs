module Polysemy.Account.Data.Port where

newtype Port =
  Port { unPort :: Int }
  deriving stock (Eq, Show)
  deriving newtype (Num, Ord, Enum, Real, Integral, Read)

json ''Port
