{-# options_haddock prune #-}

-- | Description: Privilege data type
module Polysemy.Account.Data.Privilege where

-- | The stock privilege type, used only for admin endpoint authorization in @polysemy-account-api@.
data Privilege =
  Web
  |
  Api
  |
  Admin
  deriving stock (Eq, Show, Generic)

json ''Privilege

instance {-# overlapping #-} Default [Privilege] where
  def = [Web]
