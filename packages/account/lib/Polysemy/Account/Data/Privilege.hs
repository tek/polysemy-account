module Polysemy.Account.Data.Privilege where

data Privilege =
  Web
  |
  Api
  |
  Admin
  deriving stock (Eq, Show, Generic)

json ''Privilege

isAdmin :: Foldable t => t Privilege -> Bool
isAdmin =
  elem Admin

class AccountIsAdmin p where
  accountIsAdmin :: p -> Bool

instance AccountIsAdmin Privilege where
  accountIsAdmin = \case
    Admin -> True
    _ -> False

instance AccountIsAdmin [Privilege] where
  accountIsAdmin =
    any accountIsAdmin
