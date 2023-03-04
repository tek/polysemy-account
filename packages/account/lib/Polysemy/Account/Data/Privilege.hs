{-# options_haddock prune #-}

-- | Description: Privilege data type
module Polysemy.Account.Data.Privilege where

import qualified Data.Set as Set
import GHC.Exts (IsList (fromList))

-- | The stock privilege type, used only for admin endpoint authorization in @polysemy-account-api@.
data Privilege =
  Web
  |
  Api
  |
  Admin
  deriving stock (Eq, Show, Generic, Ord, Enum, Bounded)

json ''Privilege

-- | The default privilege type for the effects and data types in this library.
newtype Privileges =
  Privileges { unPrivileges :: Set Privilege }
  deriving stock (Eq, Show, Generic)
  deriving newtype (Semigroup, Monoid, IsList)

json ''Privileges

instance {-# overlapping #-} Default Privileges where
  def = [Web]

-- | The default endpoint parameter for checking authorization.
newtype RequiredPrivileges =
  RequiredPrivileges { unRequiredPrivileges :: Privileges }
  deriving stock (Eq, Show, Generic)
  deriving newtype (Semigroup, Monoid, IsList)

json ''RequiredPrivileges

-- | Indicate whether the 'Privilege' is present in the 'Privileges'.
satisfiesPrivilege :: Privilege -> Privileges -> Bool
satisfiesPrivilege req (Privileges present) =
  elem req present

-- | Indicate whether all of the 'RequiredPrivileges' are present in the 'Privileges'.
satisfiesPrivileges :: RequiredPrivileges -> Privileges -> Bool
satisfiesPrivileges (RequiredPrivileges (Privileges required)) privs =
  all (flip satisfiesPrivilege privs) required

-- | Return the elements from 'RequiredPrivileges' that aren't present in the 'Privileges' in a 'Just', or 'Nothing' if
-- all are present.
unsatisfiedPrivileges :: RequiredPrivileges -> Privileges -> Maybe Privileges
unsatisfiedPrivileges (RequiredPrivileges (Privileges required)) privs =
  case Set.filter (not . flip satisfiesPrivilege privs) required of
    [] -> Nothing
    missing -> Just (Privileges missing)

-- | Default values for user and admin privileges.
class DefaultPrivileges p where
  -- | The privileges assigned to a newly created user if no explicit value was specified.
  defaultPrivileges :: p

  -- | The privileges assigned to a newly created admin if no explicit value was specified.
  defaultAdminPrivileges :: p

instance DefaultPrivileges Privileges where
  defaultPrivileges = [Web]
  defaultAdminPrivileges = fromList (enumFromTo minBound maxBound)
