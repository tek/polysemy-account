-- | Description: Raw password data type
module Polysemy.Account.Data.RawPassword where

import qualified Text.Show as Show

-- | A clear text password, supplied by the user or generated.
newtype RawPassword =
  UnsafeRawPassword { unRawPassword :: Text }
  deriving stock (Eq)

instance Show RawPassword where
  show _ =
    "--> raw password <--"

json ''RawPassword

-- | Construct a ''RawPassword'.
rawPassword :: Text -> RawPassword
rawPassword = UnsafeRawPassword
