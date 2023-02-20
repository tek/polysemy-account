module Polysemy.Account.Data.RawPassword where

import qualified Text.Show as Show

newtype RawPassword =
  UnsafeRawPassword { unRawPassword :: Text }
  deriving stock (Eq)

instance Show RawPassword where
  show _ =
    "--> raw password <--"

json ''RawPassword

rawPassword :: Text -> RawPassword
rawPassword = UnsafeRawPassword
