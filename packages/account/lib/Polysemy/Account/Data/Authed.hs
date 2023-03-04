-- | Description: Servant API markers for JWT auth with 'AuthedAccount'
module Polysemy.Account.Data.Authed where

import Servant.Auth (Auth, JWT)

import Polysemy.Account.Data.AuthedAccount (AuthedAccount)
import Polysemy.Account.Data.Privilege (Privileges)

-- | A Servant API marker for JWT auth with 'AuthedAccount'
type Authed i p = Auth '[JWT] (AuthedAccount i p)

-- | Convenience alias for using the default privilege type with 'Authed'.
type AuthedP i = Authed i Privileges
