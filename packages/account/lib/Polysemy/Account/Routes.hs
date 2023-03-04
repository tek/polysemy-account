-- | Description: Servant routes for the basic auth and account endpoints
module Polysemy.Account.Routes where

import Servant.API (Capture, Get, JSON, NoContent, PostCreated, PostResetContent, Put, ReqBody, (:<|>), (:>))
import Sqel (Uid)

import Polysemy.Account.Data.Account (Account)
import Polysemy.Account.Data.AccountCredentials (AccountCredentials)
import Polysemy.Account.Data.AuthToken (AuthToken)
import Polysemy.Account.Data.Authed (Authed)
import Polysemy.Account.Data.AuthedAccount (AuthedAccount)
import Polysemy.Account.Data.Privilege (Privileges)

-- | An API allowing users to log in, register accounts, and authenticate with a JWT to obtain their account
-- information.
type AuthApi i p =
  "auth" :> (
    Authed i p :> Get '[JSON] (AuthedAccount i p)
    :<|>
    "login" :> ReqBody '[JSON] AccountCredentials :> PostResetContent '[JSON] AuthToken
    :<|>
    "register" :> ReqBody '[JSON] AccountCredentials :> PostCreated '[JSON] AuthToken
  )

-- | Convenience alias for using the default privilege type with 'AuthApi'.
type AuthApiP i = AuthApi i Privileges

-- | An internal API for accessing accounts.
type AccountApi i p =
  "account" :> (
    Authed i p :> Capture "id" i :> Get '[JSON] (Uid i (Account p))
    :<|>
    Authed i p :> Get '[JSON] [Uid i (Account p)]
    :<|>
    Authed i p :> ReqBody '[JSON] (Uid i (Account p)) :> Put '[JSON] NoContent
    :<|>
    Authed i p :> Capture "id" i :> ReqBody '[JSON] (Account p) :> Put '[JSON] NoContent
  )

-- | Convenience alias for using the default privilege type with 'AccountApi'.
type AccountApiP i = AccountApi i Privileges
