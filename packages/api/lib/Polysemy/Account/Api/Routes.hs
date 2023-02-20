module Polysemy.Account.Api.Routes where

import Servant.API (Capture, Get, JSON, NoContent, PostCreated, PostResetContent, Put, ReqBody, (:<|>), (:>))
import Sqel (Uid)

import Polysemy.Account.Api.Data.Authed (Authed)
import Polysemy.Account.Data.Account (Account)
import Polysemy.Account.Data.AccountCredentials (AccountCredentials)
import Polysemy.Account.Data.AuthToken (AuthToken)
import Polysemy.Account.Data.AuthedAccount (AuthedAccount)
import Polysemy.Account.Data.Privilege (Privilege)

type AuthApi i p =
  "auth" :> (
    Authed i p :> Get '[JSON] (AuthedAccount i p)
    :<|>
    "login" :> ReqBody '[JSON] AccountCredentials :> PostResetContent '[JSON] AuthToken
    :<|>
    "register" :> ReqBody '[JSON] AccountCredentials :> PostCreated '[JSON] AuthToken
  )

type AuthApiP i = AuthApi i [Privilege]

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

type AccountApiP i = AccountApi i [Privilege]
