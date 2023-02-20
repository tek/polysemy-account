module Polysemy.Account.Api.Data.Authed where

import Servant.Auth (Auth, JWT)

import Polysemy.Account (Privilege)
import Polysemy.Account.Data.AuthedAccount (AuthedAccount)

type Authed i p =
  Auth '[JWT] (AuthedAccount i p)

type AuthedP i = Authed i [Privilege]
