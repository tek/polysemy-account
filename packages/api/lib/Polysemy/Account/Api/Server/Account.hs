module Polysemy.Account.Api.Server.Account where

import Servant (ServerError, ServerT, (:<|>) ((:<|>)))
import Servant.API (NoContent (NoContent))
import Sqel (Uid (Uid))

import Polysemy.Account.Api.Routes (AccountApi)
import Polysemy.Account.Api.Server.AuthEndpoint (adminOnly1_, adminOnly2_, adminOnly_)
import Polysemy.Account.Api.Server.Error (accountsError)
import Polysemy.Account.Data.Account (Account)
import Polysemy.Account.Data.AccountsError (AccountsError)
import Polysemy.Account.Data.Privilege (AccountIsAdmin)
import qualified Polysemy.Account.Effect.Accounts as Accounts
import Polysemy.Account.Effect.Accounts (Accounts)

getAccount ::
  Members [Accounts i p !! AccountsError, Log, Stop ServerError] r =>
  i ->
  Sem r (Uid i (Account p))
getAccount i =
  Accounts.byId i !! accountsError

getAccounts ::
  Members [Accounts i p !! AccountsError, Log, Stop ServerError] r =>
  Sem r [Uid i (Account p)]
getAccounts =
  Accounts.all !! accountsError

putAccount ::
  Members [Accounts i p !! AccountsError, Log, Stop ServerError] r =>
  Uid i (Account p) ->
  Sem r NoContent
putAccount account =
  NoContent <$ Accounts.update account !! accountsError

putAccount' ::
  Members [Accounts i p !! AccountsError, Log, Stop ServerError] r =>
  i ->
  Account p ->
  Sem r NoContent
putAccount' i account =
  NoContent <$ Accounts.update (Uid i account) !! accountsError

accountServer ::
  Show i =>
  Show p =>
  AccountIsAdmin p =>
  Members [Accounts i p !! AccountsError, Log, Stop ServerError] r =>
  ServerT (AccountApi i p) (Sem r)
accountServer =
  adminOnly1_ getAccount :<|> adminOnly_ getAccounts :<|> adminOnly1_ putAccount :<|> adminOnly2_ putAccount'
