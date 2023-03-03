-- | Description: Interpreter for 'Authorize'
module Polysemy.Account.Api.Interpreter.Authorize where

import Exon (exon)

import Polysemy.Account.Effect.Authorize (Authorize (Authorize))
import qualified Polysemy.Account.Data.AuthedAccount as AuthedAccount
import Polysemy.Account.Data.AuthedAccount (AuthedAccount (AuthedAccount))
import Polysemy.Account.Data.Privilege (Privilege)

-- | Interpret 'Authorize' using a monadic predicate.
interpretAuthorizeWith ::
  (param -> AuthedAccount i priv -> Sem r (Maybe Text)) ->
  InterpreterFor (Authorize i param priv) r
interpretAuthorizeWith check =
  interpret \case
    Authorize required account ->
      check required account

-- | Interpret 'Authorize' using 'Privilege' for both parameter and privilege types.
--
-- Simply verify that all parameter privileges are present in the account.
interpretAuthorizeP ::
  InterpreterFor (Authorize i [Privilege] [Privilege]) r
interpretAuthorizeP =
  interpret \case
    Authorize required AuthedAccount {privileges} ->
      pure $ case filter (not . flip elem privileges) required of
        [] -> Nothing
        missing -> Just [exon|Missing privileges: #{show missing}|]
