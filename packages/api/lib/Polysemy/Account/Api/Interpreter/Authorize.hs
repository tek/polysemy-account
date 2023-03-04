-- | Description: Interpreter for 'Authorize'
module Polysemy.Account.Api.Interpreter.Authorize where

import Exon (exon)

import qualified Polysemy.Account.Data.AuthedAccount as AuthedAccount
import Polysemy.Account.Data.AuthedAccount (AuthedAccount (AuthedAccount))
import Polysemy.Account.Data.Privilege (unsatisfiedPrivileges)
import Polysemy.Account.Effect.Authorize (Authorize (Authorize), AuthorizeP)

-- | Interpret 'Authorize' using a monadic predicate.
interpretAuthorizeWith ::
  (param -> AuthedAccount i priv -> Sem r (Maybe Text)) ->
  InterpreterFor (Authorize i param priv) r
interpretAuthorizeWith check =
  interpret \case
    Authorize required account ->
      check required account

-- | Interpret 'Authorize' using 'Polysemy.Account.RequiredPrivileges' for the parameter and
-- 'Polysemy.Account.Privileges' for the privilege type.
--
-- Simply verifies that all parameter privileges are present in the account.
interpretAuthorizeP ::
  InterpreterFor (AuthorizeP i) r
interpretAuthorizeP =
  interpret \case
    Authorize required AuthedAccount {privileges} ->
      pure $ unsatisfiedPrivileges required privileges <&> \ missing ->
        [exon|Missing privileges: #{show missing}|]
