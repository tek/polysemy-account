module Polysemy.Account.Interpreter.Password where

import Data.Elocrypt (genOptions, genPassword)
import Data.Password.Argon2 (
  PasswordCheck (PasswordCheckSuccess),
  PasswordHash (PasswordHash),
  checkPassword,
  hashPassword,
  mkPassword,
  )
import System.Random (getStdGen)

import Polysemy.Account.Effect.Password (Password (..))
import Polysemy.Account.Data.AccountPassword (AccountPassword (AccountPassword))
import Polysemy.Account.Data.RawPassword (RawPassword (UnsafeRawPassword))

interpretPasswordId ::
  InterpreterFor Password r
interpretPasswordId =
  interpret \case
    Hash (UnsafeRawPassword pw) ->
      pure (AccountPassword pw)
    Check (UnsafeRawPassword pw) (AccountPassword apw) ->
      pure (pw == apw)
    Token ->
      pure (UnsafeRawPassword "token")

interpretPassword ::
  Member (Embed IO) r =>
  InterpreterFor Password r
interpretPassword =
  interpret \case
    Hash (UnsafeRawPassword pw) ->
      coerce <$> hashPassword (mkPassword pw)
    Check (UnsafeRawPassword pw) (AccountPassword apw) ->
      pure (PasswordCheckSuccess == checkPassword (mkPassword pw) (PasswordHash apw))
    Token -> do
      pw <- toText . fst . genPassword 20 genOptions <$> embed getStdGen
      pure (UnsafeRawPassword pw)
