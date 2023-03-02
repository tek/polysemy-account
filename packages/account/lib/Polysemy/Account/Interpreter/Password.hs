-- | Description: Interpreters for 'Password'
module Polysemy.Account.Interpreter.Password where

import Data.Elocrypt (genOptions, genPassword)
import Data.Password.Argon2 (
  PasswordCheck (PasswordCheckSuccess),
  PasswordHash (PasswordHash),
  checkPassword,
  hashPassword,
  mkPassword,
  )
import qualified Data.Text as Text
import System.Random (getStdGen)

import Polysemy.Account.Data.GeneratedPassword (GeneratedPassword (GeneratedPassword))
import Polysemy.Account.Data.HashedPassword (HashedPassword (HashedPassword))
import Polysemy.Account.Data.RawPassword (RawPassword (UnsafeRawPassword))
import Polysemy.Account.Effect.Password (Password (..))

-- | Interpret 'Password' trivially, not performing any hashing and generating sequences of asterisks.
interpretPasswordId ::
  InterpreterFor Password r
interpretPasswordId =
  interpret \case
    Hash (UnsafeRawPassword pw) ->
      pure (HashedPassword pw)
    Check (UnsafeRawPassword pw) (HashedPassword apw) ->
      pure (pw == apw)
    Generate len ->
      pure (GeneratedPassword (Text.replicate (fromIntegral len) "*"))

-- | Interpret 'Password' using the Argon2 algorithm and "Data.Elocrypt"-generated passwords.
interpretPassword ::
  Member (Embed IO) r =>
  InterpreterFor Password r
interpretPassword =
  interpret \case
    Hash (UnsafeRawPassword pw) ->
      coerce <$> hashPassword (mkPassword pw)
    Check (UnsafeRawPassword pw) (HashedPassword apw) ->
      pure (PasswordCheckSuccess == checkPassword (mkPassword pw) (PasswordHash apw))
    Generate len ->
      GeneratedPassword . toText . fst . genPassword (fromIntegral len) genOptions <$> embed getStdGen
