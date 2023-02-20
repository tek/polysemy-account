module Polysemy.Account.Interpreter.AccountByName where

import Conc (interpretAtomic)
import Polysemy.Db (PureStore (PureStore), Query, Store, interpretStoreAtomicState)
import Polysemy.Db.Query (interpretQueryAtomicStateOne)
import Sqel (Uid (Uid))

import Polysemy.Account.Data.Account (Account (Account))
import Polysemy.Account.Data.AccountByName (AccountByName (AccountByName))

match ::
  AccountByName ->
  Uid i (Account p) ->
  Bool
match (AccountByName name) (Uid _ (Account accountName _ _)) =
  name == accountName

type AccountQuery i p =
  [
    Query AccountByName (Maybe (Uid i (Account p))) !! (),
    Store i (Account p) !! (),
    AtomicState (PureStore (Uid i (Account p)))
  ]

interpretAccountByNameState ::
  ∀ i p r .
  Eq i =>
  Member (Embed IO) r =>
  [Uid i (Account p)] ->
  InterpretersFor (AccountQuery i p) r
interpretAccountByNameState initial =
  interpretAtomic (PureStore initial) .
  interpretStoreAtomicState .
  interpretQueryAtomicStateOne match
