module Polysemy.Account.Interpreter.AuthForAccount where

import Conc (interpretAtomic)
import Polysemy.Db (PureStore (PureStore), Query, Store)
import Polysemy.Db.Query (interpretQueryAtomicStateMulti)
import Polysemy.Db.Store (interpretStoreAtomicState)
import Sqel (Uid (Uid))

import Polysemy.Account.Data.AccountAuth (AccountAuth (AccountAuth))
import Polysemy.Account.Data.AuthForAccount (AuthForAccount (AuthForAccount))

match ::
  Eq i =>
  AuthForAccount i ->
  Uid i (AccountAuth i) ->
  Bool
match (AuthForAccount queryId) (Uid _ (AccountAuth accountId _ _ _)) =
  queryId == accountId

type AuthQuery i p =
  [
    Query (AuthForAccount i) [Uid i (AccountAuth i)] !! (),
    Store i (AccountAuth i) !! (),
    AtomicState (PureStore (Uid i (AccountAuth i)))
  ]

interpretAuthForAccountState ::
  ∀ i r p .
  Eq i =>
  Member (Embed IO) r =>
  [Uid i (AccountAuth i)] ->
  InterpretersFor (AuthQuery i p) r
interpretAuthForAccountState initial =
  interpretAtomic (PureStore initial) .
  interpretStoreAtomicState .
  interpretQueryAtomicStateMulti match
