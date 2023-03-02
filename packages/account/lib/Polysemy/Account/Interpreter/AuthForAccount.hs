{-# options_haddock prune #-}

-- | Description: Interpreter for the query for auth info by account ID
module Polysemy.Account.Interpreter.AuthForAccount where

import Polysemy.Db (DbError, PureStore, Query, Store, interpretQueryStoreConc)
import Sqel (Uid (Uid))

import Polysemy.Account.Data.AccountAuth (AccountAuth (AccountAuth))
import Polysemy.Account.Data.AuthForAccount (AuthForAccount (AuthForAccount))

match ::
  Eq i =>
  AuthForAccount i ->
  Uid i (AccountAuth i) ->
  Maybe (Uid i (AccountAuth i))
match (AuthForAccount queryId) a@(Uid _ (AccountAuth accountId _ _ _))
  | queryId == accountId = Just a
  | otherwise = Nothing

-- | The effects handled by 'interpretAuthForAccountState'..
type AuthQuery i p =
  [
    Query (AuthForAccount i) [Uid i (AccountAuth i)] !! DbError,
    Store i (AccountAuth i) !! DbError,
    AtomicState (PureStore i (AccountAuth i))
  ]

-- | Interpret @'Query' 'AccountAuth'@ and the corresponding 'Store' in an 'AtomicState'.
interpretAuthForAccountState ::
  ∀ i r p .
  Ord i =>
  Show i =>
  Member (Embed IO) r =>
  [Uid i (AccountAuth i)] ->
  InterpretersFor (AuthQuery i p) r
interpretAuthForAccountState initial =
  interpretQueryStoreConc match initial
