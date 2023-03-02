{-# options_haddock prune #-}

-- | Description: Interpreter for the query for an account by name
module Polysemy.Account.Interpreter.AccountByName where

import Polysemy.Db (DbError, PureStore, Query, Store, interpretQueryStoreConc)
import Sqel (Uid (Uid))

import Polysemy.Account.Data.Account (Account (Account))
import Polysemy.Account.Data.AccountByName (AccountByName (AccountByName))

match ::
  AccountByName ->
  Uid i (Account p) ->
  Maybe (Uid i (Account p))
match (AccountByName name) a@(Uid _ (Account accountName _ _))
  | name == accountName = Just a
  | otherwise = Nothing

-- | The effects handled by 'interpretAccountByNameState'.
type AccountQuery i p =
  [
    Query AccountByName (Maybe (Uid i (Account p))) !! DbError,
    Store i (Account p) !! DbError,
    AtomicState (PureStore i (Account p))
  ]

-- | Interpret @'Query' 'AccountByName'@ and the corresponding 'Store' in an 'AtomicState'.
interpretAccountByNameState ::
  ∀ i p r .
  Ord i =>
  Show i =>
  Member (Embed IO) r =>
  [Uid i (Account p)] ->
  InterpretersFor (AccountQuery i p) r
interpretAccountByNameState initial =
  interpretQueryStoreConc match initial
