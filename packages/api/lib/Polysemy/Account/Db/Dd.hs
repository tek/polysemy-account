{-# options_ghc -Wno-partial-type-signatures #-}

module Polysemy.Account.Db.Dd where

import Sqel (Dd, Sqel, Uid, array, enum, nullable, pk, prim, primNewtype, prod, uid, type (:>) ((:>)))
import Sqel.Codec (PrimColumn)
import Sqel.Comp (Column)
import Sqel.Data.Codec (FullCodec)
import Sqel.Data.TableSchema (TableSchema)
import Sqel.Ext (named)
import Sqel.PgType (tableSchema)
import Sqel.ReifyCodec (ReifyCodec)
import Sqel.ReifyDd (ReifyDd)

import Polysemy.Account.Data.Account (Account, AccountP)
import Polysemy.Account.Data.AccountAuth (AccountAuth)

account ::
  Column p "privileges" s s =>
  Dd s ->
  Sqel (Uid i (Account p)) _
account p =
  uid (pk prim) (prod (primNewtype :> enum :> p))

accountP :: Sqel (Uid i AccountP) _
accountP =
  account (named @"privileges" (array enum))

accountSchema ::
  PrimColumn i =>
  Column p "privileges" s s =>
  ReifyCodec FullCodec s p =>
  ReifyDd s =>
  Dd s ->
  TableSchema (Uid i (Account p))
accountSchema p =
  tableSchema (account p)

accountSchemaP ::
  PrimColumn i =>
  TableSchema (Uid i AccountP)
accountSchemaP =
  tableSchema accountP

accountAuth ::
  Sqel (Uid i (AccountAuth i)) _
accountAuth =
  uid (pk prim) (prod (prim :> primNewtype :> primNewtype :> nullable prim))

accountAuthSchema ::
  PrimColumn i =>
  TableSchema (Uid i (AccountAuth i))
accountAuthSchema =
  tableSchema accountAuth
