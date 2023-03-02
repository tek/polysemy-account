{-# options_haddock prune #-}
{-# options_ghc -Wno-partial-type-signatures #-}

-- | Description: Sqel Dd definitions for account and auth tables
module Polysemy.Account.Db.Dd where

import Prelude hiding (Mod)
import Sqel (
  Dd,
  EnumColumn,
  Mod,
  Mods,
  Prim,
  PrimNewtype,
  PrimaryKey,
  Prod,
  Sqel,
  Uid,
  UidDd,
  array,
  enum,
  nullable,
  pk,
  prim,
  primNewtype,
  prod,
  uid,
  type (*>),
  type (:>) ((:>)),
  type (>),
  )
import Sqel.Codec (PrimColumn)
import Sqel.Comp (Column)
import Sqel.Data.Codec (FullCodec)
import Sqel.Data.PgType (PgPrimName)
import Sqel.Data.TableSchema (TableSchema)
import Sqel.Ext (named)
import Sqel.PgType (tableSchema)
import Sqel.ReifyCodec (ReifyCodec)
import Sqel.ReifyDd (ReifyDd)

import Polysemy.Account.Data.Account (Account, AccountP)
import Polysemy.Account.Data.AccountAuth (AccountAuth)
import Polysemy.Account.Data.AccountName (AccountName)
import Polysemy.Account.Data.AccountStatus (AccountStatus)
import Polysemy.Account.Data.Privilege (Privilege)

type DdAccount i p s =
  UidDd (Mod PrimaryKey (Prim "id" i)) (
    Prod (Account p) *>
    PrimNewtype "name" AccountName >
    Mods [PgPrimName, EnumColumn] (Prim "status" AccountStatus) >
    s
  )

privileges :: Sqel [Privilege] _
privileges =
  named @"privileges" (array enum)

account ::
  Column p "privileges" s s =>
  Dd s ->
  Dd (DdAccount i p s)
account p =
  uid (pk prim) (prod (primNewtype :> enum :> p))

accountP :: Sqel (Uid i AccountP) _
accountP =
  account privileges

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
