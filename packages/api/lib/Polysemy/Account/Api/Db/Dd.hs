-- | Description: Sqel Dd definitions for account and auth tables
module Polysemy.Account.Api.Db.Dd where

import Chronos (Datetime)
import Prelude hiding (Mod)
import Sqel (
  Array,
  Dd,
  EnumColumn,
  Mod,
  Mods,
  Newtyped,
  Nullable,
  Prim,
  PrimNewtype,
  PrimaryKey,
  Prod,
  Uid,
  UidDd,
  array,
  enum,
  newtyped,
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
import Polysemy.Account.Data.AccountAuthDescription (AccountAuthDescription)
import Polysemy.Account.Data.AccountName (AccountName)
import Polysemy.Account.Data.AccountStatus (AccountStatus)
import Polysemy.Account.Data.HashedPassword (HashedPassword)
import Polysemy.Account.Data.Privilege (Privileges)

-- | The sqel type for @'Uid' i ('Account' p)@.
type DdAccount i p s =
  UidDd (Mod PrimaryKey (Prim "id" i)) (
    Prod (Account p) *>
    PrimNewtype "name" AccountName >
    Mods [PgPrimName, EnumColumn] (Prim "status" AccountStatus) >
    s
  )

-- | The sqel type for @Privileges@.
type DdPrivileges = Newtyped Privileges (Array Set (Mods [PgPrimName, EnumColumn] (Prim "privileges" Privileges)))

-- | The database definition for 'Privileges'.
privileges :: Dd DdPrivileges
privileges =
  named @"privileges" (newtyped (array enum))

-- | The database definition for 'Account'.
account ::
  Column p "privileges" s s =>
  Dd s ->
  Dd (DdAccount i p s)
account p =
  uid (pk prim) (prod (primNewtype :> enum :> p))

-- | The database definition for 'AccountP'.
accountP :: Dd (DdAccount i Privileges DdPrivileges)
accountP =
  account privileges

-- | The database schema for 'Account'.
accountSchema ::
  PrimColumn i =>
  Column p "privileges" s s =>
  ReifyCodec FullCodec s p =>
  ReifyDd s =>
  Dd s ->
  TableSchema (Uid i (Account p))
accountSchema p =
  tableSchema (account p)

-- | The database schema for 'AccountP'.
accountSchemaP ::
  PrimColumn i =>
  TableSchema (Uid i AccountP)
accountSchemaP =
  tableSchema accountP

-- | The sqel type for @'Uid' i ('AccountAuth' i)@.
type DdAccountAuth i =
  UidDd (Mod PrimaryKey (Prim "id" i)) (
    Prod (AccountAuth i) *>
    Prim "account" i >
    PrimNewtype "description" AccountAuthDescription >
    PrimNewtype "password" HashedPassword >
    Mod Nullable (Prim "expiry" (Maybe Datetime))
  )

-- | The database definition for 'AccountAuth'.
accountAuth ::
  Dd (DdAccountAuth i)
accountAuth =
  uid (pk prim) (prod (prim :> primNewtype :> primNewtype :> nullable prim))

-- | The database schema for 'AccountAuth'.
accountAuthSchema ::
  PrimColumn i =>
  TableSchema (Uid i (AccountAuth i))
accountAuthSchema =
  tableSchema accountAuth
