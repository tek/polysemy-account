-- | Description: Sqel Dd definitions for account and auth tables
module Polysemy.Account.Api.Db.Dd where

import Prelude hiding (Enum)
import Sqel (Enum, Newtype, Pk, Prim, Prod, Query, Sqel, UidTable, sqel)
import Sqel.Exts (ReifySqel)

import Polysemy.Account.Data.Account (Account)
import Polysemy.Account.Data.AccountAuth (AccountAuth)
import Polysemy.Account.Data.AccountByName (AccountByName)
import Polysemy.Account.Data.AuthForAccount (AuthForAccount)
import Polysemy.Account.Data.Privilege (Privileges)

-- | The sqel table type for @'Sqel.Uid' i ('Account' p)@.
type Table_Account i p sp =
  UidTable "account" i (Account p) (Pk Prim) (Prod [Newtype, Enum, sp])

-- | The sqel table type for @'Sqel.Uid' i 'Polysemy.Account.AccountP'@.
type Table_AccountP i =
  Table_Account i Privileges (Newtype Enum)

-- | The sqel table for 'Table_Account'.
table_Account ::
  ∀ sp i p .
  ReifySqel (Table_Account i p sp) =>
  Sqel (Table_Account i p sp)
table_Account = sqel

-- | The sqel table for 'Table_AccountP'.
table_AccountP ::
  ∀ i .
  ReifySqel (Table_AccountP i) =>
  Sqel (Table_AccountP i)
table_AccountP = sqel

-- | The sqel table type for @'Sqel.Uid' i ('AccountAuth' i)@.
type Table_AccountAuth i =
  UidTable "account_auth" i (AccountAuth i) Prim (Prod [Prim, Newtype, Newtype, Prim])

-- | The sqel table for 'Table_AccountAuth'.
table_AccountAuth ::
  ∀ i .
  ReifySqel (Table_AccountAuth i) =>
  Sqel (Table_AccountAuth i)
table_AccountAuth = sqel

-- | The sqel query type for @'AuthForAccount' i@.
type Query_AuthForAccount i = Query (AuthForAccount i) (Prod '[Prim])

-- | The sqel query for @'AuthForAccount' i@.
query_authForAccount ::
  ∀ i .
  ReifySqel (Query_AuthForAccount i) =>
  Sqel (Query_AuthForAccount i)
query_authForAccount = sqel

-- | The sqel query type for 'AccountByName'.
type Query_AccountByName = Query AccountByName (Prod '[Newtype])

-- | The sqel query for 'AccountByName'.
query_accountByName :: Sqel Query_AccountByName
query_accountByName = sqel
