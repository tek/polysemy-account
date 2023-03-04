{-# options_haddock prune #-}

-- | Description: Interpreters for 'Accounts' and 'Password'
module Polysemy.Account.Interpreter.Accounts where

import Chronos (Datetime)
import Polysemy.Db (DbError, Id, Query, Store, newId)
import qualified Polysemy.Db.Effect.Query as Query
import qualified Polysemy.Db.Effect.Store as Store
import Sqel (Uid (Uid))

import Polysemy.Account.Data.Account (Account (Account))
import Polysemy.Account.Data.AccountAuth (AccountAuth (AccountAuth))
import Polysemy.Account.Data.AccountAuthDescription (AccountAuthDescription)
import Polysemy.Account.Data.AccountByName (AccountByName (AccountByName))
import Polysemy.Account.Data.AccountName (AccountName)
import qualified Polysemy.Account.Data.AccountStatus as AccountStatus
import Polysemy.Account.Data.AccountStatus (AccountStatus)
import qualified Polysemy.Account.Data.AccountsConfig as AccountsConfig
import Polysemy.Account.Data.AccountsConfig (AccountsConfig (AccountsConfig))
import Polysemy.Account.Data.AccountsError (
  AccountsClientError (Conflict, InvalidAuth, NoAccountId, NoAccountName),
  AccountsError (Client, Internal),
  )
import Polysemy.Account.Data.AuthForAccount (AuthForAccount (AuthForAccount))
import Polysemy.Account.Data.AuthedAccount (AuthedAccount (AuthedAccount))
import Polysemy.Account.Data.GeneratedPassword (GeneratedPassword (GeneratedPassword))
import Polysemy.Account.Data.RawPassword (RawPassword (UnsafeRawPassword))
import Polysemy.Account.Effect.Accounts (Accounts (..))
import qualified Polysemy.Account.Effect.Password as Password
import Polysemy.Account.Effect.Password (Password)
import Polysemy.Account.Interpreter.AccountByName (interpretAccountByNameState)
import Polysemy.Account.Interpreter.AuthForAccount (interpretAuthForAccountState)
import Polysemy.Account.Interpreter.Password (interpretPasswordId)

dbError ::
  ∀ eff e r .
  Show e =>
  Members [eff !! e, Stop AccountsError] r =>
  InterpreterFor eff r
dbError =
  resumeHoist (Internal . show)

storeError ::
  ∀ a e i r .
  Show e =>
  Members [Store i a !! e, Stop AccountsError] r =>
  InterpreterFor (Store i a) r
storeError =
  resumeHoist (Internal . show)

queryError ::
  ∀ a q e r .
  Show e =>
  Members [Query q a !! e, Stop AccountsError] r =>
  InterpreterFor (Query q a) r
queryError =
  resumeHoist (Internal . show)

config ::
  Show e =>
  Members [Reader (AccountsConfig p) !! e, Stop AccountsError] r =>
  Sem r (AccountsConfig p)
config =
  dbError ask

byId ::
  ∀ i r a .
  Members [Store i a, Stop AccountsError] r =>
  i ->
  Sem r (Uid i a)
byId accountId =
  stopNote (Client NoAccountId) =<< Store.fetch accountId

byName ::
  ∀ i r a .
  Members [Query AccountByName (Maybe (Uid i a)), Stop AccountsError] r =>
  AccountName ->
  Sem r (Uid i a)
byName name =
  stopNote (Client NoAccountName) =<< Query.query (AccountByName name)

authedAccount ::
  ∀ i p r .
  Members [Store i (Account p), Store i (AccountAuth i), Stop AccountsError] r =>
  i ->
  Sem r (AuthedAccount i p)
authedAccount authId = do
  aa <- Store.fetch authId
  Uid _ (AccountAuth accountId _ _ _) <- stopNote (Client InvalidAuth) aa
  Uid _ (Account name status privs) <- byId accountId
  pure (AuthedAccount accountId authId name status privs)

-- TODO see if Query for AccountAuth can be used without Uid, extracting it in the interpreter
authenticate ::
  Show e =>
  Member (Query AccountByName (Maybe (Uid i a)) !! e) r =>
  Member (Query (AuthForAccount i) [Uid i (AccountAuth i)] !! e) r =>
  Members [Stop AccountsError, Password] r =>
  AccountName ->
  RawPassword ->
  Sem r (Uid i (AccountAuth i))
authenticate name password = do
  Uid i _ <- notFound =<< queryError (Query.query (AccountByName name))
  auths <- queryError (Query.query (AuthForAccount i))
  invalid =<< findM check auths
  where
    notFound =
      stopNote (Client NoAccountName)
    check (Uid _ (AccountAuth _ _ hash _)) =
      Password.check password hash
    invalid =
      stopNote (Client InvalidAuth)

privileges ::
  ∀ i p r .
  Members [Store i (Account p), Stop AccountsError] r =>
  i ->
  Sem r p
privileges i =
  Store.fetch i >>= \case
    Just (Uid _ (Account _ _ privs)) -> pure privs
    Nothing -> stop (Client NoAccountId)

addPassword ::
  Members [Password, Store i (AccountAuth i), Id i, Stop AccountsError] r =>
  AccountAuthDescription ->
  i ->
  RawPassword ->
  Maybe Datetime ->
  Sem r (Uid i (AccountAuth i))
addPassword desc accountId password expiry = do
  hashedPassword <- Password.hash password
  authId <- newId
  let auth = Uid authId (AccountAuth accountId desc hashedPassword expiry)
  auth <$ Store.insert auth

generatePassword ::
  Show e =>
  Members [Password, Store i (AccountAuth i), Reader (AccountsConfig p) !! e, Id i, Stop AccountsError] r =>
  i ->
  Maybe Datetime ->
  Sem r GeneratedPassword
generatePassword accountId expiry = do
  AccountsConfig {..} <- config
  pw@(GeneratedPassword raw) <- Password.generate passwordLength
  coerce pw <$ addPassword "auth token" accountId (UnsafeRawPassword raw) expiry

-- | Fail if the account name is already present in the store.
-- If the account status is `AccountStatus.Creating', however, a previous attempt has failed critically and the account
-- can be overwritten.
deletePreviousFailure ::
  Members [Store i (Account p), Stop AccountsError] r =>
  Uid i (Account p) ->
  Sem r ()
deletePreviousFailure (Uid i (Account _ AccountStatus.Creating _)) =
  void (Store.delete i)
deletePreviousFailure _ =
  stop (Client Conflict)

create ::
  ∀ i p e r .
  Members [Store i (Account p), Query AccountByName (Maybe (Uid i (Account p))), Reader (AccountsConfig p) !! e] r =>
  Members [Id i, Stop AccountsError] r =>
  AccountName ->
  p ->
  Sem r (Uid i (Account p))
create name privs = do
  traverse_ deletePreviousFailure =<< Query.query (AccountByName name)
  accountId <- newId
  let account = Uid accountId (Account name AccountStatus.Creating privs)
  account <$ Store.upsert account

finishCreate ::
  ∀ i p r .
  Members [Store i (Account p), Stop AccountsError] r =>
  Bool ->
  i ->
  Sem r (Uid i (Account p))
finishCreate active accountId = do
  account :: Uid i (Account p) <- stopNote (Internal "Account absent after password creation") =<< Store.fetch accountId
  let updatedAccount = account & #payload . #status .~ status
  updatedAccount <$ Store.upsert (account & #payload . #status .~ status)
  where
    status = if active then AccountStatus.Active else AccountStatus.Pending

setStatus ::
  Members [Store i (Account p), Stop AccountsError] r =>
  i ->
  AccountStatus ->
  Sem r ()
setStatus accountId status = do
  account <- stopNote (Client NoAccountId) =<< Store.fetch accountId
  Store.upsert (account & #payload . #status .~ status)

updatePrivileges ::
  ∀ i p e r .
  Show e =>
  Members [Store i (Account p) !! e, Stop AccountsError] r =>
  i ->
  (p -> p) ->
  Sem r ()
updatePrivileges i f =
  dbError (Store.fetch i) >>= \case
    Just account ->
      dbError (Store.upsert (account & #payload . #privileges %~ f))
    Nothing ->
      stop (Client NoAccountId)

-- | Interpret 'Accounts' using 'Store' and 'Query' from [Polysemy.Db]("Polysemy.Db") as the storage backend.
interpretAccounts ::
  ∀ e i p r .
  Show e =>
  Member (Query AccountByName (Maybe (Uid i (Account p))) !! e) r =>
  Member (Query (AuthForAccount i) [Uid i (AccountAuth i)] !! e) r =>
  Members [Password, Store i (Account p) !! e, Store i (AccountAuth i) !! e, Reader (AccountsConfig p) !! e, Id i] r =>
  InterpreterFor (Accounts i p !! AccountsError) r
interpretAccounts =
  interpretResumable \case
    Authenticate name password ->
      authenticate name password
    GeneratePassword accountId expiry ->
      storeError (generatePassword accountId expiry)
    Create name privs -> do
      AccountsConfig {..} <- config
      queryError (storeError (create name (fromMaybe defaultPrivileges privs)))
    FinalizeCreate accountId -> do
      AccountsConfig {..} <- config
      storeError (finishCreate initActive accountId)
    AddPassword accountId password expiry ->
      storeError (addPassword "user login" accountId password expiry)
    SetStatus accountId status ->
      storeError (setStatus accountId status)
    ById accountId ->
      storeError (byId accountId)
    ByName name ->
      queryError (byName name)
    Authed authId ->
      storeError (storeError @(Account _) (authedAccount authId))
    Auths accountId ->
      queryError (Query.query (AuthForAccount accountId))
    Update account ->
      storeError (Store.upsert account)
    Privileges i ->
      storeError (privileges i)
    UpdatePrivileges i f ->
      updatePrivileges i f
    All ->
      storeError Store.fetchAll
    AllAuths ->
      storeError Store.fetchAll

-- | Interpret 'Accounts' and 'Password' using 'AtomicState' as storage backend.
--
-- This variant uses 'Reader' for the config.
interpretAccountsState' ::
  ∀ i p r .
  Ord i =>
  Show i =>
  Members [Reader (AccountsConfig p) !! DbError, Id i, Log, Embed IO] r =>
  [Uid i (Account p)] ->
  [Uid i (AccountAuth i)] ->
  InterpretersFor [Accounts i p !! AccountsError, Password] r
interpretAccountsState' accounts auths =
  interpretPasswordId .
  interpretAccountByNameState accounts .
  interpretAuthForAccountState auths .
  interpretAccounts .
  insertAt @1

-- | Interpret 'Accounts' and 'Password' using 'AtomicState' as storage backend.
interpretAccountsState ::
  ∀ i p r .
  Ord i =>
  Show i =>
  Members [Log, Id i, Embed IO] r =>
  AccountsConfig p ->
  [Uid i (Account p)] ->
  [Uid i (AccountAuth i)] ->
  InterpretersFor [Accounts i p !! AccountsError, Password, Reader (AccountsConfig p) !! DbError] r
interpretAccountsState conf accounts auths =
  raiseResumable (runReader conf) .
  interpretAccountsState' accounts auths
