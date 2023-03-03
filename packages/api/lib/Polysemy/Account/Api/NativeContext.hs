{-# options_haddock prune #-}

-- | Description: Full Polysemy runners for Servant servers
module Polysemy.Account.Api.NativeContext where

import Control.Monad.Trans.Except (ExceptT (ExceptT))
import Exon.Quote (exon)
import qualified Log
import Network.Wai (Application)
import qualified Network.Wai.Handler.Warp as Warp
import Network.Wai.Handler.Warp (
  defaultSettings,
  setBeforeMainLoop,
  setGracefulShutdownTimeout,
  setHost,
  setInstallShutdownHandler,
  setPort,
  )
import qualified Network.Wai.Middleware.RequestLogger as Logger
import Network.Wai.Middleware.RequestLogger (destination, mkRequestLogger)
import qualified Polysemy.Conc.Effect.Interrupt as Interrupt
import Polysemy.Final (withWeavingToFinal)
import Servant (
  Context,
  DefaultErrorFormatters,
  ErrorFormatters,
  Handler (Handler),
  HasContextEntry,
  HasServer,
  Server,
  ServerError,
  ServerT,
  err500,
  hoistServerWithContext,
  serveWithContext,
  type (.++),
  )
import qualified Sync
import System.Log.FastLogger (fromLogStr)

import Polysemy.Account.Data.Port (Port (Port))

-- | A dummy value used to indicate that the server has fully started up, using 'Sync'.
data ServerReady = ServerReady
  deriving stock (Eq, Show)

logErrors ::
  Member Log r =>
  Sem r (Either ServerError a) ->
  Sem r (Either ServerError a)
logErrors ma =
  ma >>= \case
    Right a -> pure (Right a)
    Left err -> Left err <$ Log.error (show err)

lowerServer ::
  ∀ (api :: Type) context r s .
  Functor s =>
  Member Log r =>
  HasServer api context =>
  s () ->
  (∀ a . s (Sem r a) -> IO (s a)) ->
  (∀ x . s x -> Maybe x) ->
  ServerT api (Sem (Stop ServerError : r)) ->
  Server api
lowerServer s lower ins srv =
  hoistServerWithContext (Proxy @api) (Proxy @context) handle srv
  where
    handleErrors =
      logErrors . runStop @ServerError
    cons =
      Handler . ExceptT
    handle :: ∀ x . Sem (Stop ServerError : r) x -> Handler x
    handle ma =
      cons (err <$> lower (handleErrors ma <$ s))
      where
        err =
          ins >>> \case
            Just a -> a
            Nothing -> Left err500

-- | Run a Servant server using a callback in @'Final' 'IO'@, sending logs to 'Log'.
runServerSem ::
  ∀ (api :: Type) context r a .
  HasServer api context =>
  HasContextEntry (context .++ DefaultErrorFormatters) ErrorFormatters =>
  Members [Log, Embed IO, Final IO] r =>
  ServerT api (Sem (Stop ServerError : r)) ->
  Context context ->
  (Application -> IO a) ->
  Sem r a
runServerSem srv context f =
  withWeavingToFinal \ s lower ins ->
    (<$ s) <$> f (serveWithContext (Proxy @api) context (lowerServer @api @context s lower ins srv))

toHandler :: IO (Maybe (Either ServerError a)) -> Handler a
toHandler =
  Handler . ExceptT . fmap (fromMaybe (Left err500))

-- | Run a Servant server using Warp in @'Final' 'IO'@, sending logs to 'Log', registering the shutdown handler with
-- 'Interrupt'.
runServer ::
  ∀ (api :: Type) context r .
  HasServer api context =>
  HasContextEntry (context .++ DefaultErrorFormatters) ErrorFormatters =>
  Members [Sync ServerReady, Log, Interrupt, Final IO] r =>
  ServerT api (Sem (Stop ServerError : r)) ->
  Context context ->
  Port ->
  Sem r ()
runServer srv context (Port port) = do
  Log.info [exon|server port: #{show port}|]
  withWeavingToFinal \ s wv ins -> do
    let
      app =
        serveWithContext (Proxy @api) context (hoistServerWithContext (Proxy @api) (Proxy @context) hoist srv)
      hoist :: Sem (Stop ServerError : r) a -> Handler a
      hoist =
        toHandler . fmap ins . wv . (<$ s) . logErrors . runStop @ServerError
      shut h =
        void (wv (Interrupt.register "api" h <$ s))
      settings =
        setHost "*6" $
        setPort (fromIntegral port) $
        setBeforeMainLoop (void (wv (Sync.putBlock ServerReady <$ s))) $
        setInstallShutdownHandler shut $
        setGracefulShutdownTimeout (Just 0) $
        defaultSettings
      log msg =
        void (wv ((Log.debug (decodeUtf8 (fromLogStr msg))) <$ s))
    logger <- mkRequestLogger def { destination = Logger.Callback log }
    (<$ s) <$> Warp.runSettings settings (logger app)
