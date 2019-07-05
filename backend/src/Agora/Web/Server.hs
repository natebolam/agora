{-# LANGUAGE OverloadedLabels #-}

{-|
Utilities for running Agora web server.
-}
module Agora.Web.Server
       ( runAgora
       , serveWeb
       , addrSettings
       ) where

import Fmt ((+|), (|+))
import Loot.Config (option)
import Loot.Log (logInfo)
import qualified Network.Wai.Handler.Warp as Warp
import Network.Wai.Middleware.Cors (simpleCors)
import Servant (Application, Handler, Server, hoistServer, serve, throwError)
import UnliftIO (UnliftIO (..))
import qualified UnliftIO as UIO

import Agora.Config
import Agora.Mode
import Agora.Util
import Agora.Web.API
import Agora.Web.Error
import Agora.Web.Handlers
import Agora.Web.Swagger

-- | Sets the given listen address in a Warp server settings.
addrSettings :: NetworkAddress -> Warp.Settings
addrSettings NetworkAddress {..} = Warp.defaultSettings
  & Warp.setHost (fromString $ toString naHost)
  & Warp.setPort (fromIntegral naPort)

-- | Helper for running a Warp server on a given listen port in
-- arbitrary @MonadIO@.
serveWeb :: MonadIO m => NetworkAddress -> Application -> m a
serveWeb addr app = do
  liftIO $ Warp.runSettings (addrSettings addr) app
  return $ error "Server terminated early"

-- | Makes the @Server@ for Agora API, given the natural
-- transformation from the working monad to @Handler@.
agoraServer
  :: forall m. AgoraWorkMode m
  => (forall a. m a -> Handler a)
  -> Server AgoraAPI
agoraServer nat = hoistServer agoraAPI nat agoraHandlers

-- | Natural transformation which uses @UnliftIO@ to convert
-- an arbitrary monadic handler which throws @AgoraAPIError@s
-- to a regular @Handler@.
convertAgoraHandler :: UnliftIO m -> m a -> Handler a
convertAgoraHandler (UnliftIO unlift) action =
  liftIO (unlift action)
  `catch` throwServant
  `catchAny` (throwServant . InternalError . show)
  where
    throwServant = throwError . toServantErr @AgoraAPIError

-- | Runs the web server which serves Agora API.
runAgora :: AgoraWorkMode m => m ()
runAgora = do
  cfg <- askConfig @AgoraConfig    -- TODO: find out how to get rid of this type app
  unlift <- UIO.askUnliftIO
  let listenAddr = cfg ^. option #listen_addr
      withDocs = cfg ^. option #serve_docs
      apiServer = agoraServer $ convertAgoraHandler unlift

  logInfo $ "Serving Agora API on "+|listenAddr|+""
  serveWeb listenAddr $ simpleCors $
    if withDocs
    then serve agoraAPIWithDocs $
         withSwaggerUI agoraAPI agoraApiSwagger apiServer
    else serve agoraAPI apiServer
