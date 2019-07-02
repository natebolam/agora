{-|
Utilities for running Agora web server.
-}
module Agora.Web.Server
       ( runAgora
       , runAgoraWithDocs
       ) where

import Network.Wai.Handler.Warp (Port, run)
import Network.Wai.Middleware.Cors (simpleCors)
import Servant.Server (Handler, hoistServer, serve)

import Agora.Web.API
import Agora.Web.Handlers
import Agora.Web.Swagger

-- | Runs the web server which serves Agora API.
runAgora
  :: AgoraHandlersMode m
  => (forall a . m a -> Handler a)
  -> Port -> IO ()
runAgora hoist port = run port $ simpleCors $
  serve agoraAPI $ -- Basically it's genericServeT from servant-16
  hoistServer agoraAPI hoist agoraHandlers

-- | Runs the web server which serves Agora API and also
-- Swagger documentation for it.
runAgoraWithDocs
  :: AgoraHandlersMode m
  => (forall a . m a -> Handler a)
  -> Port
  -> IO ()
runAgoraWithDocs hoist port = run port $ simpleCors $
  serve agoraAPIWithDocs $
  withSwaggerUI agoraAPI agoraApiSwagger $
  hoistServer agoraAPI hoist agoraHandlers
