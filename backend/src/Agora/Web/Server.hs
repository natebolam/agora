{-|
Utilities for running Agora web server.
-}
module Agora.Web.Server
       ( runAgora
       , runAgoraWithDocs
       ) where

import Network.Wai.Handler.Warp (Port, run)
import Servant.Server (serve)

import Agora.Web.API
import Agora.Web.Handlers
import Agora.Web.Swagger

-- | Runs the web server which serves Agora API.
runAgora :: Port -> IO ()
runAgora port = run port $
  serve agoraAPI agoraHandlers

-- | Runs the web server which serves Agora API and also
-- Swagger documentation for it.
runAgoraWithDocs :: Port -> IO ()
runAgoraWithDocs port = run port $
  serve agoraAPIWithDocs agoraServerWithDocs

