module Agora.Web.Server where

import Network.Wai.Handler.Warp (Port, run)
import Servant.Server (serve)

import Agora.Web.API
import Agora.Web.Handlers

runAgora :: Port -> IO ()
runAgora port = run port $ serve agoraAPI agoraHandlers
