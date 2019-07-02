module Main where

import Loot.Log (NameSelector (..), withLogging, basicConfig, logInfo)
import Monad.Capabilities (emptyCaps)
import Servant (runHandler)

import Agora.Web.Server (runAgoraWithDocs)

main :: IO ()
main =
  runner >>=
  either (const $ error "Impossible, all errors from server are caught by serve") pure
  where
    runner = runHandler $ usingReaderT emptyCaps $
      withLogging basicConfig CallstackName $ do
        logInfo "Serving Agora API on 8190"
        caps <- ask
        liftIO $ runAgoraWithDocs (usingReaderT caps) 8190
