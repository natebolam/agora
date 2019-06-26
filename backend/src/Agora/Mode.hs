{-|
Definitions of monadic contstraints commonly used by the workers
and handlers.
-}
module Agora.Mode
       ( AgoraWorkMode
       ) where

import Loot.Log (MonadLogging)
import UnliftIO (MonadUnliftIO)

import Agora.Config (AgoraConfig, MonadConfig)

-- | Common set of constraints for Agora business logic.
type AgoraWorkMode m =
  ( MonadIO m
  , MonadUnliftIO m
  , MonadLogging m
  , MonadConfig AgoraConfig m
  )
