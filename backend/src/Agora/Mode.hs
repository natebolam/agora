{-# LANGUAGE OverloadedLabels #-}

{-|
Definitions of monadic contstraints commonly used by the workers
and handlers.
-}
module Agora.Mode
       ( AgoraWorkMode
       , runAgoraReal
       ) where

import Loot.Config (option)
import Loot.Log (Logging, MonadLogging, NameSelector (..), withLogging)
import Monad.Capabilities (CapsT, emptyCaps)
import UnliftIO (MonadUnliftIO)

import Agora.Config

-- | Common set of constraints for Agora business logic.
type AgoraWorkMode m =
  ( MonadIO m
  , MonadUnliftIO m
  , MonadLogging m
  , MonadConfig AgoraConfig m
  )

-- | Runs an action which requires an @AgoraWorkMode@ in @CapsT@ over @IO@.
runAgoraReal
  :: AgoraConfigRec
  -> CapsT '[Logging, ConfigCap AgoraConfig] IO a
  -> IO a
runAgoraReal config action = usingReaderT emptyCaps $
  withConfig config $
  withLogging (config ^. option #logging) CallstackName action

