{-# LANGUAGE OverloadedLabels #-}

{-|
Definitions of monadic contstraints commonly used by the workers
and handlers.
-}
module Agora.Mode
       ( AgoraWorkMode
       , AgoraCaps
       , runAgoraReal
       ) where

import Loot.Log (Logging, MonadLogging, NameSelector (..), withLogging)
import Monad.Capabilities (CapsT, emptyCaps)
import UnliftIO (MonadUnliftIO)

import Agora.Config
import Agora.DB
import Agora.Node
import Agora.BlockStack

-- | Common set of constraints for Agora business logic.
type AgoraWorkMode m =
  ( MonadIO m
  , MonadUnliftIO m
  , MonadLogging m
  , MonadConfig AgoraConfig m
  , MonadTezosClient m
  , MonadPostgresConn m
  , MonadBlockStack m
  , MonadSyncWorker m
  )

-- | List of capabilities required for `AgoraWorkMode`
type AgoraCaps = '[SyncWorker, BlockStack, PostgresConn, TezosClient, Logging, AgoraConfigCap]

-- | Runs an action which requires an @AgoraWorkMode@ in @CapsT@ over @IO@.
runAgoraReal
  :: AgoraConfigRec
  -> CapsT AgoraCaps IO a
  -> IO a
runAgoraReal config = usingReaderT emptyCaps
  . withConfig config
  . withLogging (config ^. option #logging) CallstackName
  . withTezosClient (config ^. option #node_addr)
  . withPostgresConn
  . withBlockStack
  . withSyncWorker
