{-# LANGUAGE OverloadedLabels #-}

{-|
Definitions of monadic contstraints commonly used by the workers
and handlers.
-}
module Agora.Mode
       ( AgoraWorkMode
       , AgoraCaps
       , runAgoraReal
       , setEncoding
       ) where

import Loot.Log (Logging, MonadLogging, NameSelector (..), withLogging)
import Monad.Capabilities (CapsT, emptyCaps)
import System.IO (hSetEncoding, utf8)
import UnliftIO (MonadUnliftIO)

import Agora.BlockStack
import Agora.Config
import Agora.DB
import Agora.Node
import Agora.Discourse

-- | Common set of constraints for Agora business logic.
type AgoraWorkMode m =
  ( MonadIO m
  , MonadUnliftIO m
  , MonadTzConstants m
  , MonadLogging m
  , MonadConfig AgoraConfig m
  , MonadTezosClient m
  , MonadPostgresConn m
  , MonadBlockStack m
  , MonadSyncWorker m
  )

-- | List of capabilities required for `AgoraWorkMode`
type AgoraCaps = '[
    SyncWorker
  , BlockStack
  , DiscourseClient
  , PostgresConn
  , TezosClient
  , Logging
  , AgoraConfigCap
  , TzConstantsCap
  ]

setEncoding :: IO ()
setEncoding = do
  hSetEncoding stdout utf8
  hSetEncoding stderr utf8

-- | Runs an action which requires an @AgoraWorkMode@ in @CapsT@ over @IO@.
runAgoraReal
  :: AgoraConfigRec
  -> CapsT AgoraCaps IO a
  -> IO a
runAgoraReal config action = do
  setEncoding
  usingReaderT emptyCaps
    . withRealTzConstants
    . withConfig config
    . withLogging (config ^. option #logging) CallstackName
    . withTezosClient
    . withPostgresConn
    . withDiscourseClient
    . withBlockStack
    . withSyncWorker
    $ action
