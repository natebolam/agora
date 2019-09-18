{-# LANGUAGE TypeOperators #-}

module Agora.Node.BlockListener
       ( tezosBlockListener

       , blockMetasFiller
       ) where

import Fmt (Buildable (..), fmt, (+|), (|+))
import Loot.Log (MonadLogging, logError, logWarning, logDebug)
import UnliftIO (MonadUnliftIO)
import qualified UnliftIO as UIO

import Database.Beam.Query (select, aggregate_, max_, all_)
import Agora.DB.Connection
import Agora.DB.Schema

import Agora.BlockStack
import Agora.Node.Client
import Agora.Node.Types
import Agora.Node.Constants
import Agora.Node.BlocksStream
import Agora.Util (suppressException)

tezosBlockListener
  :: forall m .
  ( MonadUnliftIO m
  , MonadBlockStack m
  , MonadLogging m
  , MonadTezosClient m
  , MonadTzConstants m
  )
  => m ()
tezosBlockListener = tezosBlockListenerTemplate $ do
  initAdopted <- getAdoptedHead
  withTezosNodeBlocksStream (bhLevel initAdopted) $ \newBlock -> do
    adoptedHead <- getAdoptedHead
    -- if level of an adopted head + 1 equals to a level of a new block
    -- then their hashes have to be matched
    unless (bhrPredecessor (bHeader newBlock) == bhHash adoptedHead
            || not (bmLevel (bMetadata newBlock) == bhLevel adoptedHead + 1)) $
      UIO.throwIO $ NotContinuation adoptedHead (block2Head newBlock)
    applyBlock newBlock

blockMetasFiller
  :: forall m .
  ( MonadUnliftIO m
  , MonadBlockStack m
  , MonadLogging m
  , MonadTezosClient m
  , MonadTzConstants m
  , MonadPostgresConn m
  )
  => m ()
blockMetasFiller = tezosBlockListenerTemplate $ do
  bmHeadLevel <- fromMaybe 0 <$> getBlockMetasAdoptedLevel
  withTezosNodeBlocksStream bmHeadLevel $ \newBlock ->
    if not (null $ unOperations $ bOperations newBlock) then do
      transact (insertBlockMeta newBlock)
      logDebug $ "" +| block2Head newBlock |+ " has been added to the block_metas table"
    else
      logDebug $ "" +| block2Head newBlock |+ " has been skipped"
  where
    getBlockMetasAdoptedLevel = fmap join $ runSelectReturningOne' $ select $
      aggregate_ (max_ . blLevel) (all_ $ asBlockMetas agoraSchema)

tezosBlockListenerTemplate
  :: forall m . (MonadUnliftIO m, MonadLogging m)
  => m ()
  -> m ()
tezosBlockListenerTemplate listenerDo = do
  let retryIn = 5 -- retry in 5 seconds
  let retryInInt = fromIntegral retryIn :: Int
  suppressException @SomeException
    retryIn
    (\e -> logError $ displayException e |+ " happened in the block sync worker. \
          \Retry with the same level in " +| retryInInt |+ " seconds. ")
    $
      suppressException @TezosClientError
        retryIn
        (\e -> logWarning $ "Block sync worker experiences problem with Tezos node: " +| displayException e |+
                      ". Retry with the same level in " +| retryInInt |+ " seconds. ")
        listenerDo

data SyncWorkerError
  = NotContinuation
  { adopted :: !BlockHead
  , next    :: !BlockHead
  } deriving (Show, Generic)

instance Buildable SyncWorkerError where
  build (NotContinuation adopted next) = "Adopted " +| adopted |+ " mismatches with " +| headWPred  next |+ ""

instance Exception SyncWorkerError where
  displayException = fmt . build
