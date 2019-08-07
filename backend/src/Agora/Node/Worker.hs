{-# LANGUAGE TypeOperators #-}

module Agora.Node.Worker
       ( MonadSyncWorker (..)
       , SyncWorker
       , withSyncWorker
       , workerSyncCapImpl
       , worker
       , newTBChan
       ) where

import Control.Concurrent.STM.TBChan (TBChan, newTBChan, readTBChan, tryWriteTBChan, writeTBChan)
import Control.Monad.Reader (withReaderT)
import Fmt (Buildable (..), fmt, (+|), (|+))
import Loot.Log (Logging, MonadLogging, logDebug, logError, logWarning)
import Monad.Capabilities (CapImpl (..), CapsT, HasCap, HasNoCap, addCap, makeCap)
import UnliftIO (MonadUnliftIO)
import qualified UnliftIO as UIO

import Agora.BlockStack
import Agora.Node.Client
import Agora.Node.Types
import Agora.Types (Level, BlockHash)
import Agora.Util (supressException)

data SyncWorker m = SyncWorker
  { _pushHead     :: BlockHead -> m ()
  -- ^ Push a head to the worker and don't wait for a result
  -- Passed BlockHead can even be skipped if the worker queue is full.
  , _pushHeadWait :: BlockHead -> m ()
  -- ^ Push a head to the worker and wait when it is processed.
  }

makeCap ''SyncWorker

-- | Initialise worker in child thread.
-- This worker basically read requests on syncing up to a head hash
-- and then perform syncing, downloading blocks starting from last adopted + 1
-- to a previous block of the taken head.
withSyncWorker
  :: forall m caps a .
  ( MonadUnliftIO m
  , HasNoCap SyncWorker caps
  , HasCap Logging caps
  , MonadBlockStack (CapsT caps m)
  , HasCap TezosClient caps
  )
  => CapsT (SyncWorker ': caps) m a
  -> CapsT caps m a
withSyncWorker action = do
  -- should channel size be specified from outside?
  chan <- UIO.atomically $ newTBChan 10
  UIO.withAsync (worker chan) $ \_ ->
    withReaderT (addCap $ workerSyncCapImpl chan) action

workerSyncCapImpl :: MonadUnliftIO m => TBChan HeadRequest -> CapImpl SyncWorker '[Logging] m
workerSyncCapImpl chan = CapImpl $ SyncWorker
  { _pushHead = \bh -> do
      success <- UIO.atomically $ tryWriteTBChan chan $ NonBlockingRequest bh
      if success then
        logDebug $ bh |+ " is sucessfully added to the worker queue"
      else
        logWarning $ bh |+ " is NOT added to the worker queue"
  , _pushHeadWait = \bh -> do
      waitVar <- UIO.newEmptyMVar
      UIO.atomically $ writeTBChan chan $ BlockingRequest bh waitVar
      void $ UIO.takeMVar waitVar
  }

data SyncWorkerError
  = UnexpectedBlock
  { expected :: !BlockHash
  , actual   :: !BlockHash
  , level    :: !Level
  }
  | NotContinuation
  { adopted :: !BlockHead
  , next    :: !BlockHead
  } deriving (Show, Generic)

instance Buildable SyncWorkerError where
  build (UnexpectedBlock ex act lev) =
    "Divergence of block hashes for level " +| lev |+
    "expected hash: " +| ex |+
    ", but Tezos node returned: " +| act |+ ""
  build (NotContinuation adopted next) = "Adopted " +| adopted |+ " mismatches with " +| headWPred  next |+ ""

instance Exception SyncWorkerError where
  displayException = fmt . build

data HeadRequest
  = NonBlockingRequest BlockHead
  | BlockingRequest BlockHead (MVar BlockHead)

requiredHead :: HeadRequest -> BlockHead
requiredHead (NonBlockingRequest h) = h
requiredHead (BlockingRequest h _)  = h

worker
  :: forall m .
  ( MonadUnliftIO m
  , MonadBlockStack m
  , MonadLogging m
  , MonadTezosClient m
  )
  => TBChan HeadRequest
  -> m ()
worker chan = forever $ do
  el <- UIO.atomically $ readTBChan chan
  let retryIn = 5 -- retry in 5 seconds
  let retryInInt = fromIntegral retryIn :: Int
  supressException @SomeException
    retryIn
    (\e -> logError $ displayException e |+ " happened in the block sync worker. \
          \Retry with the same " +| requiredHead el |+ " in " +| retryInInt |+ " seconds. ")
    $
      supressException @TezosClientError
        retryIn
        (\e -> logWarning $ "Block sync worker experiences problem with Tezos node: " +| displayException e |+
                      ". Retry with the same " +| requiredHead el |+ " in " +| retryInInt |+ " seconds. ")
        (workerDo el)
  where
    fetchBlock' = fetchBlock MainChain . LevelRef

    workerDo el@(requiredHead -> pushedHead) = do
      adoptedHead <- getAdoptedHead
      if bhLevel adoptedHead >= bhLevel pushedHead then
        -- If it happens we treat this as a race condition between calls of @pushHead@,
        -- not invalid chain, because we don't have a proof that it's actually
        -- invalid chain.
        logWarning $ "Pushed "+| pushedHead |+ "is behind or \
        \at the same lavel comparing to adopted "+| adoptedHead |+""
      else do
        blockchainHead <- block2Head <$>
          if bhLevel adoptedHead + 1 == bhLevel pushedHead then
              fetchBlock' (bhLevel adoptedHead)
          else
              fetchApplyBlocks (bhLevel adoptedHead + 1) (bhLevel pushedHead - 1)

        unless (bhHash blockchainHead == bhPredecessor pushedHead) $
          UIO.throwIO $
            UnexpectedBlock
              (bhPredecessor pushedHead)
              (bhHash blockchainHead)
              (bhLevel blockchainHead)
      case el of
        BlockingRequest _ mvar -> void $ UIO.tryPutMVar mvar pushedHead
        _                      -> pass

    fetchApplyBlocks :: Level -> Level -> m Block
    fetchApplyBlocks !from !to = do
      nextBlock <- fetchBlock' from
      adoptedHead <- getAdoptedHead
      unless (bhrPredecessor (bHeader nextBlock) == bhHash adoptedHead
           && bmLevel (bMetadata nextBlock) == bhLevel adoptedHead + 1) $
        UIO.throwIO $ NotContinuation adoptedHead (block2Head nextBlock)
      applyBlock nextBlock
      if from == to then pure nextBlock
      else fetchApplyBlocks (from + 1) to
