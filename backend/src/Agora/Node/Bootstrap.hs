{-# LANGUAGE TypeOperators #-}

module Agora.Node.Bootstrap
       ( bootstrap
       ) where

import Fmt ((+|), (|+))
import Loot.Log (MonadLogging, logDebug, logError, logInfo, logWarning)
import UnliftIO (MonadUnliftIO)
import qualified UnliftIO as UIO

import Agora.BlockStack
import Agora.Node.Client
import Agora.Node.Constants
import Agora.Node.Types
import Agora.Node.Worker
import Agora.Types (Level (..))
import Agora.Util (suppressException)

data BootstrapError
  = OnlyArchiveSupported
  deriving (Show, Generic)

instance Exception BootstrapError

bootstrap
  :: forall m .
  ( MonadUnliftIO m
  , MonadTezosClient m
  , MonadBlockStack m
  , MonadSyncWorker m
  , MonadLogging m
  , MonadTzConstants m
  )
  => m ()
bootstrap = do
  logInfo "Bootstrapping started"
  Checkpoint{..} <- fetchCheckpoint MainChain
  if cHistoryMode /= "archive" then do
    logError "Only archive history mode is supported. Bootstrapping is interrupted."
    UIO.throwIO OnlyArchiveSupported
  else do
    let retryIn = 5 -- retry in 5 seconds
    let retryInInt = fromIntegral retryIn :: Int
    finallyAdopted <- suppressException @SomeException
      retryIn
      (\e -> logError $ displayException e |+ " happened during bootstrap. Retry in " +| retryInInt |+ " seconds.")
      $
        suppressException @TezosClientError
          retryIn
          (\e -> logWarning $ "During bootstrapping the problem with Tezos node happened: " +| displayException e |+
                             ". Retry in " +| retryInInt |+ " seconds. ")
          bootstrapDo
    logInfo $ "Boostrapping is finished, adopted head: " +| finallyAdopted |+ ""
  where
    bootstrapDo = do
      onePeriod <- askOnePeriod
      emptyPeriods <- tzEmptyPeriods <$> askTzConstants
      adopted <- getAdoptedHead
      nodeHead <- fetchBlockHead MainChain HeadRef
      -- a start level is the first level of a next period
      let startLevel =
            if bhLevel adopted == 0 then Level 1
            else (((bhLevel adopted - 1) `div` onePeriod) + 1) * onePeriod + 1
      -- an end level is min from a level finishing first @emptyPeriods@ and
      -- level known by a node
      let endLevel = min (fromIntegral emptyPeriods * onePeriod) (bhLevel nodeHead)
      when (bhLevel adopted < endLevel) $ do
        logInfo $ "Trying to skip empty periods"
        logDebug $
          "Current adopted level: " +| bhLevel adopted |+
          ", starting level: " +| startLevel |+
          ", target level: " +| endLevel |+ ""
        periodsSkipped <- skipPeriods onePeriod startLevel endLevel
        logInfo $ "" +| periodsSkipped |+ " periods are skipped"

      syncUpHead

    skipPeriods :: Level -> Level -> Level -> m Int
    skipPeriods onePeriod !from !to
      | from <= to = do
          block <- fetchBlock MainChain (LevelRef from)
          applyBlock block
          (+1) <$> skipPeriods onePeriod (from + onePeriod) to
      | otherwise = do
          block <- fetchBlock MainChain (LevelRef to)
          0 <$ applyBlock block

    syncUpHead = do
      nodeHead <- fetchBlockHead MainChain HeadRef
      adopted <- getAdoptedHead
      -- TODO consider '>=' case carefully
      if bhLevel adopted + 1 < bhLevel nodeHead then do
        logDebug $ "Adopted head: " +| adopted |+ ", target head: " +| nodeHead |+ ""
        pushHeadWait nodeHead
        syncUpHead
      else pure adopted
