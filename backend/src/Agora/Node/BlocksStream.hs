module Agora.Node.BlocksStream
    ( withTezosNodeBlocksStream
    ) where

import qualified UnliftIO.IORef as UIO
import UnliftIO (MonadUnliftIO)

import Agora.Types (Level (..))
import Agora.Node.Types
import Agora.Node.Constants
import Agora.Node.Client

withTezosNodeBlocksStream
  :: ( MonadTezosClient m
     , MonadUnliftIO m
     , MonadTzConstants m
     )
  => Level
  -> (Block -> m ())
  -> m ()
withTezosNodeBlocksStream adopted callback = do
  nodeHead <- fetchBlockHead MainChain HeadRef
  adoptedVar <- UIO.newIORef =<< yieldUpTo adopted nodeHead
  headsStream MainChain $ \bh -> do
    curAdopted <- UIO.readIORef adoptedVar
    UIO.writeIORef adoptedVar =<< yieldUpTo curAdopted bh
  where
    nextLevel x = do
      isEnd <- isPeriodEnd x
      if isEnd then pure $ x + 1
      else do
        emptyPeriods <- tzEmptyPeriods <$> askTzConstants
        onePeriod <- askOnePeriod
        let skipBlocks = fromIntegral emptyPeriods * onePeriod
        if x < skipBlocks then pure $ ((x `div` onePeriod) + 1) * onePeriod
        else pure $ x + 1

    yieldUpTo adoptedLevel header = do
      nextToAdopt <- nextLevel adoptedLevel
      -- don't fetch head block because it may be rolled back
      if nextToAdopt >= bhLevel header then pure adoptedLevel
      else do
        blockToAdopt <- fetchBlock MainChain (LevelRef nextToAdopt)
        callback blockToAdopt
        yieldUpTo nextToAdopt header
