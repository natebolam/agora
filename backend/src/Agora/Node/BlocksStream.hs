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
    yieldUpTo adoptedLevel header = do
      let nextToAdopt = adoptedLevel + 1
      -- don't fetch head block because it may be rolled back
      if nextToAdopt >= bhLevel header then pure adoptedLevel
      else do
        blockToAdopt <- fetchBlock MainChain (LevelRef nextToAdopt)
        callback blockToAdopt
        yieldUpTo nextToAdopt header
