{-# OPTIONS_GHC -Wno-unused-imports #-}

module Agora.Node.WorkerSpec
      ( spec
      ) where

import Monad.Capabilities (CapImpl (..))
import qualified Servant.Client as C
import Test.Hspec (Spec, describe, it, shouldBe)
import Test.QuickCheck (once)
import Test.QuickCheck.Monadic (monadicIO, pick)
import qualified UnliftIO as UIO

import Agora.BlockStack
import Agora.DB
import Agora.Node.Client
import Agora.Node.Types
import Agora.Node.Worker

import Agora.Node.Blockchain
import Agora.TestMode

spec :: Spec
spec = withDbCapAll $ describe "Block sync worker" $ do
  it "Apply 2K blocks" $ \dbCap -> once $ monadicIO $ do
    bc <- pick $ genBlockChain 2000
    let hd = block2Head $ bcHead bc
    agoraPropertyM dbCap (inmemoryClient bc, blockStackCapOverDbImpl) $ do
      pushHeadWait hd
      adopted <- getAdoptedHead
      pure $ adopted `shouldBe` hd

  describe "Imitate failures in the worker" $ do
    it "Tezos node fails once" $ \dbCap -> once $ monadicIO $ do
      counter <- UIO.newTVarIO (0 :: Word32)
      let failingTezosClient = CapImpl $ fetcher1
              { _fetchBlock         = \chain bid -> do
                  runs <- UIO.readTVarIO counter
                  UIO.atomically $ UIO.writeTVar counter (runs + 1)
                  if runs == 0 then
                    UIO.throwIO $ TezosNodeError $ C.ConnectionError "Tezos node not run"
                  else _fetchBlock fetcher1 chain bid
              }
      agoraPropertyM dbCap (failingTezosClient, blockStackCapOverDbImpl) $ do
        pushHeadWait (block2Head block1)
        adopted <- getAdoptedHead
        pure $ adopted `shouldBe` block2Head block1

    it "The worker catches a synchronous exception" $ \dbCap -> once $ monadicIO $ do
      counter <- UIO.newTVarIO (0 :: Word32)
      let failOnBlock :: CapImpl BlockStack '[PostgresConn] IO
          failOnBlock = CapImpl $
            blockStackCapOverDb { _applyBlock = \block -> do
              runs <- UIO.readTVarIO counter
              UIO.atomically $ UIO.writeTVar counter (runs + 1)
              if runs == 0 then UIO.throwIO ApplyError
              else _applyBlock blockStackCapOverDb block
            }
      agoraPropertyM dbCap (CapImpl fetcher1, failOnBlock) $ do
        pushHeadWait (block2Head block1)
        adopted <- getAdoptedHead
        pure $ adopted `shouldBe` block2Head block1

data ApplyError = ApplyError
  deriving Show

instance Exception ApplyError
