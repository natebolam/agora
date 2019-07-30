module Agora.Node.WorkerSpec
      ( spec
      ) where

import Monad.Capabilities (CapImpl (..))
import qualified Servant.Client as C
import Test.Hspec (Spec, describe, it, shouldBe)
import Test.QuickCheck (once, within)
import Test.QuickCheck.Monadic (monadicIO, pick)
import qualified UnliftIO as UIO

import Agora.BlockStack
import Agora.Node.Client
import Agora.Node.Types
import Agora.Node.Worker

import Agora.Node.Blockchain
import Agora.TestMode

spec :: Spec
spec = withDbCapAll $ describe "Block sync worker" $ do
  it "Apply 2K blocks" $ \dbCap -> once $ monadicIO $ do
    bc <- pick $ genEmptyBlockChain 2000
    let hd = block2Head $ bcHead bc
    blockStackImpl <- lift blockStackCapOverDbImplM
    agoraPropertyM dbCap (inmemoryClient bc, emptyDiscourseClient, blockStackImpl) $ do
      pushHeadWait hd
      adopted <- getAdoptedHead
      pure $ adopted `shouldBe` hd

  let waitFor = 6000000
  describe "Imitate failures in the worker" $ do
    it "Tezos node fails once" $ \dbCap -> within waitFor $ once $ monadicIO $ do
      counter <- UIO.newTVarIO (0 :: Word32)
      let failingTezosClient = CapImpl $ fetcher1
              { _fetchBlock         = \chain bid -> do
                  runs <- UIO.readTVarIO counter
                  UIO.atomically $ UIO.writeTVar counter (runs + 1)
                  if runs == 0 then
                    UIO.throwIO $ TezosNodeError $ C.ConnectionError "Tezos node not run"
                  else _fetchBlock fetcher1 chain bid
              }
      blockStackImpl <- lift blockStackCapOverDbImplM
      agoraPropertyM dbCap (failingTezosClient, emptyDiscourseClient, blockStackImpl) $ do
        pushHeadWait (block2Head block1)
        adopted <- getAdoptedHead
        pure $ adopted `shouldBe` block2Head block1

    it "The worker catches a synchronous exception" $ \dbCap -> within waitFor $ once $ monadicIO $ do
      counter <- UIO.newTVarIO (0 :: Word32)
      cache <- lift $ UIO.newTVarIO (Nothing :: Maybe BlockHead)
      let failOnBlock :: BlockStackCapImpl IO
          failOnBlock = CapImpl $
            (blockStackCapOverDb cache) { _applyBlock = \block -> do
              runs <- UIO.readTVarIO counter
              UIO.atomically $ UIO.writeTVar counter (runs + 1)
              if runs == 0 then UIO.throwIO ApplyError
              else _applyBlock (blockStackCapOverDb cache) block
            }
      agoraPropertyM dbCap (CapImpl fetcher1, emptyDiscourseClient, failOnBlock) $ do
        pushHeadWait (block2Head block1)
        adopted <- getAdoptedHead
        pure $ adopted `shouldBe` block2Head block1

data ApplyError = ApplyError
  deriving Show

instance Exception ApplyError
