module Agora.Node.BlockListenerSpec
      ( spec
      ) where

import Monad.Capabilities (CapImpl (..))
import Test.Hspec (Spec, describe, it, shouldBe)
import Test.QuickCheck (once, within)
import Test.QuickCheck.Monadic (monadicIO, pick)
import qualified UnliftIO as UIO

import Agora.BlockStack
import Agora.Node.Types
import Agora.Node.Constants
import Agora.Node.BlockListener

import Agora.Node.Blockchain
import Agora.TestMode
import Agora.Node.API (neGetBlock)
import Control.Monad.Except (throwError)

spec :: Spec
spec = withDbResAll $ describe "Block listener" $ do
  let runWithin t = within (t * 1000000) . once . monadicIO
  it "Apply 2K blocks" $ \dbCap -> runWithin 2 $ do
    bc <- pick $ genEmptyBlockChain 2000
    let stable = block2Head $ bcStable bc
    blockStackImpl <- lift blockStackCapOverDbImplM
    discourseEndpoints <- lift inmemoryDiscourseEndpointsM
    withWaiApps (inmemoryConstantClientRaw bc) discourseEndpoints $ \wai -> 
      agoraPropertyM dbCap wai blockStackImpl $ do
        lift tezosBlockListener
        adopted <- getAdoptedHead
        pure $ adopted `shouldBe` stable

  it "Fork1 (without an intermediate valid block)" $ \dbCap -> runWithin 2 $ do
    bc <- pick $ genEmptyBlockChain 2
    let stable = bcStable bc
    blockStackImpl <- lift blockStackCapOverDbImplM
    discourseEndpoints <- lift inmemoryDiscourseEndpointsM
    (chan, tezosClient) <- lift inmemoryStreamingClient
    withWaiApps tezosClient discourseEndpoints $ \wai -> 
      agoraPropertyM dbCap wai blockStackImpl $ do
        lift $ UIO.withAsync tezosBlockListener $ \_ -> do
          waitThenEmitBlock block1 chan
          waitThenRewriteChain bc chan
          wait
        adopted <- getAdoptedHead
        pure $ adopted `shouldBe` block2HeadSafe stable

  it "Fork1 (with an intermediate valid block)" $ \dbCap -> runWithin 2 $ do
    bc <- pick $ genEmptyBlockChain 2
    let hd = bcHead bc
    let stable = bcStable bc
    blockStackImpl <- lift blockStackCapOverDbImplM
    discourseEndpoints <- lift inmemoryDiscourseEndpointsM
    (chan, tezosClient) <- lift inmemoryStreamingClient
    withWaiApps tezosClient discourseEndpoints $ \wai -> 
      agoraPropertyM dbCap wai blockStackImpl $ do
        lift $ UIO.withAsync tezosBlockListener $ \_ -> do
          waitThenEmitBlock block1 chan
          waitThenRewriteChain (takeBlocks 1 bc) chan
          waitThenEmitBlock hd chan
          wait
        adopted <- getAdoptedHead
        pure $ adopted `shouldBe` block2Head stable

  describe "Failures in the worker" $ do
    it "Tezos node fails once" $ \dbCap -> runWithin 6 $ do
      counter <- UIO.newTVarIO (0 :: Word32)
      let failingTezosClient = fetcher2
              { neGetBlock = \chain bid -> do
                  runs <- UIO.readTVarIO counter
                  UIO.atomically $ UIO.writeTVar counter (runs + 1)
                  if runs == 0 then
                    throwError $ notFound "Tezos node not run"
                  else neGetBlock fetcher2 chain bid
              }
      blockStackImpl <- lift blockStackCapOverDbImplM
      discourseEndpoints <- lift inmemoryDiscourseEndpointsM
      withWaiApps failingTezosClient discourseEndpoints $ \wai -> 
        agoraPropertyM dbCap wai blockStackImpl $ do
          lift tezosBlockListener
          adopted <- getAdoptedHead
          pure $ adopted `shouldBe` block2Head block1

    it "The worker catches a synchronous exception" $ \dbCap -> runWithin 6 $ do
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
      discourseEndpoints <- lift inmemoryDiscourseEndpointsM
      withWaiApps fetcher2 discourseEndpoints $ \wai -> 
        agoraPropertyM dbCap wai failOnBlock $ do
          lift tezosBlockListener
          adopted <- getAdoptedHead
          pure $ adopted `shouldBe` block2Head block1

  describe "Empty periods" $ do
    it "Head corresponds to a first block in a period" $ \dbCap -> runWithin 2 $ do
      let onePeriod = tzOnePeriod testTzConstants
      bc <- pick $ genEmptyBlockChain $ fromIntegral onePeriod
      let firstBlock = block2Head $ getBlock bc (LevelRef 1)
      blockStackImpl <- lift blockStackCapOverDbImplM
      discourseEndpoints <- lift inmemoryDiscourseEndpointsM
      withWaiApps (inmemoryConstantClientRaw bc) discourseEndpoints $ \wai -> 
        agoraPropertyM dbCap wai blockStackImpl $ overrideEmptyPeriods 1 $ do
          lift tezosBlockListener
          adopted <- getAdoptedHead
          return $ adopted `shouldBe` firstBlock

    it "Head corresponds to last block in a period" $ \dbCap -> runWithin 2 $ do
      let onePeriod = tzOnePeriod testTzConstants
      bc <- pick $ genEmptyBlockChain (1 + fromIntegral onePeriod)
      let hd = block2Head $ bcStable bc
      blockStackImpl <- lift blockStackCapOverDbImplM
      discourseEndpoints <- lift inmemoryDiscourseEndpointsM
      withWaiApps (inmemoryConstantClientRaw bc) discourseEndpoints $ \wai -> 
        agoraPropertyM dbCap wai blockStackImpl $ overrideEmptyPeriods 1 $ do
          lift tezosBlockListener
          adopted <- getAdoptedHead
          return $ adopted `shouldBe` hd

    it "Head has level corresponding to 3 periods plus something" $ \dbCap -> runWithin 2 $ do
      let onePeriod = tzOnePeriod testTzConstants
      bc <- pick $ genEmptyBlockChain (3 * fromIntegral onePeriod + 100)
      let stable = block2Head $ bcStable bc
      blockStackImpl <- lift blockStackCapOverDbImplM
      discourseEndpoints <- lift inmemoryDiscourseEndpointsM
      withWaiApps (inmemoryConstantClientRaw bc) discourseEndpoints $ \wai -> 
        agoraPropertyM dbCap wai blockStackImpl $ overrideEmptyPeriods 3 $ do
          lift tezosBlockListener
          adopted <- getAdoptedHead
          return $ adopted `shouldBe` stable

data ApplyError = ApplyError
  deriving Show

instance Exception ApplyError
