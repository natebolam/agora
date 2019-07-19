module Agora.Node.BootstrapSpec
      ( spec
      ) where

import Test.Hspec (Spec, describe, it, shouldBe)
import Test.QuickCheck (once)
import Test.QuickCheck.Monadic (monadicIO, pick)
import qualified UnliftIO as UIO

import Agora.Node

import Agora.BlockStack
import Agora.Node.Blockchain
import Agora.TestMode

spec :: Spec
spec = withDbCapAll $ describe "Bootstrap" $ do
  it "Head has level 2^15" $ \dbCap -> once $ monadicIO $ do
    bc <- pick $ genBlockChain 32768
    let hd = block2Head $ bcHead bc
    cache <- lift $ UIO.newTVarIO (Nothing :: Maybe BlockHead)
    agoraPropertyM dbCap (inmemoryClient bc, blockStackCapOverDbImpl cache) $ do
      lift $ bootstrap 1
      adopted <- getAdoptedHead
      return $ adopted `shouldBe` hd

  it "Head has level 100K" $ \dbCap -> once $ monadicIO $ do
    bc <- pick $ genBlockChain 100000
    let hd = block2Head $ bcHead bc
    cache <- lift $ UIO.newTVarIO (Nothing :: Maybe BlockHead)
    agoraPropertyM dbCap (inmemoryClient bc, blockStackCapOverDbImpl cache) $ do
      lift $ bootstrap 3
      adopted <- getAdoptedHead
      return $ adopted `shouldBe` hd
