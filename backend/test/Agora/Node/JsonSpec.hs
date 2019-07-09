module Agora.Node.JsonSpec
      ( spec
      ) where

import Data.Aeson (FromJSON, eitherDecode')
import qualified Data.ByteString.Lazy as BS
import Test.Hspec (Spec, describe, expectationFailure, it, shouldBe)

import Agora.Arbitrary ()
import Agora.Node.Types (Block (..), BlockHead (..), BlockMetadata (..), Operations (..))
import Agora.Types (Cycle (..), Hash (..), Level (..), PeriodNum (..), PeriodType (..))

spec :: Spec
spec = do
  describe "Block decoding" $ do
    it "Block from Alphanet (level=439394)" $ do
      let hash = Hash $ encodeUtf8 ("BLfcT1XX1bx1QiYSxfMjdSuWvfqemPMYvnEssXTXzcrYyQnQbHL" :: Text)
      let operations = Operations []
      let metadata = BlockMetadata
            { bmLevel                = Level 439394
            , bmCycle                = Cycle 214
            , bmCyclePosition        = 1121
            , bmVotingPeriod         = PeriodNum 53
            , bmVotingPeriodPosition = 5217
            , bmVotingPeriodType     = Proposing
            }
      testDecoding "resources/block439394_testnet.json" (Block hash operations metadata)

    it "Block (level=40000)" $ do
      let hash = Hash $ encodeUtf8 ("BL1Rkb5wyp6CZ3bPvxEqh3Mn3XygYRHGjC87oWCra55L1fCCK7o" :: Text)
      let operations = Operations []
      let metadata = BlockMetadata
            { bmLevel                = Level 40000
            , bmCycle                = Cycle 9
            , bmCyclePosition        = 3135
            , bmVotingPeriod         = PeriodNum 1
            , bmVotingPeriodPosition = 7231
            , bmVotingPeriodType     = Proposing
            -- ^ Node returned Proposing for Cycle 9 ¯\_(ツ)_/¯
            }
      testDecoding "resources/block40000.json" (Block hash operations metadata)

  describe "Head decoding" $ do
    it "BlockHead (level=47163)" $ do
      testDecoding
        "resources/head47163.json"
        (BlockHead $ Hash $ encodeUtf8 ("BMdWRLqhLwiWLZeLcv9EzL7i2hkbZ1NAemUj31mDk5pAaF7E9og" :: Text))

testDecoding
  :: (Show a, Eq a, FromJSON a)
  => FilePath -> a -> IO ()
testDecoding path expected = do
  content <- BS.readFile path
  case eitherDecode' content of
    Left err  -> expectationFailure err
    Right res -> res `shouldBe` expected
