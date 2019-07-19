module Agora.Node.JsonSpec
      ( spec
      ) where

import Data.Aeson (FromJSON, eitherDecode')
import qualified Data.ByteString.Lazy as BS
import Test.Hspec (Spec, describe, expectationFailure, it, shouldBe)

import Agora.Arbitrary ()
import Agora.Node.Types (Block (..), BlockHead (..), BlockMetadata (..), Operations (..))
import Agora.Types (Cycle (..), Hash (..), Level (..), Id (..), PeriodType (..))

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
            , bmVotingPeriod         = Id 53
            , bmVotingPeriodPosition = 5217
            , bmVotingPeriodType     = Proposing
            }
      let predecessor = Hash $ encodeUtf8 ("BLKsHXHjacbLYQFTZ1hgymZjkjefpTj5YgQUfW4HYFsDE5akn1V" :: Text)
      testDecoding "resources/block439394_testnet.json" (Block hash operations metadata predecessor)

    it "Block (level=40000)" $ do
      let hash = Hash $ encodeUtf8 ("BL1Rkb5wyp6CZ3bPvxEqh3Mn3XygYRHGjC87oWCra55L1fCCK7o" :: Text)
      let operations = Operations []
      let metadata = BlockMetadata
            { bmLevel                = Level 40000
            , bmCycle                = Cycle 9
            , bmCyclePosition        = 3135
            , bmVotingPeriod         = Id 1
            , bmVotingPeriodPosition = 7231
            , bmVotingPeriodType     = Proposing
            }
      let predecessor = Hash $ encodeUtf8 ("BKn7kAMadt6Whe18S47szn7FJuGCzsz2XYnycGxw3NwhcVWUSoY" :: Text)
      testDecoding "resources/block40000.json" (Block hash operations metadata predecessor)

  describe "Head decoding" $ do
    it "BlockHead (level=47163)" $ do
      testDecoding
        "resources/head47163.json" $ BlockHead
          { bhHash = Hash $ encodeUtf8 ("BMdWRLqhLwiWLZeLcv9EzL7i2hkbZ1NAemUj31mDk5pAaF7E9og" :: Text)
          , bhLevel = Level 47163
          , bhPredecessor = Hash $ encodeUtf8 ("BLkSow2hYFuvG6MVvnwmo7iHe3wpCvVmWeGTBFB3USWg5x9Nbuv" :: Text)
          }

testDecoding
  :: (Show a, Eq a, FromJSON a)
  => FilePath -> a -> IO ()
testDecoding path expected = do
  content <- BS.readFile path
  case eitherDecode' content of
    Left err  -> expectationFailure err
    Right res -> res `shouldBe` expected
