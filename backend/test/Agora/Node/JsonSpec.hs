module Agora.Node.JsonSpec
      ( spec
      ) where

import Data.Aeson (FromJSON, eitherDecode')
import qualified Data.ByteString.Lazy as BS
import Test.Hspec (Spec, describe, expectationFailure, it, shouldBe)

import Agora.Arbitrary ()
import Agora.Node.Types (BakerInfo (..), BakerInfoList (..), Block (..), BlockHead (..),
                         BlockHeader (..), BlockMetadata (..), Operation (..), Operations (..),
                         parseUTCTime)
import Agora.Types

spec :: Spec
spec = do
  describe "Block decoding" $ do
    it "Block from Alphanet (level=439394)" $ do
      let hash = encodeHash "BLfcT1XX1bx1QiYSxfMjdSuWvfqemPMYvnEssXTXzcrYyQnQbHL"
      let operations = Operations []
      let metadata = BlockMetadata
            { bmLevel                = Level 439394
            , bmCycle                = Cycle 214
            , bmCyclePosition        = 1121
            , bmVotingPeriod         = Id 53
            , bmVotingPeriodPosition = 5217
            , bmVotingPeriodType     = Proposing
            }
      let predecessor = encodeHash "BLKsHXHjacbLYQFTZ1hgymZjkjefpTj5YgQUfW4HYFsDE5akn1V"
      let header = BlockHeader predecessor (parseUTCTime "2019-06-13T14:12:48Z")
      testDecoding "resources/block439394_testnet.json" (Block hash header operations metadata)

    it "Block (level=40000)" $ do
      let hash = encodeHash "BL1Rkb5wyp6CZ3bPvxEqh3Mn3XygYRHGjC87oWCra55L1fCCK7o"
      let operations = Operations []
      let metadata = BlockMetadata
            { bmLevel                = Level 40000
            , bmCycle                = Cycle 9
            , bmCyclePosition        = 3135
            , bmVotingPeriod         = Id 1
            , bmVotingPeriodPosition = 7231
            , bmVotingPeriodType     = Proposing
            }
      let header = BlockHeader (encodeHash "BKn7kAMadt6Whe18S47szn7FJuGCzsz2XYnycGxw3NwhcVWUSoY")
                             (parseUTCTime "2018-07-30T01:28:57Z")
      testDecoding "resources/block40000.json" (Block hash header operations metadata)

    it "Block (lev=338854) with a proposal vote" $ do
      let hash = encodeHash "BLkEXzZ7vM3iJDFimDtSQ1hZ8m1yTscKbhgKsNDf4i5fhjXaohV"
      let operations =
            Operations $ one $
              ProposalOp (encodeHash "onqsNkR7UdaMFRHfK24CradV2iPMRmCe5T31sLDQrW3qHrRGnxA")
                         (encodeHash "tz1X4Qb9NJ7JEuitX3jFpZptuAh7DUnCaLs1")
                         10
                         [encodeHash "Pt24m4xiPbLDhVgVfABUjirbmda3yohdN82Sp9FeuAXJ4eV9otd"]
      let metadata = BlockMetadata
            { bmLevel                = Level 338854
            , bmCycle                = Cycle 82
            , bmCyclePosition        = 2981
            , bmVotingPeriod         = Id 10
            , bmVotingPeriodPosition = 11173
            , bmVotingPeriodType     = Proposing
            }
      let header = BlockHeader
                   { bhrPredecessor = encodeHash "BLKerAqAt9F65ecLrjXnfSK1YGiaecVuuMaPjbcnyhLhTqSMvqZ"
                   , bhrTimestamp = parseUTCTime "2019-03-05T00:21:07Z"
                   }
      testDecoding "resources/block338854.json" (Block hash header operations metadata)

    it "Block (lev=369327) with a ballot" $ do
      let hash = encodeHash "BLXmbYmSSgxjHZvDqStXq4UW4p3eYgvoQ9qWNGZo37BrANAMz3G"
      let operations =
            Operations $ one $
              BallotOp
                (encodeHash "ont86fN1APukD23x3mfwarfn1qQyY49f7k1c8CuEtsbrTJo2ASy")
                (encodeHash "tz1Yju7jmmsaUiG9qQLoYv35v5pHgnWoLWbt")
                11
                (encodeHash "Pt24m4xiPbLDhVgVfABUjirbmda3yohdN82Sp9FeuAXJ4eV9otd")
                Yay
      let metadata = BlockMetadata
            { bmLevel                = Level 369327
            , bmCycle                = Cycle 90
            , bmCyclePosition        = 686
            , bmVotingPeriod         = Id 11
            , bmVotingPeriodPosition = 8878
            , bmVotingPeriodType     = Evaluation
            }
      let header = BlockHeader
                   { bhrPredecessor = encodeHash "BL99u1ubkghB6hL2Eoj1NigabCSEvh1PTv98M7jYhDwWRYK4UJW"
                   , bhrTimestamp = parseUTCTime "2019-03-26T21:17:14Z"
                   }
      testDecoding "resources/block369327.json" (Block hash header operations metadata)

  describe "Head decoding" $
    it "BlockHead (level=47163)" $
      testDecoding
        "resources/head47163.json" $ BlockHead
          { bhHash = encodeHash "BMdWRLqhLwiWLZeLcv9EzL7i2hkbZ1NAemUj31mDk5pAaF7E9og"
          , bhLevel = Level 47163
          , bhPredecessor = encodeHash "BLkSow2hYFuvG6MVvnwmo7iHe3wpCvVmWeGTBFB3USWg5x9Nbuv"
          }

  describe "Bakers list decoding" $ do
    it "MyTezosBaker bakers (top 10 items)" $ do
      testDecoding
        "resources/bakers_top10.json" $ BakerInfoList
          [ BakerInfo "Tezos Capital Legacy" (encodeHash "tz1TDSmoZXwVevLTEvKCTHWpomG76oC9S2fJ")
            (Just "https://mytezosbaker.com/uploads/TCap_Logo_transparent_700x700.png")
            (Just "https://mytezosbaker.com/tezoscapitallegacy/#voting")
          , BakerInfo "Crypto Delegate" (encodeHash "tz1Tnjaxk6tbAeC2TmMApPh8UsrEVQvhHvx5")
            (Just "https://mytezosbaker.com/uploads/crypto_delegate.png")
            (Just "https://mytezosbaker.com/cryptodelegate/#voting")
          , BakerInfo "Happy Tezos" (encodeHash "tz1WCd2jm4uSt4vntk4vSuUWoZQGhLcDuR9q")
            (Just "https://mytezosbaker.com/uploads/happytezos_logo_exported_1200.png")
            (Just "https://mytezosbaker.com/happytezos/#voting")
          , BakerInfo "At James" (encodeHash "tz3e75hU4EhDU3ukyJueh5v6UvEHzGwkg3yC")
            (Just "https://mytezosbaker.com/uploads/atjames.png")
            (Just "https://mytezosbaker.com/atjames/#voting")
          , BakerInfo "Flippin' tacos" (encodeHash "tz1TzaNn7wSQSP5gYPXCnNzBCpyMiidCq1PX")
            (Just "https://mytezosbaker.com/uploads/FlippinTacos_LOGO_RGB.png")
            (Just "https://mytezosbaker.com/flippintacos/#voting")
          , BakerInfo "P2P Validator" (encodeHash "tz1P2Po7YM526ughEsRbY4oR9zaUPDZjxFrb")
            (Just "https://mytezosbaker.com/uploads/p2p_Validator1.png")
            (Just "https://mytezosbaker.com/p2pvalidator/#voting")
          , BakerInfo "Bake'n'Rolls" (encodeHash "tz1NortRftucvAkD1J58L32EhSVrQEWJCEnB")
            (Just "https://mytezosbaker.com/uploads/bake-n-rolls.png")
            (Just "https://mytezosbaker.com/bakenrolls/#voting")
          , BakerInfo "Lucid Mining" (encodeHash "tz1VmiY38m3y95HqQLjMwqnMS7sdMfGomzKi")
            (Just "https://mytezosbaker.com/uploads/Lucid_Mining_-_new_bakery.png")
            (Just "https://mytezosbaker.com/lucidmining/#voting")
          , BakerInfo "XTZ Delegate" (encodeHash "tz1Xek93iSXXckyQ6aYLVS5Rr2tge2en7ZxS")
            (Just "https://mytezosbaker.com/uploads/xtz-delegate.png")
            (Just "https://mytezosbaker.com/xtzdelegate/#voting")
          , BakerInfo "TezosBC" (encodeHash "tz1c3Wh8gNMMsYwZd67JndQpYxdaaPUV27E7")
            (Just "https://mytezosbaker.com/uploads/tezosbc.png")
            (Just "https://mytezosbaker.com/tezosbc/#voting")
          ]

testDecoding
  :: (Show a, Eq a, FromJSON a)
  => FilePath -> a -> IO ()
testDecoding path expected = do
  content <- BS.readFile path
  case eitherDecode' content of
    Left err  -> expectationFailure err
    Right res -> res `shouldBe` expected
