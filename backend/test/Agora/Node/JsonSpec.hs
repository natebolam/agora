module Agora.Node.JsonSpec
      ( spec
      ) where

import Data.Aeson (FromJSON, eitherDecode')
import qualified Data.ByteString.Lazy as BS
import Test.Hspec (Spec, describe, expectationFailure, it, shouldBe)

import Agora.Arbitrary ()
import Agora.Node.Types (AccountStatus (..), Block (..), BlockHead (..), BlockHeader (..),
                         BlockMetadata (..), Operation (..), Operations (..), ServiceInfo (..),
                         ServiceInfoList (..), parseUTCTime)
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
            , bmVotingPeriodType     = Exploration
            }
      let header = BlockHeader
                   { bhrPredecessor = encodeHash "BL99u1ubkghB6hL2Eoj1NigabCSEvh1PTv98M7jYhDwWRYK4UJW"
                   , bhrTimestamp = parseUTCTime "2019-03-26T21:17:14Z"
                   }
      testDecoding "resources/block369327.json" (Block hash header operations metadata)

  describe "Head decoding" $ do
    it "BlockHead (level=47163)" $ do
      testDecoding
        "resources/head47163.json" $ BlockHead
          { bhHash = encodeHash "BMdWRLqhLwiWLZeLcv9EzL7i2hkbZ1NAemUj31mDk5pAaF7E9og"
          , bhLevel = Level 47163
          , bhPredecessor = encodeHash "BLkSow2hYFuvG6MVvnwmo7iHe3wpCvVmWeGTBFB3USWg5x9Nbuv"
          }

  describe "Services decoding" $ do
    it "TzScan delegate services (top 10 items)" $ do
      testDecoding
        "resources/services_top10.json" $ ServiceInfoList
          [ ServiceAliases
            [ AccountStatus (encodeHash "tz3RDC3Jdn4j15J7bBHZd29EUee9gVB1CxD9") (Just "Foundation Baker 1")
            , AccountStatus (encodeHash "tz3bvNMQ95vfAYtG8193ymshqjSvmxiCUuR5") (Just "Foundation Baker 2")
            , AccountStatus (encodeHash "tz3RB4aoyjov4KEVRbuhvQ1CKJgBJMWhaeB8") (Just "Foundation Baker 3")
            , AccountStatus (encodeHash "tz3bTdwZinP8U1JmSweNzVKhmwafqWmFWRfk") (Just "Foundation Baker 4")
            , AccountStatus (encodeHash "tz3NExpXn9aPNZPorRE4SdjJ2RGrfbJgMAaV") (Just "Foundation Baker 5")
            , AccountStatus (encodeHash "tz3UoffC7FG7zfpmvmjUmUeAaHvzdcUvAj6r") (Just "Foundation Baker 6")
            , AccountStatus (encodeHash "tz3WMqdzXqRWXwyvj5Hp2H7QEepaUuS7vd9K") (Just "Foundation Baker 7")
            , AccountStatus (encodeHash "tz3VEZ4k6a4Wx42iyev6i2aVAptTRLEAivNN") (Just "Foundation Baker 8")
            ]
          , ServiceAliases
            [ AccountStatus (encodeHash "tz2KrmHRWu7b7Vr3GYQ3SJ41xaW64PiqWBYm") (Just "CF Baker")
            ]
          , RegularServiceInfo
            (encodeHash "tz1SohptP53wDPZhzTWzDUFAUcWF6DMBpaJV")
            "Hayek Lab"
            "hayeklab.png"
            []
          , RegularServiceInfo
            (encodeHash "tz1ZTG13gkvouxSANka3HG3uys8C5gu3DPXZ")
            "Just a Baker"
            "just-a-baker.png"
            []
          ]

testDecoding
  :: (Show a, Eq a, FromJSON a)
  => FilePath -> a -> IO ()
testDecoding path expected = do
  content <- BS.readFile path
  case eitherDecode' content of
    Left err  -> expectationFailure err
    Right res -> res `shouldBe` expected
