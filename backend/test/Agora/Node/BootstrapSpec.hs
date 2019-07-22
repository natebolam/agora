module Agora.Node.BootstrapSpec
      ( spec
      ) where

import Data.Time.Clock (addUTCTime)
import Database.Beam.Backend (SqlSerial (..))
import Database.Beam.Query (all_, runSelectReturningList, select)
import Monad.Capabilities (CapImpl (..))
import Test.Hspec (Spec, describe, it, shouldBe)
import Test.QuickCheck (arbitrary, once, within)
import Test.QuickCheck.Monadic (monadicIO, pick)
import qualified UnliftIO as UIO

import Agora.BlockStack
import Agora.DB hiding (Voter)
import Agora.Node hiding (block1, metadata1)
import Agora.Types

import Agora.Node.Blockchain
import Agora.TestMode

spec :: Spec
spec = withDbCapAll $ describe "Bootstrap" $ do
  it "Head corresponds to last block to a period" $ \dbCap -> once $ monadicIO $ do
    bc <- pick $ genEmptyBlockChain (fromIntegral onePeriod)
    let hd = block2Head $ bcHead bc
    cache <- lift $ UIO.newTVarIO (Nothing :: Maybe BlockHead)
    agoraPropertyM dbCap (inmemoryClient bc, blockStackCapOverDbImpl cache) $ do
      lift $ bootstrap 1
      adopted <- getAdoptedHead
      return $ adopted `shouldBe` hd

  it "Head has level 100K" $ \dbCap -> once $ monadicIO $ do
    bc <- pick $ genEmptyBlockChain 100000
    let hd = block2Head $ bcHead bc
    cache <- lift $ UIO.newTVarIO (Nothing :: Maybe BlockHead)
    agoraPropertyM dbCap (inmemoryClient bc, blockStackCapOverDbImpl cache) $ do
      lift $ bootstrap 3
      adopted <- getAdoptedHead
      return $ adopted `shouldBe` hd

  let waitFor = 4000000
  it "Two blocks with identical proposal votes" $ \dbCap -> within waitFor $ once $ monadicIO $ do
    (voter, proposal, op1, op2) <- pick arbitrary
    let appendProposing op bc = appendBlock Proposing (ProposalOp op voter 0 [proposal]) bc
    newBc <- pick $ appendProposing op1 genesisBlockChain >>= appendProposing op2
    let clientWithVoters :: Monad m => TezosClient m
        clientWithVoters = (inmemoryClientRaw newBc)
          { _fetchVoters = \_ _ -> pure [Voter voter (fromIntegral @Int 10)]
          }
    cache <- lift $ UIO.newTVarIO (Nothing :: Maybe BlockHead)
    agoraPropertyM dbCap (CapImpl clientWithVoters, blockStackCapOverDbImpl cache) $ do
      lift $ bootstrap 1 -- it's intentionally to check how system works if node is lagging
      periodVotes <- lift $ runPg $ runSelectReturningList $ select (all_ $ asProposalVotes agoraSchema)
      let expectedProposalVotes = one $
            ProposalVote
            { pvId        = SqlSerial 1
            , pvVoter     = VoterHash voter
            , pvProposal  = ProposalId 1
            , pvCastedRolls  = fromIntegral @Int 10
            , pvOperation = op1
            , pvVoteTime  = addUTCTime 60 (bhrTimestamp (bHeader genesisBlock))
            }
      return $ periodVotes `shouldBe` expectedProposalVotes

  it "Two blocks with identical ballots" $ \dbCap -> within waitFor $ once $ monadicIO $ do
    (voter, proposal, op1, op2, op3, op4, op5) <- pick arbitrary
    bc <- pick $ genBlockChainSkeleton [Proposing, Exploration, Promotion] (2 + 2 * fromIntegral onePeriod)
    let resBc =
          bc & modifyBlock 1 (Operations $ one $ ProposalOp op1 voter 0 [proposal])
             & modifyBlock (onePeriod + 1) (Operations [ BallotOp op2 voter 1 proposal Yay
                                                       , BallotOp op3 voter 1 proposal Yay
                                                       ]) -- it's a hack to avoid applying 64K empty blocks
                                                          -- what burden tests af. This hack has to be removed after AG-63 is done.
             & modifyBlock (2 * onePeriod + 1) (Operations [ BallotOp op4 voter 2 proposal Yay
                                                           , BallotOp op5 voter 2 proposal Yay
                                                           ])

    let clientWithVoters :: Monad m => TezosClient m
        clientWithVoters = (inmemoryClientRaw resBc)
          { _fetchVoters = \_ _ -> pure [Voter voter (fromIntegral @Int 10)]
          }
    cache <- lift $ UIO.newTVarIO (Nothing :: Maybe BlockHead)
    agoraPropertyM dbCap (CapImpl clientWithVoters, blockStackCapOverDbImpl cache) $ do
      lift $ bootstrap 3 -- it's a hack. Will be removed after AG-63 is done.
      ballots <- lift $ runPg $ runSelectReturningList $ select (all_ $ asBallots agoraSchema)
      let ballot1 =
            Ballot
            { bId         = SqlSerial 1
            , bVoteType   = ExplorationVote
            , bVoter      = VoterHash voter
            , bPeriod     = PeriodMetaId 1
            , bProposal   = ProposalId 2 -- it's because we perform all tests within one transaction
            , bCastedRolls = fromIntegral @Int 10
            , bOperation   = op2
            , bBallotTime  = addUTCTime ((fromIntegral onePeriod + 1) * 60) (bhrTimestamp (bHeader genesisBlock))
            , bBallotDecision = Yay
            }
      let ballot2 =
            Ballot
            { bId         = SqlSerial 2
            , bVoteType   = PromotionVote
            , bVoter      = VoterHash voter
            , bPeriod     = PeriodMetaId 2
            , bProposal   = ProposalId 2 -- it's because we perform all tests within one transaction
            , bCastedRolls = fromIntegral @Int 10
            , bOperation   = op4
            , bBallotTime  = addUTCTime ((2 * fromIntegral onePeriod + 1) * 60) (bhrTimestamp (bHeader genesisBlock))
            , bBallotDecision = Yay
            }
      return $ ballots `shouldBe` [ballot1, ballot2]
