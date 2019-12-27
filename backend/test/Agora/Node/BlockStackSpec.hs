module Agora.Node.BlockStackSpec
      ( spec
      ) where

import Data.Time.Clock (addUTCTime)
import Database.Beam.Backend (SqlSerial (..))
import Database.Beam.Query (all_, runSelectReturningList, select)
import Test.Hspec (Spec, describe, it, shouldBe)
import Test.QuickCheck (arbitrary, once, within)
import Test.QuickCheck.Monadic (monadicIO, pick)

import Agora.BlockStack
import Agora.DB hiding (Voter)
import Agora.Node hiding (block1, metadata1)
import Agora.Types

import Agora.Node.Blockchain
import Agora.TestMode

spec :: Spec
spec = withDbResAll $ describe "BlockStack" $ do
  let waitFor = 4000000
  it "Two blocks with identical proposal votes" $ \dbRes -> within waitFor $ once $ monadicIO $ do
    (voter, proposal, op1, op2) <- pick arbitrary
    let appendProposing op = appendGenBlock Proposing (ProposalOp op voter 0 [proposal])
    newBc <- pick $ appendProposing op1 genesisBlockChain >>= appendProposing op2
    let clientWithVoters = (inmemoryConstantClientRaw newBc)
          { neGetVoters = \_ _ -> pure [Voter voter (fromIntegral @Int 10)]
          }
    blockStackImpl <- lift blockStackCapOverDbImplM
    discourseEndpoints <- lift inmemoryDiscourseEndpointsM
    withWaiApps clientWithVoters discourseEndpoints $ \wai ->
      agoraPropertyM dbRes wai blockStackImpl $
        overrideEmptyPeriods 1 $ do -- it's intentionally equals to 1 to check how system works if node is lagging
          lift tezosBlockListener
          periodVotes <- lift $ runPg $ runSelectReturningList $ select (all_ $ asProposalVotes agoraSchema)
          let expectedProposalVotes = one $
                ProposalVote
                { pvId        = SqlSerial 1
                , pvVoter     = VoterHash voter
                , pvProposal  = ProposalId 1
                , pvCastedRolls  = fromIntegral @Int 10
                , pvOperation = op1
                , pvVoteTime  = addUTCTime 60 (blockTimestamp genesisBlock)
                , pvBlock = BlockMetaId 1
                }
          return $ periodVotes `shouldBe` expectedProposalVotes

  it "Two blocks with identical ballots" $ \dbRes -> within waitFor $ once $ monadicIO $ do
    let onePeriod = tzOnePeriod testTzConstants
    (voter, proposal, op1, op2, op3, op4, op5) <- pick arbitrary
    bc <- pick $ genBlockChainSkeleton [Proposing, Evaluation, Voting] (3 + 2 * fromIntegral onePeriod)
    let resBc =
          bc & modifyBlock 1 (Operations $ one $ ProposalOp op1 voter 0 [proposal])
             & modifyBlock (onePeriod + 1) (Operations $ one $ BallotOp op2 voter 1 proposal Yay)
             & modifyBlock (onePeriod + 2) (Operations $ one $ BallotOp op3 voter 1 proposal Yay)
             & modifyBlock (2 * onePeriod + 1) (Operations $ one $ BallotOp op4 voter 2 proposal Yay)
             & modifyBlock (2 * onePeriod + 2) (Operations $ one $ BallotOp op5 voter 2 proposal Yay)

    let clientWithVoters = (inmemoryConstantClientRaw resBc)
          { neGetVoters = \_ _ -> pure [Voter voter (fromIntegral @Int 10)]
          }
    blockStackImpl <- lift blockStackCapOverDbImplM
    discourseEndpoints <- lift inmemoryDiscourseEndpointsM
    withWaiApps clientWithVoters discourseEndpoints $ \wai ->
      agoraPropertyM dbRes wai blockStackImpl $
        overrideEmptyPeriods 1 $ do
          lift tezosBlockListener
          ballots <- lift $ runPg $ runSelectReturningList $ select (all_ $ asBallots agoraSchema)
          let ballot1 =
                Ballot
                { bId         = SqlSerial 1
                , bVoteType   = EvaluationVote
                , bVoter      = VoterHash voter
                , bPeriod     = PeriodMetaId 1
                , bProposal   = ProposalId 1
                , bCastedRolls = fromIntegral @Int 10
                , bOperation   = op2
                , bBallotTime  = addUTCTime ((fromIntegral onePeriod + 1) * 60) (blockTimestamp genesisBlock)
                , bBallotDecision = Yay
                , bBlock       = BlockMetaId $ onePeriod + 1
                }
          let ballot2 =
                Ballot
                { bId         = SqlSerial 2
                , bVoteType   = VotingVote
                , bVoter      = VoterHash voter
                , bPeriod     = PeriodMetaId 2
                , bProposal   = ProposalId 1
                , bCastedRolls = fromIntegral @Int 10
                , bOperation   = op4
                , bBallotTime  = addUTCTime ((2 * fromIntegral onePeriod + 1) * 60) (blockTimestamp genesisBlock)
                , bBallotDecision = Yay
                , bBlock       = BlockMetaId $ 2 * onePeriod + 1
                }
          return $ ballots `shouldBe` [ballot1, ballot2]
