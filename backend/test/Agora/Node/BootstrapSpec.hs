module Agora.Node.BootstrapSpec
      ( spec
      ) where

import qualified Data.Map as M
import Data.Time.Clock (NominalDiffTime, addUTCTime)
import qualified Data.Vector as V
import Database.Beam.Backend (SqlSerial (..))
import Database.Beam.Query (all_, runSelectReturningList, select)
import Monad.Capabilities (CapImpl (..))
import Test.Hspec (Spec, describe, it, shouldBe)
import Test.QuickCheck (Gen, arbitrary, once, within)
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
    bc <- pick $ genBlockChain (fromIntegral onePeriod)
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

  let waitFor = 2000000
  it "Two blocks with identical proposal votes" $ \dbCap -> within waitFor $ once $ monadicIO $ do
    (voter, proposal, op1, op2) <- pick arbitrary
    let appendBlock' op bc = appendBlock bc Proposing (ProposalOp op voter 0 [proposal])
    newBc <- pick $ appendBlock' op1 genesisBlockChain >>= appendBlock' op2
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
    (voter, proposal, op2) <- pick arbitrary
    newBc <- pick $ do
      bc <- genBlockChain (fromIntegral onePeriod - 1)
      op1 <- arbitrary
      bc1 <- appendBlock bc Proposing (ProposalOp op1 voter 0 [proposal])
      let appendBlock' op b = appendBlock b Exploration (BallotOp op voter 1 proposal Yay)
      bc2 <- appendBlock' op2 bc1
      op3 <- arbitrary
      appendBlock' op3 bc2

    let clientWithVoters :: Monad m => TezosClient m
        clientWithVoters = (inmemoryClientRaw newBc)
          { _fetchVoters = \_ _ -> pure [Voter voter (fromIntegral @Int 10)]
          }
    cache <- lift $ UIO.newTVarIO (Nothing :: Maybe BlockHead)
    agoraPropertyM dbCap (CapImpl clientWithVoters, blockStackCapOverDbImpl cache) $ do
      lift $ bootstrap 1
      ballots <- lift $ runPg $ runSelectReturningList $ select (all_ $ asBallots agoraSchema)
      let expectedBallots = one $
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
      return $ ballots `shouldBe` expectedBallots

appendBlock
  :: BlockChain
  -> PeriodType
  -> Operation
  -> Gen BlockChain
appendBlock BlockChain{..} ptype op = do
  let level = fromIntegral (V.length bcBlocksList - 1)
  let lst = V.last bcBlocksList
  let metadata = BlockMetadata
        { bmLevel                = Level (level + 1)
        , bmCycle                = Cycle $ level `div` 4096
        , bmCyclePosition        = fromIntegral level `mod` 4096
        , bmVotingPeriod         = Id $ level `div` fromIntegral onePeriod
        , bmVotingPeriodPosition = fromIntegral level `mod` fromIntegral onePeriod
        , bmVotingPeriodType     = ptype
        }
  hash <- arbitrary
  let oneMinute = 60 :: NominalDiffTime
  let operations = Operations $ one op
  let block = Block
                { bHash = hash
                , bOperations = operations
                , bMetadata = metadata
                , bHeader = BlockHeader
                              (bHash lst)
                              (addUTCTime oneMinute $ bhrTimestamp $ bHeader lst)
                }

  pure $ BlockChain (M.insert (bHash block) block bcBlocks) (V.snoc bcBlocksList block)
