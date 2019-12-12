module Agora.DB.QuerySpec (spec) where

import Database.Beam.Backend.SQL.BeamExtensions (runInsertReturningList)
import Database.Beam.Query (all_, asc_, default_, insert, insertExpressions, insertValues, orderBy_,
                            select, val_)
import Monad.Capabilities (CapsT)
import Test.Hspec (Spec, describe, shouldBe, specify)
import Test.QuickCheck (Arbitrary, Property, Testable, arbitrary, resize, withMaxSuccess)
import Test.QuickCheck.Monadic (monadicIO, pick)

import Agora.Arbitrary ()
import Agora.BlockStack
import Agora.DB
import Agora.Mode
import Agora.TestMode

schemaProperty
  :: (Testable prop, Show a, Arbitrary a)
  => (a -> CapsT AgoraCaps IO prop)
  -> DbRes
  -> Property
schemaProperty propAction dbRes = withMaxSuccess 20 $ monadicIO $ do
  blockStackImpl <- lift blockStackCapOverDbImplM
  discourseEndpoints <- lift inmemoryDiscourseEndpointsM
  withWaiApps emptyTezosClient discourseEndpoints $ \wai ->
    agoraPropertyM dbRes wai blockStackImpl $
      pick (resize 10 arbitrary) >>= lift . propAction

spec :: Spec
spec = withDbResAll $
  describe "DB schema is valid" $ do
    let toMeta ((pmId, pmType, pmVotesCast, pmVotesAvailable, pmVotersNum,
                 pmTotalVotersNum, pmQuorum, pmWhenStarted, pmStartLevel),
                pmEndLevel, pmLastBlockLevel, pmLastBlockHash, pmPrevBlockHash,
                pmBallotsYay, pmBallotsNay, pmBallotsPass) =
          PeriodMeta {..}
        toBlockMeta (blLevel, blHash, blPredecessor, blBlockTime, blVotingPeriodType) = BlockMeta{..}
        toVoter period (h, r, mName, mLogo, mProfile) = Voter h mName mLogo mProfile r (PeriodMetaId period)
        toProposalExpr pMeta voter (pHash, pTimeProposed, (pVotesCasted, pVotersNum),
                                    (dTitle, dShortDesc, dLongDesc,
                                     dFile, dTopicId, dPostId)) = Proposal
          { prId = default_
          , prPeriod = val_ $ PeriodMetaId $ pmId pMeta
          , prHash = val_ pHash
          , prTimeProposed = val_ pTimeProposed
          , prProposer = val_ $ VoterHash $ voterPbkHash voter
          , prVotesCast = val_ pVotesCasted
          , prVotersNum = val_ pVotersNum

          , prDiscourseTitle = val_ dTitle
          , prDiscourseShortDesc = val_ dShortDesc
          , prDiscourseLongDesc = val_ dLongDesc
          , prDiscourseFile = val_ dFile
          , prDiscourseTopicId = val_ dTopicId
          , prDiscoursePostId = val_ dPostId
          }
        toProposalVal pMeta voter pId (pHash, pTimeProposed, (pVotesCasted, pVotersNum),
                                       (dTitle, dShortDesc, dLongDesc,
                                        dFile, dTopicId, dPostId)) = Proposal
          { prId = pId
          , prPeriod = PeriodMetaId $ pmId pMeta
          , prHash = pHash
          , prTimeProposed = pTimeProposed
          , prProposer = VoterHash $ voterPbkHash voter
          , prVotesCast = pVotesCasted
          , prVotersNum = pVotersNum

          , prDiscourseTitle = dTitle
          , prDiscourseShortDesc = dShortDesc
          , prDiscourseLongDesc = dLongDesc
          , prDiscourseFile = dFile
          , prDiscourseTopicId = dTopicId
          , prDiscoursePostId = dPostId
          }
        toProposalVoteExpr voter proposal level (rolls, op, voteTime) = ProposalVote
          { pvId = default_
          , pvVoter = val_ $ VoterHash $ voterPbkHash voter
          , pvProposal = val_ $ ProposalId $ prId proposal
          , pvCastedRolls = val_ rolls
          , pvOperation = val_ op
          , pvVoteTime = val_ voteTime
          , pvBlock    = val_ $ BlockMetaId level
          }
        toProposalVoteVal voter proposal pvId' level (rolls, op, voteTime) = ProposalVote
          { pvId = pvId'
          , pvVoter = VoterHash $ voterPbkHash voter
          , pvProposal = ProposalId $ prId proposal
          , pvCastedRolls = rolls
          , pvOperation = op
          , pvVoteTime = voteTime
          , pvBlock    = BlockMetaId level
          }
        toBallotExpr voter period proposal level (vType, rolls, op, bTime, decision) = Ballot
          { bId = default_
          , bVoteType = val_ vType
          , bVoter = val_ $ VoterHash $ voterPbkHash voter
          , bPeriod = val_ $ PeriodMetaId $ pmId period
          , bProposal = val_ $ ProposalId $ prId proposal
          , bCastedRolls = val_ rolls
          , bOperation = val_ op
          , bBallotTime = val_ bTime
          , bBallotDecision = val_ decision
          , bBlock      = val_ $ BlockMetaId level
          }
        toBallotVal voter period proposal bId' level (vType, rolls, op, bTime, decision) = Ballot
          { bId = bId'
          , bVoteType = vType
          , bVoter = VoterHash $ voterPbkHash voter
          , bPeriod = PeriodMetaId $ pmId period
          , bProposal = ProposalId $ prId proposal
          , bCastedRolls = rolls
          , bOperation = op
          , bBallotTime = bTime
          , bBallotDecision = decision
          , bBlock     = BlockMetaId level
          }

        AgoraSchema {..} = agoraSchema

    specify "Period metas" $ schemaProperty $ \metadatas -> do
      let pMetas = map toMeta metadatas
      runInsert' $ insert asPeriodMetas $ insertValues pMetas
      vals' <- runSelectReturningList' $ select $
        orderBy_ (asc_ . pmId) $ all_ asPeriodMetas
      return $ vals' `shouldBe` sortOn pmId pMetas

    specify "Voters" $ schemaProperty $ \(pMetaData, voterHashesRolls) -> do
      let pMeta = toMeta pMetaData
      let voters = map (toVoter $ pmId pMeta) voterHashesRolls
      runInsert' $ insert asPeriodMetas $ insertValues [pMeta]
      runInsert' $ insert asVoters $ insertValues voters
      vals' <- runSelectReturningList' $ select $
        orderBy_ (asc_ . voterPbkHash) $ all_ asVoters
      return $ vals' `shouldBe` sortOn voterPbkHash voters

    specify "Proposals" $ schemaProperty $ \(pMetaData, voterData, propDatas) -> do
      let pMeta = toMeta pMetaData
          voter = toVoter (pmId pMeta) voterData
          proposalVals = zipWith (toProposalVal pMeta voter) (repeat 0) propDatas
      runInsert' $ insert asPeriodMetas $ insertValues [pMeta]
      runInsert' $ insert asVoters $ insertValues [voter]
      runInsert' $ insert asProposals $ insertExpressions $
        map (toProposalExpr pMeta voter) propDatas
      proposals' <- fmap (map $ \pr -> pr { prId = 0 }) $
        runSelectReturningList' $ select $
        orderBy_ (asc_ . prId) $ all_ asProposals
      return $ proposals' `shouldBe` proposalVals

    specify "Block metas" $ schemaProperty $ \metadatas -> do
      let bMetas = map toBlockMeta metadatas
      runInsert' $ insert asBlockMetas $ insertValues bMetas
      vals' <- runSelectReturningList' $ select $
        orderBy_ (asc_ . blLevel) $ all_ asBlockMetas
      return $ vals' `shouldBe` sortOn blLevel bMetas

    specify "Proposal votes" $ schemaProperty $ \(pMetaData, bMetaData, voterData, propData, pVoteData) -> do
      let pMeta = toMeta pMetaData
          bMeta = toBlockMeta bMetaData
          voter = toVoter (pmId pMeta) voterData
      runInsert' $ insert asPeriodMetas $ insertValues [pMeta]
      runInsert' $ insert asVoters $ insertValues [voter]
      [proposal] <- runPg $ runInsertReturningList $ insert asProposals $
        insertExpressions [toProposalExpr pMeta voter propData]
      runInsert' $ insert asBlockMetas $ insertValues [bMeta]
      runInsert' $ insert asProposalVotes $ insertExpressions
        [toProposalVoteExpr voter proposal (blLevel bMeta) pVoteData]

      let proposalVote = toProposalVoteVal voter proposal 0 (blLevel bMeta) pVoteData
      [proposalVote'] <- fmap (map $ \pv -> pv { pvId = 0 }) $
        runSelectReturningList' $ select $ all_ asProposalVotes
      return $ proposalVote' `shouldBe` proposalVote

    specify "Ballots" $ schemaProperty $ \(pMetaData, bMetaData, voterData, propData, ballotData) -> do
      let pMeta = toMeta pMetaData
          bMeta = toBlockMeta bMetaData
          voter = toVoter (pmId pMeta) voterData
      runInsert' $ insert asPeriodMetas $ insertValues [pMeta]
      runInsert' $ insert asVoters $ insertValues [voter]
      [proposal] <- runPg $ runInsertReturningList $ insert asProposals $
        insertExpressions [toProposalExpr pMeta voter propData]
      runInsert' $ insert asBlockMetas $ insertValues [bMeta]
      runInsert' $ insert asBallots $ insertExpressions
        [toBallotExpr voter pMeta proposal (blLevel bMeta) ballotData]

      let ballot = toBallotVal voter pMeta proposal 0 (blLevel bMeta) ballotData
      [ballot'] <- fmap (map $ \b -> b { bId = 0 }) $
        runSelectReturningList' $ select $ all_ asBallots
      return $ ballot' `shouldBe` ballot
