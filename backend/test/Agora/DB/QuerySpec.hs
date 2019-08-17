module Agora.DB.QuerySpec (spec) where

import Database.Beam.Backend.SQL.BeamExtensions (runInsertReturningList)
import Database.Beam.Query (all_, asc_, default_, insert, insertExpressions, insertValues, orderBy_,
                            select, val_)
import Monad.Capabilities (CapsT)
import Test.Hspec (Spec, describe, shouldBe, specify)
import Test.QuickCheck (Arbitrary, Property, Testable, arbitrary, resize)
import Test.QuickCheck.Monadic (monadicIO, pick)

import Agora.Arbitrary ()
import Agora.BlockStack
import Agora.DB
import Agora.Mode
import Agora.TestMode

schemaProperty
  :: (Testable prop, Show a, Arbitrary a)
  => (a -> CapsT AgoraCaps IO prop)
  -> DbCap
  -> Property
schemaProperty propAction dbCap = monadicIO $ do
  blockStackImpl <- lift blockStackCapOverDbImplM
  discourseEndpoints <- lift inmemoryDiscourseEndpointsM
  agoraPropertyM dbCap (emptyTezosClient, discourseEndpoints, blockStackImpl) $
    pick (resize 10 arbitrary) >>= lift . propAction

spec :: Spec
spec = withDbCapAll $ do
  describe "DB schema is valid" $ do
    let toMeta ((pmId, pmType, pmVotesCast, pmVotesAvailable, pmVotersNum,
                 pmTotalVotersNum, pmQuorum, pmWhenStarted, pmStartLevel),
                pmEndLevel, pmLastBlockLevel, pmLastBlockHash, pmPrevBlockHash,
                pmBallotsYay, pmBallotsNay, pmBallotsPass) =
          PeriodMeta {..}
        toVoter (h, r, mName, mLogo) = Voter h mName mLogo r
        toProposalExpr pMeta voter (pHash, pTimeProposed,
                                    (dTitle, dShortDesc, dLongDesc,
                                     dFile, dTopicId, dPostId)) = Proposal
          { prId = default_
          , prPeriod = val_ $ PeriodMetaId $ pmId pMeta
          , prHash = val_ pHash
          , prTimeProposed = val_ pTimeProposed
          , prProposer = val_ $ VoterHash $ voterPbkHash voter
          , prDiscourseTitle = val_ dTitle
          , prDiscourseShortDesc = val_ dShortDesc
          , prDiscourseLongDesc = val_ dLongDesc
          , prDiscourseFile = val_ dFile
          , prDiscourseTopicId = val_ dTopicId
          , prDiscoursePostId = val_ dPostId
          }
        toProposalVal pMeta voter pId (pHash, pTimeProposed,
                                       (dTitle, dShortDesc, dLongDesc,
                                        dFile, dTopicId, dPostId)) = Proposal
          { prId = pId
          , prPeriod = PeriodMetaId $ pmId pMeta
          , prHash = pHash
          , prTimeProposed = pTimeProposed
          , prProposer = VoterHash $ voterPbkHash voter
          , prDiscourseTitle = dTitle
          , prDiscourseShortDesc = dShortDesc
          , prDiscourseLongDesc = dLongDesc
          , prDiscourseFile = dFile
          , prDiscourseTopicId = dTopicId
          , prDiscoursePostId = dPostId
          }
        toProposalVoteExpr voter proposal (rolls, op, voteTime) = ProposalVote
          { pvId = default_
          , pvVoter = val_ $ VoterHash $ voterPbkHash voter
          , pvProposal = val_ $ ProposalId $ prId proposal
          , pvCastedRolls = val_ rolls
          , pvOperation = val_ op
          , pvVoteTime = val_ voteTime
          }
        toProposalVoteVal voter proposal pvId' (rolls, op, voteTime) = ProposalVote
          { pvId = pvId'
          , pvVoter = VoterHash $ voterPbkHash voter
          , pvProposal = ProposalId $ prId proposal
          , pvCastedRolls = rolls
          , pvOperation = op
          , pvVoteTime = voteTime
          }
        toBallotExpr voter period proposal (vType, rolls, op, bTime, decision) = Ballot
          { bId = default_
          , bVoteType = val_ vType
          , bVoter = val_ $ VoterHash $ voterPbkHash voter
          , bPeriod = val_ $ PeriodMetaId $ pmId period
          , bProposal = val_ $ ProposalId $ prId proposal
          , bCastedRolls = val_ rolls
          , bOperation = val_ op
          , bBallotTime = val_ bTime
          , bBallotDecision = val_ decision
          }
        toBallotVal voter period proposal bId' (vType, rolls, op, bTime, decision) = Ballot
          { bId = bId'
          , bVoteType = vType
          , bVoter = VoterHash $ voterPbkHash voter
          , bPeriod = PeriodMetaId $ pmId period
          , bProposal = ProposalId $ prId proposal
          , bCastedRolls = rolls
          , bOperation = op
          , bBallotTime = bTime
          , bBallotDecision = decision
          }

        AgoraSchema {..} = agoraSchema

    specify "Period metas" $ schemaProperty $ \metadatas -> do
      let pMetas = map toMeta metadatas
      runInsert' $ insert asPeriodMetas $ insertValues pMetas
      vals' <- runSelectReturningList' $ select $
        orderBy_ (asc_ . pmId) $ all_ asPeriodMetas
      return $ vals' `shouldBe` sortOn pmId pMetas

    specify "Voters" $ schemaProperty $ \voterHashesRolls -> do
      let voters = map toVoter voterHashesRolls
      runInsert' $ insert asVoters $ insertValues voters
      vals' <- runSelectReturningList' $ select $
        orderBy_ (asc_ . voterPbkHash) $ all_ asVoters
      return $ vals' `shouldBe` sortOn voterPbkHash voters

    specify "Proposals" $ schemaProperty $ \(pMetaData, voterData, propDatas) -> do
      let pMeta = toMeta pMetaData
          voter = toVoter voterData
          proposalVals = zipWith (toProposalVal pMeta voter) (repeat 0) propDatas
      runInsert' $ insert asPeriodMetas $ insertValues [pMeta]
      runInsert' $ insert asVoters $ insertValues [voter]
      runInsert' $ insert asProposals $ insertExpressions $
        map (toProposalExpr pMeta voter) propDatas
      proposals' <- fmap (map $ \pr -> pr { prId = 0 }) $
        runSelectReturningList' $ select $
        orderBy_ (asc_ . prId) $ all_ asProposals
      return $ proposals' `shouldBe` proposalVals

    specify "Proposal votes" $ schemaProperty $ \(pMetaData, voterData, propData, pVoteData) -> do
      let pMeta = toMeta pMetaData
          voter = toVoter voterData
      runInsert' $ insert asPeriodMetas $ insertValues [pMeta]
      runInsert' $ insert asVoters $ insertValues [voter]
      [proposal] <- runPg $ runInsertReturningList $ insert asProposals $
        insertExpressions [toProposalExpr pMeta voter propData]

      let proposalVote = toProposalVoteVal voter proposal 0 pVoteData
      runInsert' $ insert asProposalVotes $ insertExpressions
        [toProposalVoteExpr voter proposal pVoteData]
      [proposalVote'] <- fmap (map $ \pv -> pv { pvId = 0 }) $
        runSelectReturningList' $ select $ all_ asProposalVotes
      return $ proposalVote' `shouldBe` proposalVote

    specify "Ballots" $ schemaProperty $ \(pMetaData, voterData, propData, ballotData) -> do
      let pMeta = toMeta pMetaData
          voter = toVoter voterData
      runInsert' $ insert asPeriodMetas $ insertValues [pMeta]
      runInsert' $ insert asVoters $ insertValues [voter]
      [proposal] <- runPg $ runInsertReturningList $ insert asProposals $
        insertExpressions [toProposalExpr pMeta voter propData]

      let ballot = toBallotVal voter pMeta proposal 0 ballotData
      runInsert' $ insert asBallots $ insertExpressions
        [toBallotExpr voter pMeta proposal ballotData]
      [ballot'] <- fmap (map $ \b -> b { bId = 0 }) $
        runSelectReturningList' $ select $ all_ asBallots
      return $ ballot' `shouldBe` ballot
