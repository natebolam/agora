{- Module which is responsibe for application block to db. -}

{-# LANGUAGE TypeOperators #-}

module Agora.BlockStack
       ( MonadBlockStack (..)
       , BlockStack (..)
       , withBlockStack
       , blockStackCapOverDb
       , blockStackCapOverDbImpl
       , BlockStackCapImpl
       ) where

import Control.Monad.Reader (withReaderT)
import qualified Data.Set as S
import Database.Beam.Backend.SQL.BeamExtensions (runUpdateReturningList)
import Database.Beam.Query (all_, countAll_, current_, default_, filter_, guard_, in_, insert,
                            insertExpressions, insertValues, select, update, val_, (&&.), (<-.),
                            (==.))
import qualified Database.Beam.Query as B
import Fmt (blockListF, listF, mapF, (+|), (|+))
import Loot.Log (Logging, MonadLogging, logDebug, logInfo)
import Monad.Capabilities (CapImpl (..), CapsT, HasCap, HasNoCap, addCap, makeCap)
import UnliftIO (MonadUnliftIO)
import qualified UnliftIO as UIO

import Agora.DB
import qualified Agora.DB as DB
import Agora.Node.Client
import Agora.Node.Types
import Agora.Types

data BlockStack m = BlockStack
  { _getAdoptedHead :: m BlockHead
  , _applyBlock     :: Block -> m ()
  }

makeCap ''BlockStack

withBlockStack
  :: forall m caps a .
  ( HasCap PostgresConn caps
  , HasCap TezosClient caps
  , HasCap Logging caps
  , HasNoCap BlockStack caps
  , MonadUnliftIO m
  )
  => CapsT (BlockStack ': caps) m a
  -> CapsT caps m a
withBlockStack action = do
  tvar <- UIO.newTVarIO Nothing
  withReaderT (addCap $ blockStackCapOverDbImpl tvar) action

type BlockStackCapImpl m = CapImpl BlockStack '[PostgresConn, TezosClient, Logging] m

blockStackCapOverDbImpl
  :: MonadUnliftIO m
  => TVar (Maybe BlockHead)
  -> BlockStackCapImpl m
blockStackCapOverDbImpl cache = CapImpl $ blockStackCapOverDb cache

blockStackCapOverDb
  :: ( MonadUnliftIO m
     , MonadPostgresConn m
     , MonadTezosClient m
     , MonadLogging m
     )
  => TVar (Maybe BlockHead)
  -> BlockStack m
blockStackCapOverDb cache = BlockStack
  { _getAdoptedHead = readAdoptedHead cache
  , _applyBlock = onBlock cache
  }

-- | Errors which can be raised during block application
data ApplyBlockError
  = VoterNotExist PublicKeyHash
  | ProposalNotExist ProposalHash
  deriving (Generic, Show, Eq)

instance Exception ApplyBlockError

readAdoptedHead ::
  ( MonadPostgresConn m
  , MonadUnliftIO m
  )
  => TVar (Maybe BlockHead) -> m BlockHead
readAdoptedHead cache = do
  mb <- UIO.readTVarIO cache
  case mb of
    Just x -> pure x
    Nothing -> do
      bhMb <- runSelectReturningOne' $ B.select $ do
        mx <- B.aggregate_ (B.max_ . pmId) (B.all_ $ asPeriodMetas agoraSchema)
        x <- B.all_ (asPeriodMetas agoraSchema)
        B.guard_ (mx ==. B.just_ (pmId x))
        pure (pmLastBlockHash x, pmLastBlockLevel x, pmPrevBlockHash x)
      let ret = case bhMb of
                  Nothing        -> genesisBlockHead
                  Just (h, l, p) -> BlockHead h l p
      UIO.atomically $ UIO.writeTVar cache (Just ret)
      pure ret

-- | Analise the passed block and update corresponding tables.
-- If the passed block is the first in the period then:
-- * Current quorum is updated using /votes/current_quorum
-- * Voters' rolls are updated using /votes/listings
-- For any block:
-- * If block period is Proposal then new proposal are added
-- * New votes are added either to Ballots or ProposalVotes, depending on period type
-- * Info about current period is updated in PeriodMeta
onBlock
  :: forall m .
  ( MonadPostgresConn m
  , MonadUnliftIO m
  , MonadTezosClient m
  , MonadLogging m
  )
  => TVar (Maybe BlockHead)
  -> Block
  -> m ()
onBlock cache b@Block{..} = do
  adopted <- readAdoptedHead cache
  if bmLevel > bhLevel adopted then do
    when (isPeriodStart bMetadata) $ transact $ do
      logInfo $ "The first block in a period: " +| bmVotingPeriod |+ ", head: " +| block2Head b |+ ""
      -- quorum can be updated only when exploration ends, but who cares
      quorum <- fetchQuorum MainChain (LevelRef bmLevel)
      voters <- fetchVoters MainChain (LevelRef bmLevel)
      totVotes <- refreshVoters voters
      insertPeriodMeta quorum totVotes

    transact $ do
      let currentPeriodNum = bmVotingPeriod
      casted <- case bmVotingPeriodType of
        Proposing -> do
          insertNewProposals
          Left <$> updateProposalVotes
        Testing     -> pure $ Left 0
        Exploration -> Right <$> updateBallots ExplorationVote
        Promotion   -> Right <$> updateBallots PromotionVote
      updatePeriodMetas currentPeriodNum casted
    UIO.atomically $ UIO.writeTVar cache (Just $ block2Head b)
    logInfo $ "Block " +| block2Head b |+ " is applied to the database"
  else
    logInfo $
      "Block's " +| block2Head b |+ " is equal to or behind last adopted one " +| adopted |+ ".\
      \The block was ignored."
  where
    BlockHeader{..} = bHeader
    BlockMetadata{..} = bMetadata
    AgoraSchema{..} = agoraSchema
    startLevel = bmLevel - fromIntegral bmVotingPeriodPosition

    insertNewProposals = do
      let proposals =
            flip concatMap (unOperations bOperations) $
              \case
                BallotOp{} -> []
                ProposalOp _ proposer period propHashes -> map (proposer, period, ) propHashes
      let proposalHashes = map (\(_,_,p) -> p) proposals

      existedHashes <- fmap S.fromList $ runSelectReturningList' $ select $
        filter_ (flip in_ $ map val_ proposalHashes) (prHash <$> all_ asProposals)
      let proposalsNew = filter (\(_, _, p) -> S.notMember p existedHashes) proposals
      runInsert' $ insert asProposals $ insertExpressions $ flip map proposalsNew $ \(who, periodId, what) ->
        Proposal
        { prId           = default_
        , prPeriod       = val_ $ PeriodMetaId periodId
        , prHash         = val_ what
        , prTitle        = val_ Nothing
        , prShortDesc    = val_ Nothing
        , prLongDesc     = val_ Nothing
        , prTimeProposed = val_ bhrTimestamp
        , prProposer     = val_ $ VoterHash who
        , prDiscourseUrl = val_ Nothing
        }
      let newProposalHashes = map (\(_, _, p) -> p) proposalsNew
      unless (null newProposalHashes) $
        logInfo $ "New proposals are added: " +| listF newProposalHashes |+ ""

    updateBallots :: VoteType -> m (Votes, Votes, Votes)
    updateBallots tp = do
      results <- fmap catMaybes $ forM (unOperations bOperations) $ \case
        BallotOp op vhash periodId phash decision -> do
          rolls <- getVoterRolls vhash
          proposalId <- getProposalId phash
          counterMb <- runSelectReturningOne' $ select $ B.aggregate_ (\_ -> countAll_) $ do
            pv <- all_ asBallots
            guard_ (bProposal pv ==. val_ (ProposalId proposalId) &&.
                    bVoter pv ==. val_ (VoterHash vhash))
            pure $ bId pv
          case counterMb of
            Just 0 -> do
              runInsert' $ insert asBallots $ insertExpressions $ one $
                Ballot
                { bId             = default_
                , bVoteType       = val_ tp
                , bVoter          = val_ $ VoterHash vhash
                , bPeriod         = val_ $ PeriodMetaId periodId
                , bProposal       = val_ $ ProposalId proposalId
                , bCastedRolls    = val_ rolls
                , bOperation      = val_ op
                , bBallotTime     = val_ bhrTimestamp
                , bBallotDecision = val_ decision
                }
              pure $ Just (op, decision, fromIntegral rolls)
            _ -> do
              logDebug $ "Duplicating ballot from " +| vhash |+ " for " +| phash |+ ""
              pure Nothing
        ProposalOp{} -> pure Nothing
      let ret =
            foldl (\(!y, !n, !p) (_, d, rolls) ->
                case d of
                  Yay  -> (y + rolls, n, p)
                  Nay  -> (y, n + rolls, p)
                  Pass -> (y, n, p + rolls))
               (0, 0, 0)
               results
      unless (null results) $
        logInfo $ "New ballots are added, operations: " +| blockListF (map (view _1) results) |+ ""
      pure ret

    updateProposalVotes :: m Votes
    updateProposalVotes = do
      results <- fmap (catMaybes . concat) $ forM (unOperations bOperations) $ \case
        BallotOp{} -> pure []
        ProposalOp op vhash _period proposals -> do
          rolls <- getVoterRolls vhash
          forM proposals $ \p -> do
            proposalId <- getProposalId p
            counterMb <- runSelectReturningOne' $ select $ B.aggregate_ (\_ -> countAll_) $ do
              pv <- all_ asProposalVotes
              guard_ (pvProposal pv ==. val_ (ProposalId proposalId) &&.
                      pvVoter pv ==. val_ (VoterHash vhash))
              pure $ pvId pv

            case counterMb of
              Just 0 -> do
                runInsert' $ insert asProposalVotes $ insertExpressions $ one $
                  ProposalVote
                  { pvId          = default_
                  , pvVoter       = val_ $ VoterHash vhash
                  , pvProposal    = val_ $ ProposalId proposalId
                  , pvCastedRolls = val_ rolls
                  , pvOperation   = val_ op
                  , pvVoteTime    = val_ bhrTimestamp
                  }
                pure $ Just (op, fromIntegral rolls)
              _ -> pure Nothing
      let ret = foldl (\ !s (_, rolls) -> s + rolls) 0 results
      unless (null results) $
        logInfo $ "New proposal votes are added, operations: " +| blockListF (map (view _1) results) |+ ""
      pure ret

    updatePeriodMetas :: PeriodId -> Either Votes (Votes, Votes, Votes) -> m ()
    updatePeriodMetas curPer casted =
      runUpdate' $ update asPeriodMetas (\ln ->
        (pmLastBlockLevel ln <-. val_ bmLevel) <>
        (pmLastBlockHash ln <-. val_ bHash) <>
        case casted of
          Left ad -> pmVotesCast ln <-. current_ (pmVotesCast ln) + val_ ad
          Right (y, n, p) ->
            (pmVotesCast ln <-. current_ (pmVotesCast ln) + val_ (y + n + p)) <>
            (pmBallotsYay ln <-. current_ (pmBallotsYay ln) + val_ y) <>
            (pmBallotsNay ln <-. current_ (pmBallotsNay ln) + val_ n) <>
            (pmBallotsPass ln <-. current_ (pmBallotsPass ln) + val_ p)
            )
            (\ln -> pmId ln ==. val_ curPer)

    refreshVoters voters = do
      let !total = fromIntegral $ sum (map vRolls voters)
      -- pva701: dunno how to make batch updates via beam
      -- we can remove foreign key restriction from proposal_votes and ballots
      -- and delete voters, and then insert new ones
      updatedSet <- fmap (S.fromList . concat) $ forM voters $ \v ->
        fmap (map voterPbkHash) $ runPg $ runUpdateReturningList $
          update asVoters (\ln -> voterRolls ln <-. val_ (vRolls v)) (\ln -> voterPbkHash ln ==. val_ (vPkh v))
      let newVoters = filter (\v -> S.notMember (vPkh v) updatedSet) voters
      runInsert' $ insert asVoters $ insertValues $
        map (\v -> DB.Voter (vPkh v) Nothing Nothing (vRolls v)) newVoters
      unless (null newVoters) $
        logInfo $ "New voters are added: " +| mapF (map (\v -> (vPkh v, vRolls v)) newVoters) |+ ""
      pure total

    insertPeriodMeta q totVotes =
      runInsert' $
        insert asPeriodMetas $
        insertValues $ one $ PeriodMeta
          { pmId = bmVotingPeriod
          , pmType = bmVotingPeriodType
          , pmVotesCast = 0
          , pmVotesAvailable = totVotes
          , pmQuorum = q
          , pmWhenStarted = bhrTimestamp
          , pmStartLevel = startLevel
          , pmEndLevel   = startLevel + onePeriod - 1
          , pmLastBlockLevel = bmLevel
          , pmLastBlockHash = bHash
          , pmPrevBlockHash = bhrPredecessor
          , pmBallotsYay = 0
          , pmBallotsNay = 0
          , pmBallotsPass = 0
          }

    getVoterRolls :: PublicKeyHash -> m Rolls
    getVoterRolls vHash = do
      mbRolls <- runSelectReturningOne' $ select $ do
        voter <- all_ asVoters
        guard_ (voterPbkHash voter ==. val_ vHash)
        pure $ voterRolls voter
      maybe (UIO.throwIO $ VoterNotExist vHash) pure mbRolls

    -- getProposalId :: ProposalHash -> m Int
    getProposalId proposalHash = do
      mbProposalId <- runSelectReturningOne' $ select $ do
        pr <- all_ asProposals
        guard_ (prHash pr ==. val_ proposalHash)
        pure $ prId pr
      maybe (UIO.throwIO $ ProposalNotExist proposalHash) pure mbProposalId
