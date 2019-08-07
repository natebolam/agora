{- Module which is responsibe for application block to db. -}

{-# LANGUAGE TypeOperators #-}

module Agora.BlockStack
       ( MonadBlockStack (..)
       , BlockStack (..)
       , withBlockStack
       , blockStackCapOverDb
       , blockStackCapOverDbImplM
       , BlockStackCapImpl
       ) where

import Control.Monad.Reader (withReaderT)
import qualified Data.Set as S
import Database.Beam.Backend (SqlSerial)
import Database.Beam.Backend.SQL.BeamExtensions (runUpdateReturningList)
import Database.Beam.Query (all_, countAll_, current_, default_, filter_, guard_, in_, insert,
                            insertExpressions, insertValues, references_, select, update, val_,
                            (&&.), (<-.), (==.))
import qualified Database.Beam.Query as B
import Fmt (listF, mapF, (+|), (|+))
import Loot.Log (Logging, MonadLogging, logDebug, logInfo)
import Monad.Capabilities (CapImpl (..), CapsT, HasCap, HasNoCap, addCap, makeCap)
import UnliftIO (MonadUnliftIO)
import qualified UnliftIO as UIO

import Agora.DB
import qualified Agora.DB as DB
import Agora.Discourse
import Agora.Node.Client
import Agora.Node.Constants
import Agora.Node.Types
import qualified Agora.Node.Types as TZ
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
  , HasTzConstants caps
  , HasCap DiscourseClient caps
  , HasNoCap BlockStack caps
  , MonadUnliftIO m
  )
  => CapsT (BlockStack ': caps) m a
  -> CapsT caps m a
withBlockStack action = do
  blockStack <- lift blockStackCapOverDbImplM
  withReaderT (addCap blockStack) action

type BlockStackCapImpl m
  = CapImpl BlockStack '[DiscourseClient, PostgresConn, TezosClient, Logging, TzConstantsCap] m
type BlockStackMode m =
  ( MonadUnliftIO m
  , MonadPostgresConn m
  , MonadTezosClient m
  , MonadLogging m
  , MonadTzConstants m
  , MonadDiscourseClient m
  )

blockStackCapOverDbImplM :: MonadUnliftIO m => m (BlockStackCapImpl m)
blockStackCapOverDbImplM = do
  cache <- UIO.newTVarIO (Nothing :: Maybe BlockHead)
  pure $ CapImpl $ blockStackCapOverDb cache

blockStackCapOverDb :: BlockStackMode m => TVar (Maybe BlockHead) -> BlockStack m
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

-- | Analyse the passed block and update corresponding tables.
-- If the passed block is the first in the period then:
-- * Current quorum is updated using /votes/current_quorum
-- * Voters' rolls are updated using /votes/listings
-- For any block:
-- * If block period is Proposal then new proposal are added
-- * New votes are added either to Ballots or ProposalVotes, depending on period type
-- * Info about current period is updated in PeriodMeta
onBlock
  :: forall m . BlockStackMode m
  => TVar (Maybe BlockHead)
  -> Block
  -> m ()
onBlock cache b@Block{..} = do
  let BlockHeader{..} = bHeader
      BlockMetadata{..} = bMetadata
  adopted <- readAdoptedHead cache
  if bmLevel > bhLevel adopted then do
    discourseStubs <- transact $ do
      whenM (isPeriodStart bMetadata) $ do
        logInfo $ "The first block in a period: " +| bmVotingPeriod |+ ", head: " +| block2Head b |+ ""
        -- quorum can be updated only when exploration ends, but who cares
        quorum <- fetchQuorum MainChain (LevelRef bmLevel)
        voters <- fetchVoters MainChain (LevelRef bmLevel)
        totVotes <- refreshVoters voters
        triggerBakersFetch $ S.fromList $ map vPkh voters
        insertPeriodMeta b quorum totVotes

      (discourseStubs, casted) <- case bmVotingPeriodType of
        Proposing -> do
          discourseStubs <- insertNewProposals b
          (discourseStubs, ) . Left <$> updateProposalVotes b
        Testing     -> pure $ ([], Left 0)
        Exploration -> ([],) . Right <$> updateBallots b ExplorationVote
        Promotion   -> ([],) . Right <$> updateBallots b PromotionVote
      updatePeriodMetas b casted
      pure discourseStubs

    UIO.atomically $ UIO.writeTVar cache (Just $ block2Head b)
    logInfo $ "Block " +| block2Head b |+ " is applied to the database"
    mapM_ postProposalStubAsync discourseStubs
  else
    logInfo $
      "Block's " +| block2Head b |+ " is equal to or behind last adopted one " +| adopted |+ ".\
      \The block was ignored."

-- | Fetch proposals from the block and put them into database.
insertNewProposals
  :: ( MonadIO m
     , MonadPostgresConn m
     , MonadLogging m
     , MonadDiscourseClient m
     )
  => Block -> m [ProposalHash]
insertNewProposals Block{..} = do
  let proposalsTbl = asProposals agoraSchema
      proposals = flip concatMap (unOperations bOperations) $ \case
        BallotOp{} -> []
        ProposalOp _ proposer period propHashes -> map (proposer, period, ) propHashes
  let proposalHashes = map (\(_,_,p) -> p) proposals
  existedHashes <- fmap S.fromList $ runSelectReturningList' $ select $
    filter_ (flip in_ $ map val_ proposalHashes) (prHash <$> all_ proposalsTbl)
  let proposalsNew = filter (\(_, _, p) -> S.notMember p existedHashes) proposals

  topicInfo <- forM proposalsNew $ \(_, _, ph) -> do
    let shorten = shortenHash ph
    mTopic <- getProposalTopic shorten
    case mTopic of
      Just topic -> do
        hparts <- case parseHtmlParts (pCooked $ tPosts topic) of
          Left e  -> do
            logDebug $ "Coudln't parse Discourse topic, reason: " +| e |+ ""
            pure $ HtmlParts Nothing Nothing Nothing
          Right hp -> pure $ toHtmlPartsMaybe shorten hp
        pure (Just topic, hparts)
      Nothing    ->
        pure (Nothing, HtmlParts Nothing Nothing Nothing)

  let xs = zip topicInfo proposalsNew
  runInsert' $ insert proposalsTbl $ insertExpressions $
    flip map xs $ \((t, hp), (who, periodId, what)) ->
      Proposal
      { prId                 = default_
      , prPeriod             = val_ $ PeriodMetaId periodId
      , prHash               = val_ what
      , prTimeProposed       = val_ (bhrTimestamp bHeader)
      , prProposer           = val_ $ VoterHash who
      , prDiscourseTitle     = val_ $ unTitle . tTitle <$> t
      , prDiscourseShortDesc = val_ $ hpShort hp
      , prDiscourseLongDesc  = val_ $ hpLong hp
      , prDiscourseFile      = val_ $ hpFileLink hp
      , prDiscourseTopicId   = val_ $ pTopicId . tPosts <$> t
      , prDiscoursePostId    = val_ $ pId . tPosts <$> t
      }
  let discourseStubs = mapMaybe (\((t, _), (_, _, ph)) -> if isNothing t then Just ph else Nothing) xs
  let newProposalHashes = map (\(_, _, p) -> p) proposalsNew
  unless (null newProposalHashes) $
    logInfo $ "New proposals are added: " +| listF newProposalHashes |+ ""
  pure discourseStubs

-- | Fetch the ballot vote operations from the block and add them
-- to the database, ignoring repeated votes.
updateBallots
  :: (MonadUnliftIO m, MonadPostgresConn m, MonadLogging m)
  => Block -> VoteType -> m (Votes, Votes, Votes)
updateBallots Block{..} tp = do
  let ballotsTbl = asBallots agoraSchema
  results <- fmap catMaybes $ forM (unOperations bOperations) $ \case
    BallotOp op vhash periodId phash decision -> do
      rolls <- getVoterRolls vhash
      proposalId <- getProposalId phash
      counterMb <- runSelectReturningOne' $ select $ B.aggregate_ (\_ -> countAll_) $ do
        pv <- all_ ballotsTbl
        guard_ (bProposal pv ==. val_ (ProposalId proposalId) &&.
                bVoter pv ==. val_ (VoterHash vhash) &&.
                bVoteType pv ==. val_ tp)
        pure $ bId pv
      case counterMb of
        Just 0 -> do
          runInsert' $ insert ballotsTbl $ insertExpressions $ one $
            Ballot
            { bId             = default_
            , bVoteType       = val_ tp
            , bVoter          = val_ $ VoterHash vhash
            , bPeriod         = val_ $ PeriodMetaId periodId
            , bProposal       = val_ $ ProposalId proposalId
            , bCastedRolls    = val_ rolls
            , bOperation      = val_ op
            , bBallotTime     = val_ (bhrTimestamp bHeader)
            , bBallotDecision = val_ decision
            }
          pure $ Just (op, decision, fromIntegral rolls)
        _ -> do
          logDebug $ "Duplicating ballot from " +| vhash |+ " for " +| phash |+ ""
          pure Nothing
    ProposalOp{} -> pure Nothing
  let addVote (!y, !n, !p) (_, d, rolls) = case d of
        Yay  -> (y + rolls, n, p)
        Nay  -> (y, n + rolls, p)
        Pass -> (y, n, p + rolls)
      ret = foldl addVote (0, 0, 0) results
  unless (null results) $
    logInfo $ "New ballots are added, operations: " +| listF (map (view _1) results) |+ ""
  pure ret

-- | Adds new votes for proposals to the database, ignoring
-- repeated votes.
updateProposalVotes
  :: (MonadUnliftIO m, MonadPostgresConn m, MonadLogging m)
  => Block -> m Votes
updateProposalVotes Block {..} = do
  let AgoraSchema {..} = agoraSchema
  results <- fmap (catMaybes . concat) $ forM (unOperations bOperations) $ \case
    BallotOp{} -> pure []
    ProposalOp op vhash periodId proposals -> do
      rolls <- getVoterRolls vhash
      forM proposals $ \p -> do
        proposalId <- getProposalId p
        mPairCounter <- runSelectReturningOne' $ select $ B.aggregate_ (const countAll_) $ do
          pv <- all_ asProposalVotes
          guard_ (pvProposal pv ==. val_ (ProposalId proposalId) &&.
                  pvVoter pv ==. val_ (VoterHash vhash))
          pure $ pvId pv

        case mPairCounter of
          Just 0 -> do
            mVoterCounter <- runSelectReturningOne' $ select $ B.aggregate_ (const countAll_) $ do
              pv <- all_ asProposalVotes
              prop <- all_ asProposals
              guard_ (DB.pvProposal pv `references_` prop)
              guard_ (prPeriod prop ==. val_ (PeriodMetaId periodId) &&.
                      pvVoter pv ==. val_ (VoterHash vhash))
              pure $ pvId pv

            runInsert' $ insert asProposalVotes $ insertExpressions $ one $
              ProposalVote
              { pvId          = default_
              , pvVoter       = val_ $ VoterHash vhash
              , pvProposal    = val_ $ ProposalId proposalId
              , pvCastedRolls = val_ rolls
              , pvOperation   = val_ op
              , pvVoteTime    = val_ (bhrTimestamp bHeader)
              }

            case mVoterCounter of
              Just 0 -> pure $ Just (op, fromIntegral rolls)
              _      -> pure $ Just (op, 0)
          _ -> pure Nothing

  let ret = foldl (\ !s (_, rolls) -> s + rolls) 0 results
  unless (null results) $
    logInfo $ "New proposal votes are added, operations: " +| listF (map (view _1) results) |+ ""
  pure ret

-- | Adds casted votes (proposal votes or ballots) to the
-- total vote counters in period meta in database.
updatePeriodMetas
  :: (MonadIO m, MonadPostgresConn m)
  => Block -> Either Votes (Votes, Votes, Votes) -> m ()
updatePeriodMetas Block{..} casted =
  runUpdate' $ update (asPeriodMetas agoraSchema) (\ln ->
    (pmLastBlockLevel ln <-. val_ (bmLevel bMetadata)) <>
    (pmLastBlockHash ln <-. val_ bHash) <>
    case casted of
      Left ad -> pmVotesCast ln <-. current_ (pmVotesCast ln) + val_ ad
      Right (y, n, p) ->
        (pmVotesCast ln <-. current_ (pmVotesCast ln) + val_ (y + n + p)) <>
        (pmBallotsYay ln <-. current_ (pmBallotsYay ln) + val_ y) <>
        (pmBallotsNay ln <-. current_ (pmBallotsNay ln) + val_ n) <>
        (pmBallotsPass ln <-. current_ (pmBallotsPass ln) + val_ p))
    (\ln -> pmId ln ==. val_ (bmVotingPeriod bMetadata))

-- | Adds newly encountered voters to the database
-- and update info about existing ones.
refreshVoters
  :: (MonadIO m, MonadPostgresConn m, MonadLogging m)
  => [TZ.Voter] -> m Votes
refreshVoters voters = do
  let !total = fromIntegral $ sum (map vRolls voters)
      AgoraSchema {..} = agoraSchema
  -- pva701: dunno how to make batch updates via beam
  -- we can remove foreign key restriction from proposal_votes and ballots
  -- and delete voters, and then insert new ones
  --
  -- flyingleafe: we can do it properly with using Postgres `ON CONFLICT`
  -- statements (https://tathougies.github.io/beam/user-guide/backends/beam-postgres/#specifying-actions)
  updatedSet <- fmap (S.fromList . concat) $ forM voters $ \v ->
    fmap (map voterPbkHash) $ runPg $ runUpdateReturningList $
      update asVoters (\ln -> voterRolls ln <-. val_ (vRolls v)) (\ln -> voterPbkHash ln ==. val_ (vPkh v))
  let newVoters = filter (\v -> S.notMember (vPkh v) updatedSet) voters
  runInsert' $ insert asVoters $ insertValues $
    map (\v -> DB.Voter (vPkh v) Nothing Nothing (vRolls v)) newVoters
  unless (null newVoters) $
    logInfo $ "New voters are added: " +| mapF (map (\v -> (vPkh v, vRolls v)) newVoters) |+ ""
  pure total

-- | Initialize a new `PeriodMeta` record in the database
-- as the new period starts.
insertPeriodMeta
  :: (MonadTzConstants m, MonadPostgresConn m, MonadIO m)
  => Block -> Quorum -> Votes -> m ()
insertPeriodMeta Block{..} q totVotes = do
  let BlockHeader{..} = bHeader
      BlockMetadata{..} = bMetadata
      startLevel = bmLevel - fromIntegral bmVotingPeriodPosition
  onePeriod <- askOnePeriod
  runInsert' $
    insert (asPeriodMetas agoraSchema) $
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

-- | Fetches the number of voter rolls from the database.
-- Throws an exception if the voter is unknown.
getVoterRolls
  :: (MonadUnliftIO m, MonadPostgresConn m)
  => PublicKeyHash -> m Rolls
getVoterRolls vHash = do
  mbRolls <- runSelectReturningOne' $ select $ do
    voter <- all_ $ asVoters agoraSchema
    guard_ (voterPbkHash voter ==. val_ vHash)
    pure $ voterRolls voter
  maybe (UIO.throwIO $ VoterNotExist vHash) pure mbRolls

-- | Gets database ID of a proposal with a given hash.
getProposalId
  :: (MonadUnliftIO m, MonadPostgresConn m)
  => ProposalHash -> m (SqlSerial Int)
getProposalId proposalHash = do
  mbProposalId <- runSelectReturningOne' $ select $ do
    pr <- all_ $ asProposals agoraSchema
    guard_ (prHash pr ==. val_ proposalHash)
    pure $ prId pr
  maybe (UIO.throwIO $ ProposalNotExist proposalHash) pure mbProposalId
