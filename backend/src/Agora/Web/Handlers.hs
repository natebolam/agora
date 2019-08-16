{-|
API handlers implementation.
-}
module Agora.Web.Handlers
       ( agoraHandlers

       -- * For tests
       , getPeriodInfo
       , getProposals
       , getProposalVotes
       , getSpecificProposalVotes
       , getBallots
       ) where

import Data.Time.Clock (addUTCTime, getCurrentTime)
import Database.Beam.Postgres (Postgres)
import Database.Beam.Query (Projectible, Q, QBaseScope, QExpr, aggregate_, all_, countAll_, desc_,
                            group_, guard_, limit_, max_, oneToMany_, orderBy_, references_, select,
                            sum_, val_, (&&.), (<.), (==.))
import Database.Beam.Query.Internal (QNested)
import Fmt (build, fmt, (+|), (|+))
import Servant.API.Generic (ToServant)
import Servant.Server.Generic (AsServerT, genericServerT)
import qualified Universum.Unsafe as U
import UnliftIO (throwIO)

import Agora.Config
import Agora.DB
import qualified Agora.DB as DB
import Agora.Mode
import Agora.Node
import Agora.Types
import Agora.Util
import Agora.Web.API
import Agora.Web.Error
import Agora.Web.Types
import qualified Agora.Web.Types as T

type AgoraHandlers m = ToServant AgoraEndpoints (AsServerT m)

-- | Server handler implementation for Agora API.
agoraHandlers :: forall m . AgoraWorkMode m => AgoraHandlers m
agoraHandlers = genericServerT AgoraEndpoints
  { aePeriod                = getPeriodInfo
  , aeProposals             = getProposals
  , aeProposal              = getProposal
  , aeSpecificProposalVotes = getSpecificProposalVotes
  , aeProposalVotes         = getProposalVotes
  , aeBallots               = getBallots
  }

-- | Fetch info about specific period.
-- If Nothing passed the last known period will be used.
getPeriodInfo :: AgoraWorkMode m => Maybe PeriodId -> m PeriodInfo
getPeriodInfo periodIdMb = do
  periodId <- case periodIdMb of
    Nothing  -> getLastPeriod
    Just pid -> pure pid
  pMb <- runSelectReturningOne' $ select $ do
    pm <- all_ (asPeriodMetas agoraSchema)
    guard_ (pmId pm ==. val_ periodId)
    pure pm
  oneCycle <- tzCycleLength <$> askTzConstants

  PeriodMeta{..} <- pMb `whenNothing` throwIO noSuchPeriod
  curTime <- liftIO getCurrentTime
  let leftLevels = pmEndLevel - pmLastBlockLevel
  let passedLevels = pmLastBlockLevel  - pmStartLevel + 1
  let _iPeriod =
          Period
          { _pId = periodId
          , _pStartLevel = pmStartLevel
          , _pEndLevel   = pmEndLevel
          , _pStartTime  = pmWhenStarted
          , _pEndTime    = addUTCTime (fromIntegral leftLevels * 60) curTime
          , _pCycle      = fromIntegral $ passedLevels `div` oneCycle
          }
  _iTotalPeriods <- fromIntegral . (+1) <$> getLastPeriod
  _iDiscourseLink <- askDiscourseHost
  case pmType of
    Proposing -> do
      let _piVoteStats = VoteStats pmVotesCast pmVotesAvailable
      pure $ ProposalInfo {..}
    Exploration -> do
      _eiProposal <- getWinner (periodId - 1)
      let _eiVoteStats = VoteStats pmVotesCast pmVotesAvailable
      let _eiBallots = Ballots pmBallotsYay pmBallotsNay pmBallotsPass (fromIntegral pmQuorum / 100.0) 80.0
      pure $ ExplorationInfo{..}
    Testing -> do
      _tiProposal <- getWinner (periodId - 2)
      pure $ TestingInfo{..}
    Promotion -> do
      _piProposal <- getWinner (periodId - 3)
      let _piVoteStats = VoteStats pmVotesCast pmVotesAvailable
      let _piBallots = Ballots pmBallotsYay pmBallotsNay pmBallotsPass (fromIntegral pmQuorum / 100.0) 80.0
      pure $ PromotionInfo{..}
  where
    noSuchPeriod = NotFound "Period with given number does not exist"

-- | Return the winner of proposal period.
-- Implying that this function is calling
-- only when a winner exists.
getWinner :: AgoraWorkMode m => PeriodId -> m T.Proposal
getWinner period = do
  proposals <- getProposals period
  case sortOn (Down . _prVotesCasted) proposals of
    []         -> throwIO $ InternalError $ "No one proposal in period " +| period |+ " found."
    (prop : _) -> pure prop

-- | Fetch last known period.
-- It's possible that the database is empty and
-- last period is unknow, in this case @PeriodMetasNotFilledYet@ will be thrown.
getLastPeriod :: AgoraWorkMode m => m PeriodId
getLastPeriod = do
  perMb <- runSelectReturningOne' $ select $
    aggregate_ (max_ . pmId) (all_ $ asPeriodMetas agoraSchema)
  case perMb of
    Just (Just pid) -> pure pid
    _               -> throwIO PeriodMetasNotFilledYet

-- | Get all proposals for passed period.
getProposals
  :: AgoraWorkMode m
  => PeriodId
  -> m [T.Proposal]
getProposals periodId = do
  host <- askDiscourseHost
  results <- fmap (map (second $ fromIntegral . fromMaybe 0)) $
    runSelectReturningList' $ select $ do
      -- group all proposal votes in the period by id,
      -- aggregate casted rolls
      (propId, casted) <- aggregate_ (\pv -> (group_ (pvProposal pv), sum_ (pvCastedRolls pv))) $
        getAllProposalVotesForPeriod periodId
      prop <- all_ (asProposals agoraSchema)
      voter <- all_ (asVoters agoraSchema)
      -- fetch info about corresponding proposals and proposer
      guard_ (propId `references_` prop)
      guard_ (DB.prProposer prop `references_` voter)
      pure (prop, voter, casted)
  pure $ sortOn (Down . \x -> (_prVotesCasted x, _prHash x)) $ map (convertProposal host) results

-- | Get info about proposal by proposal id.
getProposal
  :: AgoraWorkMode m
  => ProposalId
  -> m T.Proposal
getProposal propId = do
  host <- askDiscourseHost
  resultMb <- fmap (map (second $ fromIntegral . fromMaybe 0)) $
    runSelectReturningOne' $ select $ do
      -- aggregate casted votes of proposal votes for passed proposal id
      casted <- aggregate_ (sum_ . pvCastedRolls) $ do
        p <- all_ (asProposalVotes agoraSchema)
        guard_ $ pvProposal p ==. val_ (ProposalId $ fromIntegral propId)
        pure p
      pr <- all_ (asProposals agoraSchema)
      voter <- all_ (asVoters agoraSchema)
      guard_ $ DB.prId pr ==. val_ (fromIntegral propId)
      guard_ $ DB.prProposer pr `references_` voter
      pure (pr, voter, casted)
  result <- resultMb `whenNothing` throwIO (NotFound "Proposal with given id not exist")
  pure $ convertProposal host result

-- | Fetch proposal votes for a proposal
getSpecificProposalVotes
  :: AgoraWorkMode m
  => ProposalId
  -> Maybe ProposalVoteId
  -> Maybe Limit
  -> m (PaginatedList T.ProposalVote)
getSpecificProposalVotes propId mLastId mLimit = do
  let limit = fromMaybe 20 mLimit
      lastIdGuard
        :: forall s. ProposalVoteT (QExpr Postgres s)
        -> Q Postgres AgoraSchema s ()
      lastIdGuard pv = case mLastId of
        Nothing     -> pure ()
        Just lastId -> guard_ $ DB.pvId pv <. val_ (fromIntegral lastId)

  results <- runSelectReturningList' $ select $
    limit_ (fromIntegral limit) $ orderBy_ (desc_ . DB.pvId . (^. _1)) $ do
      prop <- all_ (asProposals agoraSchema)
      pv <- all_ (asProposalVotes agoraSchema)
      voter <- all_ (asVoters agoraSchema)

      guard_ $ DB.prId prop ==. val_ (fromIntegral propId)
      guard_ $ DB.pvProposal pv `references_` prop
      guard_ $ DB.pvVoter pv `references_` voter
      lastIdGuard pv

      pure (pv, voter, DB.prHash prop)

  let pVotes = map convertProposalVote results
  buildPaginatedList limit (fromIntegral . _pvId) pVotes $ do
    pv <- all_ (asProposalVotes agoraSchema)
    guard_ $ DB.pvProposal pv ==. val_ (DB.ProposalId $ fromIntegral propId)
    lastIdGuard pv
    return pv

-- | Fetch proposal votes for period according to pagination params.
getProposalVotes
  :: AgoraWorkMode m
  => PeriodId
  -> Maybe ProposalVoteId
  -> Maybe Limit
  -> m (PaginatedList T.ProposalVote)
getProposalVotes periodId mLastId mLimit = do
  let limit = fromMaybe 20 mLimit
  -- fetch proposal votes with id less that lastId
  let sqlBody :: Q Postgres AgoraSchema s (ProposalVoteT (QExpr Postgres s))
      sqlBody =
        case mLastId of
          Nothing -> getAllProposalVotesForPeriod periodId
          Just lastId -> do
            x <- getAllProposalVotesForPeriod periodId
            guard_ $ DB.pvId x <. val_ (fromIntegral lastId)
            pure x

  results <- runSelectReturningList' $ select $
    limit_ (fromIntegral limit) $ orderBy_ (desc_ . DB.pvId . (^. _1)) $ do
      pv <- sqlBody
      prop <- all_ (asProposals agoraSchema)
      voter <- all_ (asVoters agoraSchema)
      -- fetch corresponding proposal hash and voter
      guard_ (DB.pvProposal pv `references_` prop)
      guard_ (DB.pvVoter pv `references_` voter)
      pure (pv, voter, DB.prHash prop)

  buildPaginatedList limit (fromIntegral . _pvId) (map convertProposalVote results) sqlBody

-- | Fetch ballots for period according to pagination params.
getBallots
  :: AgoraWorkMode m
  => PeriodId
  -> Maybe BallotId
  -> Maybe Limit
  -> Maybe Decision
  -> m (PaginatedList T.Ballot)
getBallots periodId mLastId mLimit mDec = do
  let limit = fromMaybe 20 mLimit
  let sqlBody :: Q Postgres AgoraSchema s (BallotT (QExpr Postgres s))
      sqlBody = do
        x <- all_ (asBallots agoraSchema)
        guard_ $ DB.bPeriod x ==. val_ (PeriodMetaId periodId) &&.
          case mLastId of
            Nothing     -> val_ True
            Just lastId -> DB.bId x <. val_ (fromIntegral lastId)
          &&.
          case mDec of
            Nothing  -> val_ True
            Just dec -> DB.bBallotDecision x ==. val_ dec
        pure x

  results <- runSelectReturningList' $ select $
    limit_ (fromIntegral limit) $ orderBy_ (desc_ . DB.bId . fst) $ do
      ballot <- sqlBody
      voter <- all_ (asVoters agoraSchema)
      guard_ (DB.bVoter ballot `references_` voter)
      pure (ballot, voter)

  buildPaginatedList limit (fromIntegral . _bId) (map convertBallot results) sqlBody

-- | Takes limit, list and sql request
-- which selects entries to return and build PaginatedList.
buildPaginatedList
  ::
  ( AgoraWorkMode m
  , Projectible Postgres r
  )
  => Limit
  -> (a -> Word32)
  -> [a]
  -> Q Postgres AgoraSchema (QNested QBaseScope) r
  -> m (PaginatedList a)
buildPaginatedList limit fetchId plResults sqlCounter = do
  let pdLimit = Just limit
  let pdLastId =
        case plResults of
          [] -> Nothing
          _  -> Just $ fetchId $ U.last plResults

  rest <- fmap (fromMaybe 0) $
    runSelectReturningOne' $ select $ aggregate_ (const countAll_) sqlCounter
  let pdRest = fromIntegral $ rest - length plResults
  let plPagination = PaginationData {..}
  pure $ PaginatedList{..}

-- | Fetch all proposal votes in passed period.
getAllProposalVotesForPeriod
  :: PeriodId
  -> Q Postgres AgoraSchema s (ProposalVoteT (QExpr Postgres s))
getAllProposalVotesForPeriod periodId = do
  p <- all_ (asProposals agoraSchema)
  guard_ $ prPeriod p ==. val_ (PeriodMetaId periodId)
  oneToMany_ (asProposalVotes agoraSchema) pvProposal p

askDiscourseHost :: MonadAgoraConfig m => m Text
askDiscourseHost = fmt . build <$> fromAgoraConfig (sub #discourse . option #host)

---------------------------------------------------------------------------
-- Converters from db datatypes to corresponding web ones
---------------------------------------------------------------------------

convertProposal :: Text -> (DB.Proposal, DB.Voter, Votes) -> T.Proposal
convertProposal discourseHost (DB.Proposal{prId=propId,..}, DB.Voter{..}, casted) =
  T.Proposal
  { _prId = fromIntegral propId
  , _prPeriod = unPeriodMetaId prPeriod
  , _prHash = prHash
  , _prTitle = prDiscourseTitle
  , _prShortDescription = prDiscourseShortDesc
  , _prLongDescription = prDiscourseLongDesc
  , _prTimeCreated = prTimeProposed
  , _prProposalFile = prDiscourseFile
  , _prDiscourseLink = liftA2 sl (Just $ discourseHost `sl` "t") (fmt . build <$> prDiscourseTopicId)
  , _prProposer = Baker (unVoterHash prProposer) voterRolls (fromMaybe "" voterName) voterLogoUrl
  , _prVotesCasted = casted
  }
  where
    sl a b = a <> "/" <> b

convertBallot :: (DB.Ballot, DB.Voter) -> T.Ballot
convertBallot (DB.Ballot{bId=ballId,..}, DB.Voter{..}) =
  T.Ballot
  { _bId = Id $ fromIntegral ballId
  , _bAuthor = Baker (unVoterHash bVoter)
               bCastedRolls (fromMaybe "" voterName) voterLogoUrl
  , _bDecision = bBallotDecision
  , _bOperation = bOperation
  , _bTimestamp = bBallotTime
  }

convertProposalVote :: (DB.ProposalVote, DB.Voter, ProposalHash) -> T.ProposalVote
convertProposalVote (DB.ProposalVote{pvId=propVoteId,..}, DB.Voter{..}, pHash) =
  T.ProposalVote
  { _pvId = Id $ fromIntegral propVoteId
  , _pvProposal = pHash
  , _pvAuthor = Baker (unVoterHash pvVoter)
                pvCastedRolls (fromMaybe "" voterName) voterLogoUrl
  , _pvOperation = pvOperation
  , _pvTimestamp = pvVoteTime
  }
