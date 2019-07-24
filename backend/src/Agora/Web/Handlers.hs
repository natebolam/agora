{-|
API handlers implementation.
-}
module Agora.Web.Handlers
       ( agoraHandlers

       -- * For tests
       , getPeriodInfo
       , getProposals
       , getProposalVotes
       , getBallots
       ) where

import Data.Time.Clock (addUTCTime)
import Database.Beam.Postgres (Postgres)
import Database.Beam.Query (Projectible, Q, QBaseScope, QExpr, aggregate_, all_, countAll_, desc_,
                            group_, guard_, limit_, max_, oneToMany_, orderBy_, references_, select,
                            sum_, val_, (&&.), (<.), (==.))
import Database.Beam.Query.Internal (QNested)
import Fmt ((+|), (|+))
import Servant.API.Generic (ToServant)
import Servant.Server.Generic (AsServerT, genericServerT)
import qualified Universum.Unsafe as U
import UnliftIO (throwIO)

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
  { aePeriod        = getPeriodInfo
  , aeProposals     = getProposals
  , aeProposalVotes = getProposalVotes
  , aeBallots       = getBallots
  }

getPeriodInfo :: AgoraWorkMode m => Maybe PeriodId -> m PeriodInfo
getPeriodInfo periodIdMb = do
  periodId <- case periodIdMb of
    Nothing  -> getLastPeriod
    Just pid -> pure pid
  pMb <- runSelectReturningOne' $ select $ do
    pm <- all_ (asPeriodMetas agoraSchema)
    guard_ (pmId pm ==. val_ periodId)
    pure pm
  onePeriod <- askOnePeriod
  oneCycle <- tzCycleLength <$> askTzConstants

  PeriodMeta{..} <- pMb `whenNothing` throwIO noSuchPeriod
  let _iPeriod =
          Period
          { _pId = periodId
          , _pStartLevel = pmStartLevel
          , _pEndLevel   = pmEndLevel
          , _pStartTime  = pmWhenStarted
          , _pEndTime    = addUTCTime (fromIntegral onePeriod * 60) pmWhenStarted
          , _pCycle      = fromIntegral $ (pmLastBlockLevel - pmStartLevel + 1) `div` oneCycle
          }
  _iTotalPeriods <- fromIntegral . (+1) <$> getLastPeriod
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
-- It's implying that if this function is calling
-- then a winner exists.
getWinner :: AgoraWorkMode m => PeriodId -> m T.Proposal
getWinner period = do
  proposals <- getProposals period
  case sortOn (negate . (fromIntegral @_ @Int32) . _prVotesCasted) proposals of
    []         -> throwIO $ InternalError $ "No one proposal in period " +| period |+ " found."
    (prop : _) -> pure prop

getLastPeriod :: AgoraWorkMode m => m PeriodId
getLastPeriod = do
  perMb <- runSelectReturningOne' $ select $
    aggregate_ (max_ . pmId) (all_ $ asPeriodMetas agoraSchema)
  case perMb of
    Just (Just pid) -> pure pid
    _               -> throwIO PeriodMetasNotFilledYet

getProposals
  :: AgoraWorkMode m
  => PeriodId
  -> m [T.Proposal]
getProposals periodId = do
  results <- fmap (map (second $ fromIntegral . fromMaybe 0)) $
    runSelectReturningList' $ select $ do
      (propId, casted) <- aggregate_ (\pv -> (group_ (pvProposal pv), sum_ (pvCastedRolls pv))) $
        getAllProposalVotesForPeriod periodId
      prop <- all_ (asProposals agoraSchema)
      guard_ (propId `references_` prop)
      pure (prop, casted)
  pure $ reverse $ sortOn (\x -> (_prVotesCasted x, _prHash x)) $ map convertProposal results

getProposalVotes
  :: AgoraWorkMode m
  => PeriodId
  -> Maybe ProposalVoteId
  -> Maybe Limit
  -> m (PaginatedList T.ProposalVote)
getProposalVotes periodId mLastId mLimit = do
  let limit = fromMaybe 20 mLimit
  let sqlBody :: Q Postgres AgoraSchema s (ProposalVoteT (QExpr Postgres s))
      sqlBody =
        case mLastId of
          Nothing -> getAllProposalVotesForPeriod periodId
          Just lastId -> do
            x <- getAllProposalVotesForPeriod periodId
            guard_ $ DB.pvId x <. val_ (fromIntegral lastId)
            pure x

  results <- runSelectReturningList' $ select $
    limit_ (fromIntegral limit) $ orderBy_ (desc_ . DB.pvId . fst) $ do
      pv <- sqlBody
      prop <- all_ (asProposals agoraSchema)
      guard_ (DB.pvProposal pv `references_` prop)
      pure (pv, DB.prHash prop)

  buildPaginationList limit (fromIntegral . _pvId) (map convertProposalVote results) sqlBody

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
    limit_ (fromIntegral limit) $ orderBy_ (desc_ . DB.bId) sqlBody

  buildPaginationList limit (fromIntegral . _bId) (map convertBallot results) sqlBody

buildPaginationList
  ::
  ( AgoraWorkMode m
  , Projectible Postgres r)
  => Limit
  -> (a -> Word32)
  -> [a]
  -> Q Postgres AgoraSchema (QNested QBaseScope) r
  -> m (PaginatedList a)
buildPaginationList limit fetchId plResults sqlCounter = do
  let pdLimit = Just limit
  let pdLastId =
        case plResults of
          [] -> Nothing
          _  -> Just $ fetchId $ U.last plResults

  rest <- fmap (fromMaybe 0) $
    runSelectReturningOne' $ select $ aggregate_ (\_ -> countAll_) sqlCounter
  let pdRest = fromIntegral $ rest - length plResults
  let plPagination = PaginationData {..}
  pure $ PaginatedList{..}

getAllProposalVotesForPeriod
  :: PeriodId
  -> Q Postgres AgoraSchema s (ProposalVoteT (QExpr Postgres s))
getAllProposalVotesForPeriod periodId = do
  p <- all_ (asProposals agoraSchema)
  guard_ $ prPeriod p ==. val_ (PeriodMetaId periodId)
  oneToMany_ (asProposalVotes agoraSchema) pvProposal p

convertProposal :: (DB.Proposal, Votes) -> T.Proposal
convertProposal (DB.Proposal{prId=propId,..}, casted) =
  T.Proposal
  { _prId = Id $ fromIntegral propId
  , _prHash = prHash
  , _prTitle = prTitle
  , _prShortDescription = prShortDesc
  , _prLongDescription = prLongDesc
  , _prTimeCreated = prTimeProposed
  , _prProposalFile = Nothing -- where it should come from?
  , _prDiscourseLink = Nothing
  , _prProposer = Baker (unVoterHash prProposer) 0 "" Nothing
  , _prVotesCasted = casted
  }

convertBallot :: DB.Ballot -> T.Ballot
convertBallot DB.Ballot{bId=ballId,..} =
  T.Ballot
  { _bId = Id $ fromIntegral ballId
  , _bAuthor = Baker (unVoterHash bVoter) bCastedRolls "" Nothing
  , _bDecision = bBallotDecision
  , _bOperation = bOperation
  , _bTimestamp = bBallotTime
  }

convertProposalVote :: (DB.ProposalVote, ProposalHash) -> T.ProposalVote
convertProposalVote (DB.ProposalVote{pvId=propVoteId,..}, pHash) =
  T.ProposalVote
  { _pvId = Id $ fromIntegral propVoteId
  , _pvProposal = pHash
  , _pvAuthor = Baker (unVoterHash pvVoter) pvCastedRolls "" Nothing
  , _pvOperation = pvOperation
  , _pvTimestamp = pvVoteTime
  }
