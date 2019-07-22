{-|
API handlers implementation.
-}
module Agora.Web.Handlers
       ( agoraHandlers
       ) where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Time.Clock (addUTCTime)
import Database.Beam.Backend (SqlSerial (..))
import Database.Beam.Query (aggregate_, all_, group_, guard_, max_, oneToMany_, select, sum_, val_,
                            (==.))
import Fmt ((+|), (|+))
import Servant.API.Generic (ToServant)
import Servant.Server.Generic (AsServerT, genericServerT)
import Test.QuickCheck (arbitrary, vector, vectorOf)
import UnliftIO (throwIO)

import Agora.Arbitrary
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
  { aePeriod = getPeriodInfo
  , aeProposals = \periodNum pagination mLastId ->
      paginateWithId pagination mLastId . view _2 <$> getPeriod periodNum

  , aeProposalVotes = \periodNum pagination mLastId ->
      paginateWithId pagination mLastId . view _3 <$> getPeriod periodNum

  , aeBallots = \periodNum pagination mLastId mDecision ->
      case mDecision of
        Just d ->
          paginateWithId pagination mLastId . filter ((d ==) . _bDecision) . view _4 <$> getPeriod periodNum
        Nothing ->
          paginateWithId pagination mLastId .  view _4 <$> getPeriod periodNum
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
  proposals <- runSelectReturningList' $ select $
    aggregate_ (\pv -> (group_ (pvProposal pv), sum_ (pvCastedRolls pv))) $ do
      p <- all_ (asProposals agoraSchema)
      guard_ $ prPeriod p ==. val_ (PeriodMetaId period)
      oneToMany_ (asProposalVotes agoraSchema) pvProposal p
  case sortOn (\(_, mx) -> -fromIntegral (fromMaybe 0 mx) :: Int32) proposals of
    []      -> throwIO $ InternalError $ "No one proposal in period " +| period |+ " found."
    ((pid, _) : _) -> do
      resMb <- runSelectReturningOne' $ select $ do
        p <-  all_ (asProposals agoraSchema)
        guard_ $ DB.prId p ==. val_ (unProposalId pid)
        pure p
      DB.Proposal{prId=propId,..} <- resMb `whenNothing`
          throwIO (InternalError $ "Proposal #" +| unSerial (unProposalId pid) |+ " unexpectedly not found ")
      pure $
        T.Proposal
        { _prId = Id $ fromIntegral $ unSerial propId
        , _prHash = prHash
        , _prTitle = prTitle
        , _prShortDescription = prShortDesc
        , _prLongDescription = prLongDesc
        , _prTimeCreated = prTimeProposed
        , _prProposalFile = Nothing -- where it should come from?
        , _prDiscourseLink = Nothing
        , _prProposer = Baker (unVoterHash prProposer) 0 "" Nothing
        }

getLastPeriod :: AgoraWorkMode m => m PeriodId
getLastPeriod = do
  perMb <- runSelectReturningOne' $ select $
    aggregate_ (max_ . pmId) (all_ $ asPeriodMetas agoraSchema)
  case perMb of
    Just (Just pid) -> pure pid
    _               -> throwIO PeriodMetasNotFilledYet

  -- | All possible data about the period (for the mock)
type PeriodData = (PeriodInfo, [T.Proposal], [T.ProposalVote], [T.Ballot])

-- | Gets all period data, if a period exists, or fails with 404.
getPeriod :: MonadIO m => PeriodId -> m PeriodData
getPeriod n = M.lookup n mockApiData `whenNothing` throwIO noSuchPeriod
  where noSuchPeriod = NotFound "Period with given number does not exist"

-- | Mock data for API, randomly generated with a particular seed.
mockApiData :: Map PeriodId PeriodData
mockApiData = M.fromList $ zipWith setNum [1..] periods
  where
    setNum i pd =
          let pd' = pd & _1.iPeriod.pId .~ i
                       & _1.iTotalPeriods .~ fromIntegral totalPeriodsNum
          in (i, pd')
    setIds lens = zipWith (\i pd -> pd & lens .~ i) [1..]
    periods = detGen 42 $ vectorOf totalPeriodsNum $ (,,,)
      <$> arbitrary
      <*> fmap (setIds T.prId) (vector periodListNum)
      <*> fmap (setIds T.pvId) (vector periodListNum)
      <*> fmap (setIds T.bId) (vector periodListNum)
    totalPeriodsNum = 20
    periodListNum = 1000
