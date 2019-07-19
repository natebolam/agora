{-|
API handlers implementation.
-}
module Agora.Web.Handlers
       ( agoraHandlers
       ) where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Time.Clock (UTCTime (..), addUTCTime, secondsToDiffTime)
import Servant.API.Generic (ToServant)
import Servant.Server.Generic (AsServerT, genericServerT)
import Test.QuickCheck (arbitrary, vector, vectorOf)
import UnliftIO (throwIO)

import Agora.Arbitrary
import Agora.Mode
import Agora.Node
import Agora.Types
import Agora.Util
import Agora.Web.API
import Agora.Web.Error
import Agora.Web.Types

type AgoraHandlers m = ToServant AgoraEndpoints (AsServerT m)

-- | Server handler implementation for Agora API.
agoraHandlers :: forall m . AgoraWorkMode m => AgoraHandlers m
agoraHandlers = genericServerT AgoraEndpoints
  { aePeriod = \case
      Nothing        -> getCurrentPeriodInfo
      Just periodNum -> view _1 <$> getPeriod periodNum
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
  where
    getCurrentPeriodInfo = do
      BlockMetadata{..} <- fetchBlockMetadata MainChain HeadRef
      let startLevel = bmLevel - fromIntegral bmVotingPeriodPosition
      let endLevel = startLevel + fromIntegral (8 * 4096 :: Word32)
      let genesisTime = UTCTime (toEnum 58299) (secondsToDiffTime 58052)
      let startTime = addUTCTime (fromIntegral startLevel * 60) genesisTime
      let endTime = addUTCTime (fromIntegral endLevel * 60) genesisTime
      let period = Period
            { _pId = bmVotingPeriod
            , _pStartLevel = startLevel
            , _pEndLevel = endLevel
            , _pStartTime = startTime
            , _pEndTime   = endTime
            , _pCycle     = bmCycle `mod` fromIntegral (8 :: Word32)
            }
      let total = 10
      let (proposal, voteStats, ballots) = detGen 3 $ (,,) <$> arbitrary <*> arbitrary <*> arbitrary
      pure $
        case bmVotingPeriodType of
          Proposing   -> ProposalInfo period total voteStats
          Exploration -> ExplorationInfo period total proposal voteStats ballots
          Testing     -> TestingInfo period total proposal
          Promotion   -> PromotionInfo period total proposal voteStats ballots

-- | All possible data about the period (for the mock)
type PeriodData = (PeriodInfo, [Proposal], [ProposalVote], [Ballot])

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
      <*> fmap (setIds prId) (vector periodListNum)
      <*> fmap (setIds pvId) (vector periodListNum)
      <*> fmap (setIds bId) (vector periodListNum)
    totalPeriodsNum = 20
    periodListNum = 1000
