{-|
API handlers implementation.
-}
module Agora.Web.Handlers
       ( agoraHandlers
       ) where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
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

  , aeBallots = \periodNum pagination mLastId ->
      paginateWithId pagination mLastId . view _4 <$> getPeriod periodNum
  }
  where
    getCurrentPeriodInfo = do
      BlockMetadata{..} <- getBlockMetadata MainChain HeadRef
      let startLevel = bmLevel - fromIntegral bmVotingPeriodPosition
      let startTime = detGen 3 arbitrary
      let period = Period
            { _pId = bmVotingPeriod
            , _pStartLevel = startLevel
            , _pEndLevel = startLevel + fromIntegral (8 * 4096 :: Word32)
            , _pStartTime = startTime
            , _pEndTime   = startTime
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
mockApiData = M.fromList $ zip [1..] periods
  where
    -- setNum i pd =
    --       let pd' = pd & _1.piPeriod.pId .~ i
    --                    & _1.piTotalPeriods .~ fromIntegral totalPeriodsNum
    --       in (i, pd')
    periods = detGen 42 $ vectorOf totalPeriodsNum $ (,,,)
      <$> arbitrary
      <*> vector periodListNum
      <*> vector periodListNum
      <*> vector periodListNum
    totalPeriodsNum = 20
    periodListNum = 1000
