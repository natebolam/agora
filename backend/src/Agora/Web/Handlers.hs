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
import Servant.Util.Dummy (paginate)
import Test.QuickCheck (arbitrary, vector, vectorOf)
import UnliftIO (throwIO)

import Agora.Arbitrary
import Agora.Mode
import Agora.Node
import Agora.Types
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
  , aeProposals = \periodNum pagination ->
      paginate pagination . view _2 <$> getPeriod periodNum

  , aeProposalVotes = \periodNum pagination ->
      paginate pagination . view _3 <$> getPeriod periodNum

  , aeBallots = \periodNum pagination ->
      paginate pagination . view _4 <$> getPeriod periodNum
  }
  where
    getCurrentPeriodInfo = do
      BlockMetadata{..} <- getBlockMetadata MainChain HeadRef
      let period = Period
            { _pNum = bmVotingPeriod
            , _pType = bmVotingPeriodType
            , _pStartLevel = bmLevel - fromIntegral bmVotingPeriodPosition
            , _pCycle      = bmCycle `mod` fromIntegral (8 :: Word32)
            }
      pure $ PeriodInfo period Nothing Nothing Nothing


-- | All possible data about the period (for the mock)
type PeriodData = (PeriodInfo, [Proposal], [ProposalVote], [Ballot])

-- | Gets all period data, if a period exists, or fails with 404.
getPeriod :: MonadIO m => PeriodNum -> m PeriodData
getPeriod n = M.lookup n mockApiData `whenNothing` throwIO noSuchPeriod
  where noSuchPeriod = NotFound "Period with given number does not exist"

-- | Mock data for API, randomly generated with a particular seed.
mockApiData :: Map PeriodNum PeriodData
mockApiData = M.fromList $ zipWith setNum [1..] periods
  where setNum i pd = (i, pd & _1.piPeriod.pNum .~ i)
        periods = detGen 42 $ vectorOf 20 $ (,,,)
          <$> arbitrary
          <*> vector 1000
          <*> vector 1000
          <*> vector 1000
