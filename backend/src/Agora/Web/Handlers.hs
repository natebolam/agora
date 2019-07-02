{-|
API handlers implementation.
-}
module Agora.Web.Handlers
       ( agoraHandlers
       , AgoraHandlersMode
       ) where

import Control.Monad.Except (MonadError (..))
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Servant.API.Generic (ToServant)
import Servant.Server (ServantErr (..), err404)
import Servant.Server.Generic (AsServerT, genericServerT)
import Servant.Util.Dummy (paginate)
import Test.QuickCheck (arbitrary, vector, vectorOf)
import Loot.Log (MonadLogging)

import Agora.Arbitrary
import Agora.Types
import Agora.Web.API

type AgoraHandlers m = ToServant AgoraEndpoints (AsServerT m)

type AgoraHandlersMode m = (MonadLogging m, MonadError ServantErr m)

-- | Server handler implementation for Agora API.
agoraHandlers :: AgoraHandlersMode m => AgoraHandlers m
agoraHandlers = genericServerT AgoraEndpoints
  { aePeriod = \mPeriodNum ->
      view _1 <$> getPeriod (fromMaybe 20 mPeriodNum)

  , aeProposals = \periodNum pagination ->
      paginate pagination . view _2 <$> getPeriod periodNum

  , aeProposalVotes = \periodNum pagination ->
      paginate pagination . view _3 <$> getPeriod periodNum

  , aeBallots = \periodNum pagination ->
      paginate pagination . view _4 <$> getPeriod periodNum
  }

-- | All possible data about the period (for the mock)
type PeriodData = (PeriodInfo, [Proposal], [ProposalVote], [Ballot])

-- | Gets all period data, if a period exists, or fails with 404.
getPeriod :: MonadError ServantErr m => Word32 -> m PeriodData
getPeriod n = M.lookup n mockApiData `whenNothing` throwError noSuchPeriod
  where noSuchPeriod = err404
          { errBody = "Period with given number does not exist" }

-- | Mock data for API, randomly generated with a particular seed.
mockApiData :: Map Word32 PeriodData
mockApiData = M.fromList $ zipWith setNum [1..] periods
  where setNum i pd = (i, pd & _1.piPeriod.pNum .~ i)
        periods = detGen 42 $ vectorOf 20 $ (,,,)
          <$> arbitrary
          <*> vector 1000
          <*> vector 1000
          <*> vector 1000
