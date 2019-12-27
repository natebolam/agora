{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE OverloadedLabels #-}

{-|
Tezos constants capability.
-}
module Agora.Node.Constants
       ( TzConstants (..)
       , HasTzConstants
       , MonadTzConstants (..)
       , TzConstantsCap
       , newTzConstants
       , askOnePeriod
       , tzOnePeriod
       , isPeriodStart
       , isPeriodEnd
       , withTzConstants
       , withRealTzConstants

       , minRelevantLevel
       , defaultQuorum
       ) where

import Control.Monad.Reader (withReaderT)
import Monad.Capabilities (CapImpl, CapsT, Context (..), HasContext, HasNoCap, addCap, askContext,
                           newContext)

import Agora.Types

data TzConstants = TzConstants
  { tzEmptyPeriods :: !Stage
  -- ^ How many periods which don't contain any voting data in the chain
  , tzCycleLength  :: !Level
  -- ^ Length of one cycle
  , tzNumOfCycles  :: !Word32
  -- ^ Number of cycles in one period
  }

tzOnePeriod :: TzConstants -> Level
tzOnePeriod TzConstants{..} = fromIntegral tzNumOfCycles * tzCycleLength

type HasTzConstants caps = HasContext TzConstants caps
type TzConstantsCap = Context TzConstants

class Monad m => MonadTzConstants m where
  askTzConstants :: m TzConstants

newTzConstants :: TzConstants -> CapImpl (Context TzConstants) '[] m
newTzConstants = newContext

instance (HasTzConstants caps, Monad m) =>
         MonadTzConstants (CapsT caps m) where
  askTzConstants = askContext

-- | Return number of levels in one period
askOnePeriod :: MonadTzConstants m => m Level
askOnePeriod = tzOnePeriod <$> askTzConstants

-- | Return true if passed block corresponds to first level of a period.
isPeriodStart :: MonadTzConstants m => Level -> m Bool
isPeriodStart lev = do
  onePeriod <- askOnePeriod
  pure $ lev `mod` onePeriod == 1

-- | Return true if passed block corresponds to last level of a period.
isPeriodEnd :: MonadTzConstants m => Level -> m Bool
isPeriodEnd lev = do
  onePeriod <- askOnePeriod
  pure $ lev `mod` onePeriod == 0

-- | Method for providing the tezos constant capability
withTzConstants
  :: forall m caps a. (HasNoCap TzConstantsCap caps)
  => TzConstants
  -> CapsT (TzConstantsCap ': caps) m a
  -> CapsT caps m a
withTzConstants = withReaderT . addCap . newTzConstants

-- | Method for providing real values of tezos constant
withRealTzConstants
  :: forall m caps a. (HasNoCap TzConstantsCap caps)
  => CapsT (TzConstantsCap ': caps) m a
  -> CapsT caps m a
withRealTzConstants =
  withTzConstants $
    TzConstants
    { tzEmptyPeriods = Stage 10 -- since blocks with period [0..9] periods don't contain any voting information
    , tzCycleLength = Level 4096
    , tzNumOfCycles = 8
    }

---------------------------------------------------------------------------
-- Very constant (hard-coded) constants
---------------------------------------------------------------------------

-- | Minimum level for which Tezos node do not yield 404
-- on `getVoters` and `getQuorum` queries.
minRelevantLevel :: Level
minRelevantLevel = Level 204761

-- | Initial quorum value.
defaultQuorum :: Quorum
defaultQuorum = Quorum 8000
