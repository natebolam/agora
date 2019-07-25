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
       , withTzConstants
       , withRealTzConstants
       ) where

import Control.Monad.Reader (withReaderT)
import Monad.Capabilities (CapImpl, CapsT, Context (..), HasContext, HasNoCap, addCap, askContext,
                           newContext)

import Agora.Types
import Agora.Node.Types

data TzConstants = TzConstants
  { tzEmptyPeriods :: !PeriodId
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
isPeriodStart :: MonadTzConstants m => BlockMetadata -> m Bool
isPeriodStart BlockMetadata{..} = do
  onePeriod <- askOnePeriod
  pure $ bmLevel `mod` onePeriod == 1

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
    { tzEmptyPeriods = Id 10 -- since blocks with period [0..9] periods don't contain any voting information
    , tzCycleLength = Level 4096
    , tzNumOfCycles = 8
    }
