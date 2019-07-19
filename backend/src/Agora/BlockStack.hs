{- Mock module, which will be repalced with actual db
when it's implemented within AG-25.
-}
{-# LANGUAGE TypeOperators #-}

module Agora.BlockStack
       ( MonadBlockStack (..)
       , BlockStack (..)
       , withBlockStack
       , blockStackCapOverDb
       , blockStackCapOverDbImpl
       ) where

import Control.Monad.Reader (withReaderT)
import Database.Beam.Query (val_, (<-.), (==.))
import qualified Database.Beam.Query as B
import Monad.Capabilities (CapImpl (..), CapsT, HasCap, HasNoCap, addCap, makeCap)
import UnliftIO (MonadUnliftIO)

import Agora.DB
import Agora.Node.Types
import Agora.Types

data BlockStack m = BlockStack
  { _getAdoptedHead :: m BlockHead
  , _applyBlock     :: Block -> m ()
  }

makeCap ''BlockStack

withBlockStack
  :: forall m caps a .
  ( HasCap PostgresConn caps
  , HasNoCap BlockStack caps
  , MonadUnliftIO m)
  => CapsT (BlockStack ': caps) m a
  -> CapsT caps m a
withBlockStack =
  withReaderT (addCap blockStackCapOverDbImpl)

blockStackCapOverDbImpl :: MonadUnliftIO m => CapImpl BlockStack '[PostgresConn] m
blockStackCapOverDbImpl = CapImpl blockStackCapOverDb

blockStackCapOverDb :: (MonadUnliftIO m, MonadPostgresConn m) => BlockStack m
blockStackCapOverDb = BlockStack
  { _getAdoptedHead = do
      bhMb <- runPg $ B.runSelectReturningOne $ B.select $ do
        mx <- B.aggregate_ (B.max_ . pmId) (B.all_ $ asPeriodMetas agoraSchema)
        x <- B.all_ (asPeriodMetas agoraSchema)
        B.guard_ (mx ==. B.just_ (pmId x))
        pure (pmLastBlockHash x, pmLastBlockLevel x, pmPrevBlockHash x)
      pure $ case bhMb of
        Nothing        -> genesisBlockHead
        Just (h, l, p) -> BlockHead h l p
  , _applyBlock = onBlock
  }

-- pva701: it's shorten version of 'onBlock'
-- snitched from AG-47 branch to satisfy tests
onBlock
  :: forall m .
  ( MonadPostgresConn m
  , MonadUnliftIO m
  )
  => Block
  -> m ()
onBlock Block{..} = do
  when (isPeriodStart bMetadata) $ transact $
    insertPeriodMetas (Quorum 8000) 0

  transact $
    updatePeriodMetas bmVotingPeriod 0
  where
    BlockMetadata{..} = bMetadata
    AgoraSchema{..} = agoraSchema
    startLevel = bmLevel - fromIntegral bmVotingPeriodPosition

    insertPeriodMetas q totVotes =
      runPg $ B.runInsert $
        B.insert asPeriodMetas $
        B.insertValues $ one $ PeriodMeta
          { pmId = bmVotingPeriod
          , pmType = bmVotingPeriodType
          , pmVotesCast = 0
          , pmVotesAvailable = totVotes
          , pmQuorum = q
          , pmCyclesPassed = 0
          , pmStartLevel = startLevel
          , pmEndLevel   = startLevel + onePeriod
          , pmLastBlockLevel = bmLevel
          , pmLastBlockHash = bHash
          , pmPrevBlockHash = bPredecessor
          , pmBallotsYay = 0
          , pmBallotsNay = 0
          , pmBallotsPass = 0
          }

    updatePeriodMetas :: PeriodId -> Votes -> m ()
    updatePeriodMetas curPer casted =
      runPg $ B.runUpdate $ B.update asPeriodMetas (\ln ->
        (pmCyclesPassed ln <-. val_ bmCycle) <>
        (pmLastBlockLevel ln <-. val_ bmLevel) <>
        (pmLastBlockHash ln <-. val_ bHash) <>
        (pmPrevBlockHash ln <-. val_ bPredecessor) <>
        (pmVotesCast ln <-. B.current_ (pmVotesCast ln) + val_ casted))
        (\ln -> pmId ln ==. val_ curPer)
