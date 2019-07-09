{-# LANGUAGE TypeOperators #-}

module Agora.Node.Client
       ( TezosClient (..)
       , MonadTezosClient (..)
       , TezosClientError (..)
       , tezosClient
       , withTezosClient
       ) where

import Control.Monad.Reader (withReaderT)
import Monad.Capabilities (CapImpl (..), CapsT, HasNoCap, addCap, makeCap)
import Network.HTTP.Client (defaultManagerSettings, newManager)
import Servant.API.Stream (ResultStream (..))
import Servant.Client (BaseUrl (..), ClientM, Scheme (..), ServantError, mkClientEnv, runClientM)
import Servant.Client.Generic (genericClientHoist)
import UnliftIO (MonadUnliftIO, throwIO, withRunInIO)

import Agora.Node.API (NodeEndpoints (..))
import Agora.Node.Types (Block (..), BlockHead, BlockId (..), BlockMetadata (..), ChainId,
                         Operations (..))
import Agora.Types
import Agora.Util (NetworkAddress (..))

data TezosClient m = TezosClient
  { _fetchBlock         :: ChainId -> BlockId -> m Block
  , _fetchBlockMetadata :: ChainId -> BlockId -> m BlockMetadata
  , _headsStream        :: ChainId -> (BlockHead -> m ()) -> m ()
  }

makeCap ''TezosClient

data TezosClientError
  = ParsingError !Text
  | TezosNodeError !ServantError
  deriving (Eq, Show, Generic)

instance Exception TezosClientError

-- | Implementation of TezosClient cap using servant-client
tezosClient
  :: MonadUnliftIO m
  => (forall a . ClientM a -> m a)
  -> CapImpl TezosClient '[] m
tezosClient hoist =
  let NodeEndpoints{..} = genericClientHoist hoist in
  CapImpl $ TezosClient
    { _fetchBlock         = \chain -> \case
        LevelRef (Level 1) -> pure block1
        ref                -> lift $ neGetBlock chain ref
    , _fetchBlockMetadata = \chain -> \case
        LevelRef (Level 1) -> pure metadata1
        ref                -> lift $ neGetBlockMetadata chain ref
    , _headsStream = \chain callback -> do
        stream <- lift $ neNewHeadStream chain
        onStreamItem stream $ \case
          Left e  -> throwIO $ ParsingError (fromString e)
          Right x -> callback x
    }
  where
    -- pva701: The reason of this hardcoding that
    -- chains/main/blocks/1 returns block,
    -- where "metadata" field doesn't contain "level" field
    -- I hope this exception is only for the first block
    -- We could just prefill a database with the first block
    -- but I found this hardcoding more robust, because we don't need to
    -- refill the database every time when we change its format.
    metadata1 = BlockMetadata
      { bmLevel = Level 1
      , bmCycle = Cycle 0
      , bmCyclePosition = 0
      , bmVotingPeriod = Id 0
      , bmVotingPeriodPosition = 0
      , bmVotingPeriodType = Proposing
      }
    block1 = Block
      { bHash = Hash $ encodeUtf8 ("BLSqrcLvFtqVCx8WSqkVJypW2kAVRM3eEj2BHgBsB6kb24NqYev" :: Text)
      , bOperations = Operations []
      , bMetadata = metadata1
      , bPredecessor = Hash $ encodeUtf8 ("BLockGenesisGenesisGenesisGenesisGenesisf79b5d1CoW2" :: Text)
      }

-- Taken from here https://haskell-servant.readthedocs.io/en/release-0.14/tutorial/Client.html#querying-streaming-apis
onStreamItem
  :: MonadUnliftIO m
  => ResultStream a -> (Either String a -> m ()) -> m ()
onStreamItem (ResultStream k) onItem = withRunInIO $ \runM ->
  k $ \getResult ->
    let loop = do
          r <- getResult
          case r of
            Nothing -> pure ()
            Just x  -> runM (onItem x) >> loop
    in loop

-- | Method for providing the tezos client capabilities
withTezosClient
  :: forall m caps a .
  ( HasNoCap TezosClient caps
  , MonadUnliftIO m
  )
  => NetworkAddress
  -> CapsT (TezosClient ': caps) m a
  -> CapsT caps m a
withTezosClient NetworkAddress{..} caps = do
  manager <- liftIO (newManager defaultManagerSettings)
  let clientEnv = mkClientEnv manager (BaseUrl Http (toString naHost) (fromIntegral naPort) "")
  let hoist :: forall x . ClientM x -> m x
      hoist clientM = liftIO $
        runClientM clientM clientEnv >>= \case
          Left e  -> throwIO $ TezosNodeError e
          Right x -> pure x
  withReaderT (addCap $ tezosClient hoist) caps
