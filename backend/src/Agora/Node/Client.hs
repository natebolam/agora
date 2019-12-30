{-# LANGUAGE TypeOperators #-}

module Agora.Node.Client
       ( TezosClient (..)
       , MonadTezosClient (..)
       , TezosClientError (..)
       , tezosClient
       , withTezosClient
       ) where

import Control.Monad.Reader (withReaderT)
import Loot.Log (Logging)
import Monad.Capabilities (CapImpl (..), CapsT, HasCap, HasNoCap, addCap, makeCap)
import Network.HTTP.Client (newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Servant.API.Stream (SourceIO)
import Servant.Client (ClientError, mkClientEnv)
import Servant.Client.Generic (AsClientT, genericClientHoist)
import Servant.Client.Streaming ()
import Servant.Types.SourceT (foreach)
import Tezos.V005.Micheline (Expression)
import UnliftIO (MonadUnliftIO, withRunInIO)
import qualified UnliftIO as UIO

import Agora.Config
import qualified Agora.DB as DB
import Agora.Node.API
import Agora.Node.Types
import Agora.Types
import Agora.Util

data TezosClient m = TezosClient
  { _fetchBlock         :: ChainId -> BlockId -> m Block
  , _fetchBlockMetadata :: ChainId -> BlockId -> m BlockMetadata
  , _fetchBlockHead     :: ChainId -> BlockId -> m BlockHead
  , _headsStream        :: ChainId -> (BlockHead -> m ()) -> m ()
  , _getContractStorage :: ChainId -> BlockId -> ContractHash -> m (Maybe Expression)
  }

makeCap ''TezosClient

data TezosClientError
  = ParsingError !Text
  | TezosNodeError !ClientError
  | MytezosbakerError !ClientError
  deriving (Eq, Show, Generic)

instance Exception TezosClientError

-- | Implementation of TezosClient cap using servant-client
tezosClient
  :: (MonadUnliftIO m)
  => NodeEndpoints (AsClientT m)
  -> CapImpl TezosClient '[Logging] m
tezosClient NodeEndpoints{..} = CapImpl $ TezosClient
  { _fetchBlock = \chain -> \case
      LevelRef (Level 1) -> pure block1
      ref                -> lift $ neGetBlock chain ref

  , _fetchBlockMetadata = \chain -> \case
      LevelRef (Level 1) -> pure metadata1
      ref                -> lift $ neGetBlockMetadata chain ref

  , _fetchBlockHead = \chain -> \case
      LevelRef (Level 1) -> pure blockHead1
      ref                -> lift $ neGetBlockHead chain ref

  , _headsStream = \chain callback -> do
      stream <- lift $ neNewHeadStream chain
      onStreamItem stream callback

  , _getContractStorage = \chain blockId storage -> lift $
      UIO.catch (Just <$> neGetContractStorage chain blockId storage) (\(_ :: TezosClientError) -> pure Nothing)
  }

-- Taken from here https://haskell-servant.readthedocs.io/en/release-0.15/tutorial/Client.html#querying-streaming-apis
onStreamItem
  :: MonadUnliftIO m
  => SourceIO a -> (a -> m ()) -> m ()
onStreamItem stream onItem = withRunInIO $ \runM ->
  foreach fail (runM . onItem) stream

-- | Method for providing the tezos client capabilities
withTezosClient
  :: forall m caps a .
  ( HasNoCap TezosClient caps
  , HasAgoraConfig caps
  , HasCap Logging caps
  , HasCap DB.PostgresConn caps
  , MonadUnliftIO m
  )
  => CapsT (TezosClient ': caps) m a
  -> CapsT caps m a
withTezosClient caps = do
  nodeUrl <- fromAgoraConfig $ option #node_addr
  manager <- liftIO $ newManager tlsManagerSettings

  let nodeClientEnv = mkClientEnv manager nodeUrl
      nodeClient = genericClientHoist $
        hoistClientEnv TezosNodeError nodeClientEnv

  withReaderT (addCap $ tezosClient nodeClient) caps
