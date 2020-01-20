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
import Servant.Client (ClientEnv, ClientError, mkClientEnv)
import Servant.Client.Generic (AsClientT, genericClient)
import Servant.Client.Streaming (ClientM)
import Tezos.V005.Micheline (Expression)
import UnliftIO (MonadUnliftIO)
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
  :: forall m. (MonadUnliftIO m)
  => ClientEnv
  -> NodeEndpoints (AsClientT ClientM)
  -> CapImpl TezosClient '[Logging] m
tezosClient env NodeEndpoints{..} = CapImpl $ TezosClient
  { _fetchBlock = \chain -> \case
      LevelRef (Level 1) -> pure block1
      ref                -> liftSimple $ neGetBlock chain ref

  , _fetchBlockMetadata = \chain -> \case
      LevelRef (Level 1) -> pure metadata1
      ref                -> liftSimple $ neGetBlockMetadata chain ref

  , _fetchBlockHead = \chain -> \case
      LevelRef (Level 1) -> pure blockHead1
      ref                -> liftSimple $ neGetBlockHead chain ref

  , _headsStream = \chain callback ->
      liftStreaming callback $ neNewHeadStream chain

  , _getContractStorage = \chain blockId storage ->
        liftSimple (Just <$> neGetContractStorage chain blockId storage)
      `UIO.catch`  (\(_ :: TezosClientError) -> pure Nothing)
  }
  where
    liftSimple :: ClientM x -> CapsT caps m x
    liftSimple = hoistClient TezosNodeError env

    liftStreaming :: (x -> CapsT caps m ()) -> ClientM (SourceIO x) -> CapsT caps m ()
    liftStreaming = hoistStreamingClient TezosNodeError env

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

  withReaderT (addCap $ tezosClient nodeClientEnv genericClient) caps
