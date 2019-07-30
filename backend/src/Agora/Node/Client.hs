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

import Agora.Node.API
import Agora.Node.Constants
import Agora.Node.Types
import Agora.Types
import Agora.Util

data TezosClient m = TezosClient
  { _fetchBlock         :: ChainId -> BlockId -> m Block
  , _fetchBlockMetadata :: ChainId -> BlockId -> m BlockMetadata
  , _fetchBlockHead     :: ChainId -> BlockId -> m BlockHead
  , _fetchVoters        :: ChainId -> BlockId -> m [Voter]
  , _fetchQuorum        :: ChainId -> BlockId -> m Quorum
  , _fetchCheckpoint    :: ChainId -> m Checkpoint
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
    , _fetchBlockHead = \chain -> \case
        LevelRef (Level 1) -> pure blockHead1
        ref                -> lift $ neGetBlockHead chain ref
    , _fetchVoters = \chain blockId ->
        case blockId of
          -- pva701: I've discovered that /votes endpoints
          -- return 404 Not found if a requested level is less than 204761
          -- So it is ok to return empty list in this case since
          -- there are no useful information for voting below 327680 level.
          LevelRef level
            | chain == MainChain && level < minRelevantLevel -> pure []
          _  -> lift $ neGetVoters chain blockId
    , _fetchQuorum = \chain blockId ->
        case blockId of
          -- pva701: see comment above
          LevelRef level
            | chain == MainChain && level < minRelevantLevel -> pure defaultQuorum
          _  -> lift $ neGetQuorum chain blockId
    , _fetchCheckpoint    = lift . neGetCheckpoint
    , _headsStream = \chain callback -> do
        stream <- lift $ neNewHeadStream chain
        onStreamItem stream $ \case
          Left e  -> throwIO $ ParsingError (fromString e)
          Right x -> callback x
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
