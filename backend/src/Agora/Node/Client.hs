{-# LANGUAGE TypeOperators #-}

module Agora.Node.Client
       ( TezosClient (..)
       , MonadTezosClient (..)
       , TezosClientError (..)
       , tezosClient
       , withTezosClient
       , mytezosbakerWorker
       ) where

import Control.Concurrent.STM.TBChan (TBChan, newTBChan, readTBChan, tryWriteTBChan)
import Control.Monad.Reader (withReaderT)
import qualified Data.Set as S
import qualified Database.Beam.Postgres.Full as Pg
import Database.Beam.Query (insertValues)
import Database.Beam.Schema (primaryKey)
import Fmt ((+|), (|+))
import Loot.Log (Logging, MonadLogging, logDebug, logWarning)
import Monad.Capabilities (CapImpl (..), CapsT, HasCap, HasNoCap, addCap, makeCap)
import Network.HTTP.Client (newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Servant.API.Stream (ResultStream (..))
import Servant.Client (ServantError, mkClientEnv)
import Servant.Client.Generic (AsClientT, genericClientHoist)
import UnliftIO (MonadUnliftIO, withRunInIO)
import qualified UnliftIO as UIO

import Agora.Config
import qualified Agora.DB as DB
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
  , _triggerBakersFetch :: S.Set PublicKeyHash -> m ()
  , _headsStream        :: ChainId -> (BlockHead -> m ()) -> m ()
  }

makeCap ''TezosClient

data TezosClientError
  = ParsingError !Text
  | TezosNodeError !ServantError
  | MytezosbakerError !ServantError
  deriving (Eq, Show, Generic)

instance Exception TezosClientError

-- | Implementation of TezosClient cap using servant-client
tezosClient
  :: (MonadUnliftIO m)
  => NodeEndpoints (AsClientT m)
  -> TBChan (S.Set PublicKeyHash)
  -> CapImpl TezosClient '[Logging] m
tezosClient NodeEndpoints{..} mtzbFetchChan = CapImpl $ TezosClient
  { _fetchBlock = \chain -> \case
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

  , _fetchCheckpoint = lift . neGetCheckpoint

  , _triggerBakersFetch = \votersPbkhs -> do
      success <- UIO.atomically $ tryWriteTBChan mtzbFetchChan votersPbkhs
      if success
        then logDebug "Task to fetch bakers info is added to Mytezosbaker worker queue"
        else logWarning "Task to fetch bakers info is NOT added to Mytezosbaker worker queue"

  , _headsStream = \chain callback -> do
      stream <- lift $ neNewHeadStream chain
      onStreamItem stream $ \case
        Left e  -> UIO.throwIO $ ParsingError (fromString e)
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
  , HasAgoraConfig caps
  , HasCap Logging caps
  , HasCap DB.PostgresConn caps
  , MonadUnliftIO m
  )
  => CapsT (TezosClient ': caps) m a
  -> CapsT caps m a
withTezosClient caps = do
  nodeUrl <- fromAgoraConfig $ option #node_addr
  mytezosbakerUrl <- fromAgoraConfig $ option #mytezosbaker_url
  manager <- liftIO $ newManager tlsManagerSettings

  let nodeClientEnv = mkClientEnv manager nodeUrl
      mytezosbakerClientEnv = mkClientEnv manager mytezosbakerUrl
      nodeClient = genericClientHoist $
        hoistClientEnv TezosNodeError nodeClientEnv
      mytezosbakerClient = genericClientHoist $
        hoistClientEnv MytezosbakerError mytezosbakerClientEnv

  triggerChan <- UIO.atomically $ newTBChan 10
  UIO.withAsync (mytezosbakerWorker mytezosbakerClient triggerChan) $ \_ ->
    withReaderT (addCap $ tezosClient nodeClient triggerChan) caps

mytezosbakerWorker
  :: (MonadUnliftIO m, MonadLogging m, DB.MonadPostgresConn m)
  => MytezosbakerEndpoints (AsClientT m)
  -> TBChan (S.Set PublicKeyHash)
  -> m ()
mytezosbakerWorker MytezosbakerEndpoints{..} triggerChan = forever $ do
  votersPkhSet <- UIO.atomically $ readTBChan triggerChan
  let retryIn = 30  -- retry in 30 seconds
      retryInInt = fromIntegral retryIn :: Int
      reportError e = logWarning $ "Error "+|displayException e|+
        "happened in Mytezosbaker fetcher worker. Retrying in "+|retryInInt|+" seconds."
  suppressException @SomeException retryIn reportError $ do
    bakers <- bilBakers <$> mtzbBakers

    -- Mytezosbaker API do not currently provide any info regarding logos,
    -- so we ignore them for now.
    let bakerToVoter BakerInfo {..} = DB.Voter biDelegationCode (Just biBakerName) Nothing (Rolls 0) (DB.PeriodMetaId 0)
        bakerVoters =
          filter (\v -> DB.voterPbkHash v `S.member` votersPkhSet) $
          ordNubBy DB.voterPbkHash $
          map bakerToVoter bakers

    -- This is the simplest way to perform batch update, apparently
    DB.runInsert' $ Pg.insert (DB.asVoters DB.agoraSchema) (insertValues bakerVoters) $
      Pg.onConflict (Pg.conflictingFields primaryKey) $
      Pg.onConflictUpdateInstead (\ln -> (DB.voterName ln, DB.voterLogoUrl ln))
