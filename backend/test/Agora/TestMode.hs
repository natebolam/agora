{-# LANGUAGE OverloadedLabels #-}

{-|
Defines the monadic stack and parameters for testing monadic Agora code.
-}
module Agora.TestMode where

import Control.Monad.Reader (withReaderT)
import qualified Data.Vector as V
import Database.Beam.Postgres (close, connectPostgreSQL)
import Database.PostgreSQL.Simple.Transaction (IsolationLevel (..), ReadWriteMode (..),
                                               TransactionMode (..), beginMode, rollback)
import Lens.Micro.Platform ((?~))
import Loot.Log (LogConfig (..), NameSelector (..), Severity (..), basicConfig, withLogging)
import Monad.Capabilities (CapImpl (..), CapsT, addCap, emptyCaps)
import Network.HTTP.Types (http20, status404)
import qualified Servant.Client as C
import System.Environment (lookupEnv)
import Test.Hspec (Spec, SpecWith, afterAll, beforeAll)
import Test.QuickCheck (Testable)
import Test.QuickCheck.Monadic (PropertyM (..))
import UnliftIO (MonadUnliftIO)
import qualified UnliftIO as UIO

import Agora.BlockStack
import Agora.Config
import Agora.DB
import Agora.Mode
import Agora.Node
import Agora.Types

import Agora.Node.Blockchain

-- | Env variable from which `pg_tmp` temp server connection string
-- is read.
postgresTestServerEnvName :: String
postgresTestServerEnvName = "TEST_PG_CONN_STRING"

-- | Action which reads tmp server connection string from the env var.
postgresTestServerConnString :: IO ConnString
postgresTestServerConnString = lookupEnv postgresTestServerEnvName >>= \case
  Nothing -> error $ "Connection string for test server is not provided. \
                     \Pass it via " <> show postgresTestServerEnvName <>
                     " environmental variable."
  Just res -> do
    when (null res) $
      putTextLn "Warning: empty connection string to postgres server specified"
    pure $ ConnString $ encodeUtf8 res

type DbCap = CapImpl PostgresConn '[] IO

-- | Method which constructs all necessary capabilities which are not
-- changed during tests
makeDbCap :: IO DbCap
makeDbCap = do
  connString <- postgresTestServerConnString
  conn <- connectPostgreSQL $ unConnString connString
  let dbCap = postgresConnSingle conn
  usingReaderT emptyCaps $ withReaderT (addCap dbCap) $ runPg ensureSchemaIsSetUp
  pure $ postgresConnSingle conn

cleanupDbCap :: DbCap -> IO ()
cleanupDbCap dbCap =
  usingReaderT emptyCaps $
  withReaderT (addCap dbCap) $
  withConnection $ liftIO . close

withDbCapAll :: SpecWith DbCap -> Spec
withDbCapAll = beforeAll makeDbCap . afterAll cleanupDbCap

instance (Monad m, MonadBlockStack m) => MonadBlockStack (PropertyM m) where
  getAdoptedHead = lift getAdoptedHead
  applyBlock = lift . applyBlock

instance (Monad m, MonadSyncWorker m) => MonadSyncWorker (PropertyM m) where
  pushHeadWait = lift . pushHeadWait
  pushHead = lift . pushHead

agoraPropertyM
  :: Testable prop
  => DbCap  -- ^ db cap
  -> (CapImpl TezosClient '[] IO, CapImpl BlockStack '[PostgresConn] IO)
  -- ^ these two caps are used by block sync worker
  -- which runs a separate thread, where caps can't be overrided
  -- during execution
  -> PropertyM (CapsT AgoraCaps IO) prop -- ^ testing action
  -> PropertyM IO prop
agoraPropertyM dbCap (clientCap, blockCap) (MkPropertyM unP) =
  MkPropertyM $ \call ->
    insideRollbackedTx <$> unP (\a -> liftIO <$> call a)
  where
    insideRollbackedTx :: CapsT AgoraCaps IO x -> IO x
    insideRollbackedTx act = do
      connString <- postgresTestServerConnString
      let configCap = newConfig $ testingConfig connString
      usingReaderT emptyCaps $
        withReaderT (addCap configCap) $
        withLogging (LogConfig [] Debug) CallstackName $
        withReaderT (addCap clientCap) $
        withReaderT (addCap dbCap) $
        withReaderT (addCap blockCap) $
        withSyncWorker $
          withConnection $
            \conn -> UIO.bracket_
                          (liftIO $ beginMode testTxMode conn)
                          (liftIO $ rollback conn)
                          act
    testTxMode = TransactionMode Serializable ReadWrite

-----------------------------------
-- Useful helpers to run tests
-----------------------------------

inmemoryClient
  :: Monad m
  => BlockChain
  -> CapImpl TezosClient '[] m
inmemoryClient bc = CapImpl $ TezosClient
  { _fetchBlock         = \_ -> pure . getBlock bc
  , _fetchBlockMetadata = \_ -> pure . bMetadata . getBlock bc
  , _headsStream = \_ call -> V.forM_ (V.tail $ bcBlocksList bc) (call . block2Head)
  }

fetcher1 :: MonadUnliftIO m => TezosClient m
fetcher1 = TezosClient
  { _fetchBlock = \_ -> \case
      LevelRef (Level 0) -> pure genesisBlock
      LevelRef (Level 1) -> pure block1
      HeadRef            -> pure block1
      GenesisRef         -> pure genesisBlock
      _                  -> notFound
  , _fetchBlockMetadata = \_ _ -> error "not supposed to be called"
  , _headsStream = \_ _ -> error "not supposed to be called"
  }

notFound :: MonadUnliftIO m => m a
notFound =
  UIO.throwIO $ TezosNodeError $ C.FailureResponse $ C.Response status404 mempty http20 mempty

-- | Configuration which is used in tests. Accepts a `ConnString`
-- which is determined at runtime.
testingConfig :: ConnString -> AgoraConfigRec
testingConfig connString = finaliseDeferredUnsafe $ mempty
  & option #logging ?~ basicConfig
  & sub #db . option #conn_string ?~ connString
  & sub #db . option #max_connections ?~ 1

-- | Test tezos client which does nothing.
emptyTezosClient :: Applicative m => CapImpl TezosClient '[] m
emptyTezosClient = CapImpl $ TezosClient
  { _fetchBlock = error "fetchBlock isn't supposed to be called"
  , _fetchBlockMetadata = error "fetchBlockMetadata isn't supposed to be called"
  , _headsStream = error "headStream isn't supposed to be called"
  }
