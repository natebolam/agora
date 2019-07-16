{-# LANGUAGE OverloadedLabels #-}

{-|
Defines the monadic stack and parameters for testing monadic Agora code.
-}
module Agora.TestMode where

import Database.Beam.Postgres (close, connectPostgreSQL)
import Database.PostgreSQL.Simple.Transaction (IsolationLevel (..), ReadWriteMode (..),
                                               TransactionMode (..), beginMode, rollback)
import Lens.Micro.Platform ((?~))
import Loot.Log (Logging (..), NameSelector (..), basicConfig)
import Monad.Capabilities (CapImpl (..), Capabilities, CapsT, addCap, emptyCaps)
import System.Environment (lookupEnv)
import Test.Hspec (Spec, SpecWith, afterAll, beforeAll)
import Test.QuickCheck (Property, Testable, ioProperty)
import Test.QuickCheck.Monadic (PropertyM, monadic, stop)
import qualified UnliftIO as UIO

import Agora.Config
import Agora.DB
import Agora.Mode
import Agora.Node

-- | Configuration which is used in tests. Accepts a `ConnString`
-- which is determined at runtime.
testingConfig :: ConnString -> AgoraConfigRec
testingConfig connString = finaliseDeferredUnsafe $ mempty
  & option #logging ?~ basicConfig
  & sub #db . option #conn_string ?~ connString
  & sub #db . option #max_connections ?~ 1

-- | Logging for tests which does nothing.
testLogging :: Applicative m => CapImpl Logging '[] m
testLogging = CapImpl $ Logging
  { _log = \_ -> pure ()
  , _logName = pure CallstackName
  }

-- | Test tezos client which does nothing.
-- TODO: provide some mock implementation which does not crash.
testTezosClient :: Applicative m => CapImpl TezosClient '[] m
testTezosClient = CapImpl $ TezosClient
  { _getBlock = error "TezosClient for test is not yet implemented"
  , _getBlockMetadata = error "TezosClient for test is not yet implemented"
  , _headsStream = error "TezosClient for test is not yet implemented"
  }

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

-- | Method which constructs all necessary capabilities to satisfy
-- `AgoraWorkMode`.
makeTestCaps :: IO (Capabilities AgoraCaps IO)
makeTestCaps = do
  connString <- postgresTestServerConnString
  conn <- connectPostgreSQL $ unConnString connString
  let cfg = testingConfig connString
  let caps =
        addCap (postgresConnSingle conn) $
        addCap testTezosClient $
        addCap testLogging $
        addCap (newConfig cfg) emptyCaps
  usingReaderT caps $ runPg ensureSchemaIsSetUp
  pure caps

cleanupConnection :: Capabilities AgoraCaps IO -> IO ()
cleanupConnection caps = usingReaderT caps $
  withConnection $ liftIO . close

withTestCaps :: SpecWith (Capabilities AgoraCaps IO) -> Spec
withTestCaps = beforeAll makeTestCaps . afterAll cleanupConnection

agoraPropertyM
  :: Testable prop
  => PropertyM (CapsT AgoraCaps IO) prop
  -> Capabilities AgoraCaps IO
  -> Property
agoraPropertyM action caps =
  monadic (ioProperty . insideRollbackedTx caps) $
  void $ action >>= stop
  where
    insideRollbackedTx caps' act = usingReaderT caps' $
      withConnection $ \conn -> UIO.bracket_
                                (liftIO $ beginMode testTxMode conn)
                                (liftIO $ rollback conn)
                                act
    testTxMode = TransactionMode Serializable ReadWrite

