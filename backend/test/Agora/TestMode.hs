{-# LANGUAGE OverloadedLabels #-}

{-|
Defines the monadic stack and parameters for testing monadic Agora code.
-}
module Agora.TestMode where

import Control.Concurrent.STM.TBChan (newTBChan, tryWriteTBChan)
import Control.Monad.Reader (withReaderT)
import Data.Aeson (decode')
import Data.FileEmbed (embedStringFile)
import qualified Data.Vector as V
import Database.Beam.Postgres (close, connectPostgreSQL)
import Database.PostgreSQL.Simple.Transaction (IsolationLevel (..), ReadWriteMode (..),
                                               TransactionMode (..), beginMode, rollback)
import Lens.Micro.Platform ((?~))
import Loot.Log (LogConfig (..), Logging, NameSelector (..), Severity (..), basicConfig,
                 withLogging)
import Monad.Capabilities (CapImpl (..), CapsT, HasCap, HasNoCap, addCap, emptyCaps, localContext)
import Network.HTTP.Types (http20, status404)
import Servant.Client (BaseUrl (..), Scheme (..))
import Fmt (fmt, build)
import qualified Servant.Client as C
import Servant.Client.Generic (AsClientT)
import System.Environment (lookupEnv)
import Test.Hspec (Spec, SpecWith, afterAll, beforeAll)
import Test.QuickCheck (Testable)
import Test.QuickCheck.Monadic (PropertyM (..))
import UnliftIO (MonadUnliftIO)
import qualified UnliftIO as UIO

import Agora.BlockStack
import Agora.Config
import Agora.DB
import Agora.Discourse
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
  pure dbCap

cleanupDbCap :: DbCap -> IO ()
cleanupDbCap dbCap =
  usingReaderT emptyCaps $
  withReaderT (addCap dbCap) $ do
    runPg resetSchema
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
  -> (CapImpl TezosClient '[] IO, DiscourseEndpoints (AsClientT IO), BlockStackCapImpl IO)
  -- ^ these two caps are used by block sync worker
  -- which runs a separate thread, where caps can't be overrided
  -- during execution
  -> PropertyM (CapsT AgoraCaps IO) prop -- ^ testing action
  -> PropertyM IO prop
agoraPropertyM dbCap (tezosClientCap, discourseEndpoints, blockCap) (MkPropertyM unP) =
  MkPropertyM $ \call ->
    insideRollbackedTx <$> unP (fmap liftIO . call)
  where
    insideRollbackedTx :: CapsT AgoraCaps IO x -> IO x
    insideRollbackedTx act = do
      connString <- postgresTestServerConnString
      let configCap = newConfig $ testingConfig connString
      usingReaderT emptyCaps $
        withTzConstants testTzConstants $
        withReaderT (addCap configCap) $
        withLogging (LogConfig [] Debug) CallstackName $
        withReaderT (addCap dbCap) $
        withTestTezosClient tezosClientCap $
        withDiscourseClientImpl discourseEndpoints $
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

-- | Assign proposalId to 0
-- It's a hack to cope with the fact that we are running all tests
-- within one transaction and it's hard to compute actual id.
discardId :: Integral i => Traversal' a i -> a -> a
discardId l = set l 0

overrideEmptyPeriods
  :: PeriodId
  -> PropertyM (CapsT AgoraCaps IO) a
  -> PropertyM (CapsT AgoraCaps IO) a
overrideEmptyPeriods emptyPeriods (MkPropertyM unP) =
  MkPropertyM (fmap override . unP)
  where
    override = localContext (\x -> x {tzEmptyPeriods = emptyPeriods})

inmemoryClient
  :: Monad m
  => BlockChain
  -> CapImpl TezosClient '[] m
inmemoryClient bc = CapImpl $ inmemoryClientRaw bc

inmemoryClientRaw :: Monad m => BlockChain -> TezosClient m
inmemoryClientRaw bc = TezosClient
  { _fetchBlock         = \_ -> pure . getBlock bc
  , _fetchBlockMetadata = \_ -> pure . bMetadata . getBlock bc
  , _headsStream = \_ call -> V.forM_ (V.tail $ bcBlocksList bc) (call . block2Head)
  , _fetchBlockHead = \_ -> pure . block2Head . getBlock bc
  , _fetchVoters = \_ _ -> pure []
  , _fetchQuorum = \_ _ -> pure $ Quorum 8000
  , _fetchCheckpoint = \_ -> pure $ Checkpoint "archive"
  , _triggerBakersFetch = \_ -> pure ()
  }

fetcher2 :: MonadUnliftIO m => TezosClient m
fetcher2 = fix $ \this -> TezosClient
  { _fetchBlock = \_ -> \case
      LevelRef (Level 0) -> pure genesisBlock
      LevelRef (Level 1) -> pure block1
      LevelRef (Level 2) -> pure block2
      HeadRef            -> pure block2
      GenesisRef         -> pure genesisBlock
      _                  -> notFound
  , _fetchBlockMetadata = \_ _ -> error "not supposed to be called"
  , _headsStream = \_ _ -> error "not supposed to be called"
  , _fetchBlockHead = fmap block2Head ... _fetchBlock this
  , _fetchVoters = \_ _ -> pure []
  , _fetchQuorum = \_ _ -> pure $ Quorum 8000
  , _fetchCheckpoint = \_ -> pure $ Checkpoint "archive"
  , _triggerBakersFetch = \_ -> pure ()
  }

notFound :: MonadUnliftIO m => m a
notFound =
  UIO.throwIO $ TezosNodeError $ C.FailureResponse $ C.Response status404 mempty http20 mempty

notFoundServant :: MonadUnliftIO m => m a
notFoundServant =
  UIO.throwIO $ C.FailureResponse $ C.Response status404 mempty http20 mempty

inmemoryMytezosbakerEndpoints :: Monad m => MytezosbakerEndpoints (AsClientT m)
inmemoryMytezosbakerEndpoints = MytezosbakerEndpoints
  { mtzbBakers = pure testBakers
  }

withTestTezosClient
  :: forall m caps a .
  ( HasNoCap TezosClient caps
  , HasCap Logging caps
  , HasCap PostgresConn caps
  , MonadUnliftIO m
  )
  => CapImpl TezosClient '[] m
  -> CapsT (TezosClient ': caps) m a
  -> CapsT caps m a
withTestTezosClient (CapImpl tzClient) caps = do
  triggerChan <- UIO.atomically $ newTBChan 10
  let tzClient' :: CapImpl TezosClient '[] m
      tzClient' = CapImpl $ tzClient
        { _triggerBakersFetch = void . UIO.atomically . tryWriteTBChan triggerChan
        }
  UIO.withAsync (mytezosbakerWorker inmemoryMytezosbakerEndpoints triggerChan) $ \_ ->
    withReaderT (addCap tzClient') caps

inmemoryDiscourseEndpointsM :: MonadUnliftIO m => m (DiscourseEndpoints (AsClientT m))
inmemoryDiscourseEndpointsM = do
  topics <- UIO.newTVarIO ([] :: [TopicOnePost], 0 :: DiscoursePostId)
  let proposalsCategoryId = 100
  pure $ DiscourseEndpoints
    { dePostTopic = \_apiUsername _apiKey CreateTopic{..} -> do
        (tops, postsNum) <- UIO.readTVarIO topics
        let topicId = fromIntegral $ length tops
        let post = Post postsNum topicId (unRawBody ctRaw)
        let topic = MkTopic topicId ctTitle post
        UIO.atomically $ UIO.writeTVar topics (topic : tops, postsNum + 1)
        pure $ CreatedTopic postsNum topicId

    , deGetCategoryTopics = \categoryId mPage -> do
        (tops, _postsNum) <- UIO.readTVarIO topics
        if categoryId == proposalsCategoryId then do
          let page = fromMaybe 0 mPage
          let perPage = max 1 (length tops)
          if page == 0 then
            pure $ CategoryTopics perPage (map (\MkTopic{..} -> TopicHead tId tTitle) tops)
          else pure $ CategoryTopics perPage []
        else notFoundServant

    , deGetCategories = pure $ CategoryList [Category proposalsCategoryId testDiscourseCategory]

    , deGetTopic = \topicId -> do
        (tops, _postsNum) <- UIO.readTVarIO topics
        MkTopic{..} <- find ((topicId ==) . tId) tops `whenNothing` notFoundServant
        pure $ MkTopic tId tTitle (one tPosts)

    , deGetPost = \postId -> do
      (tops, _postsNum) <- UIO.readTVarIO topics
      MkTopic{..} <- find ((postId ==) . pId . tPosts) tops `whenNothing` notFoundServant
      pure tPosts
    }

emptyDiscourseEndpoints :: DiscourseEndpoints (AsClientT m)
emptyDiscourseEndpoints = DiscourseEndpoints
  { dePostTopic = error "dePostTopic isn't supposed to be called"
  , deGetCategoryTopics = error "deGetCategoryTopics isn't supposed to be called"
  , deGetCategories = error "deGetCategories isn't supposed to be called"
  , deGetTopic = error "deGetTopic isn't supposed to be called"
  , deGetPost = error "deGetPost isn't supposed to be called"
  }

testDiscourseCategory :: Text
testDiscourseCategory = "Proposals"

testDiscourseHost :: BaseUrl
testDiscourseHost = BaseUrl Https "tezos.discourse.group" 443 ""

testDiscourseHostText :: Text
testDiscourseHostText = fmt (build testDiscourseHost)

-- | Configuration which is used in tests. Accepts a `ConnString`
-- which is determined at runtime.
testingConfig :: ConnString -> AgoraConfigRec
testingConfig connString = finaliseDeferredUnsafe $ mempty
  & option #logging ?~ basicConfig
  & sub #db . option #conn_string ?~ connString
  & sub #db . option #max_connections ?~ 1
  & sub #discourse . option #host ?~ testDiscourseHost
  & sub #discourse . option #category ?~ testDiscourseCategory
  & option #predefined_bakers ?~
      [ BakerInfo "Foundation Baker 1" (encodeHash "tz3RDC3Jdn4j15J7bBHZd29EUee9gVB1CxD9")
      , BakerInfo  "Foundation Baker 2" (encodeHash "tz3bvNMQ95vfAYtG8193ymshqjSvmxiCUuR5")
      ]

-- | Test tezos client which does nothing.
emptyTezosClient :: Applicative m => CapImpl TezosClient '[] m
emptyTezosClient = CapImpl $ TezosClient
  { _fetchBlock = error "fetchBlock isn't supposed to be called"
  , _fetchBlockMetadata = error "fetchBlockMetadata isn't supposed to be called"
  , _headsStream = error "headStream isn't supposed to be called"
  , _fetchBlockHead = error "fetchBlockHead isn't supposed to be called"
  , _fetchVoters = error "fetchVoters isn't supposed to be called"
  , _fetchQuorum = error "fetchQuorum isn't supposed to be called"
  , _fetchCheckpoint = error "fetchCheckpoint isn't supposed to be called"
  , _triggerBakersFetch = error "triggerBakersFetch isn't supposed to be called"
  }

testBakers :: BakerInfoList
testBakers = fromMaybe (error "invalid bakers info file!") $
  decode' $(embedStringFile "resources/bakers_top10.json")
