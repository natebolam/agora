{-# LANGUAGE OverloadedLabels #-}

{-|
Defines the monadic stack and parameters for testing monadic Agora code.
-}
module Agora.TestMode where

import Prelude hiding (ByteString)
import Control.Monad.Reader (withReaderT)
import Data.Aeson (decode')
import Data.FileEmbed (embedStringFile)
import qualified Data.Vector as V
import Lens.Micro.Platform ((?~))
import Loot.Log (LogConfig (..), NameSelector (..), Severity (..), basicConfig, withLogging)
import Monad.Capabilities (CapImpl (..), CapsT, addCap, emptyCaps, localContext)
import Servant.Client (BaseUrl (..), Scheme (..))
import Servant.Client.Generic (AsClientT)
import System.Environment (lookupEnv)
import Test.Hspec (Spec, SpecWith, afterAll, beforeAll)
import Test.QuickCheck (Testable)
import Test.QuickCheck.Monadic (PropertyM (..))
import UnliftIO (MonadUnliftIO)
import qualified UnliftIO as UIO
import qualified UnliftIO.Concurrent as UIO

import Agora.BlockStack
import Agora.Config
import Agora.DB
import Agora.Discourse
import Agora.Mode
import Agora.Node hiding (source)
import Agora.Types


import Agora.Node.Blockchain
import Servant.Server (Application)
import Network.Wai.Handler.Warp (Port, setPort, defaultSettings, runSettingsSocket)
import Network.Socket (Socket, socket, SocketType(..), defaultProtocol, tupleToHostAddress, bind, defaultPort, listen, socketPort)
import Control.Concurrent (ThreadId, forkIO, killThread)
import Network.Socket.Internal (Family(..), SockAddr(..))
import Servant.Server.Generic (AsServer, genericServe)
import Servant.Server.Internal.ServerError (errBody, err404, ServerError)
import Control.Monad.Except (throwError)
import Servant.Types.SourceT (source)
import Data.ByteString.Lazy.Internal (ByteString)

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

postgresTestDbConnections :: Int
postgresTestDbConnections = 200

type DbRes = (CapImpl PostgresConn '[] IO, ConnPool)
  
data WaiUrls = WaiUrls
  { nodeUrl :: BaseUrl
  , discourseUrl :: BaseUrl
  , mytezosbakerUrl :: BaseUrl
  }

setUpDbSchema :: DbRes -> IO DbRes
setUpDbSchema r@(dbCap, _pool) = do
  usingReaderT emptyCaps $
    withReaderT (addCap dbCap) $
      runPg ensureSchemaIsSetUp
  pure r

-- | Method which constructs all necessary capabilities which are not
-- changed during tests
makeDbRes :: IO DbRes
makeDbRes = do
  connString <- postgresTestServerConnString
  pool <- createConnPool connString postgresTestDbConnections
  let dbCap = postgresConnPooled pool
  pure (dbCap, pool)

resetDbSchema :: DbRes -> IO ()
resetDbSchema (dbCap, _pool) =
  usingReaderT emptyCaps $
    withReaderT (addCap dbCap) $
      runPg resetSchema

cleanupDbRes :: DbRes -> IO ()
cleanupDbRes (_dbCap, pool) = destroyConnPool pool

withDbResAll :: SpecWith DbRes -> Spec
withDbResAll =
  beforeAll (setUpDbSchema =<< makeDbRes) .
  afterAll (resetDbSchema <> cleanupDbRes)

startWaiApp :: Application -> IO (ThreadId, BaseUrl)
startWaiApp app = do
  (port, opendSocket) <- openTestSocket
  let settings = setPort port defaultSettings
  thread <- forkIO $ runSettingsSocket settings opendSocket app
  return (thread, BaseUrl Http "localhost" port "")

openTestSocket :: IO (Port, Socket)
openTestSocket = do
  s <- socket AF_INET Stream defaultProtocol
  let localhost = tupleToHostAddress (127, 0, 0, 1)
  bind s (SockAddrInet defaultPort localhost)
  listen s 1
  port <- socketPort s
  return (fromIntegral port, s)

withWaiApps ::
     Testable prop
  => NodeEndpoints AsServer
  -> DiscourseEndpoints AsServer
  -> (WaiUrls -> PropertyM IO prop)
  -> PropertyM IO prop
withWaiApps node discourse action = do
  (nodeThread, nodeUrl) <- lift $ startWaiApp $ genericServe node
  (mytezosbakerThread, mytezosbakerUrl) <- lift $ startWaiApp $ genericServe inmemoryMytezosbakerEndpoints
  (discourseThread, discourseUrl) <- lift $ startWaiApp $ genericServe discourse
  actionRes <- action WaiUrls {..}
  lift $ killThread nodeThread
  lift $ killThread mytezosbakerThread
  lift $ killThread discourseThread
  pure actionRes

instance (Monad m, MonadBlockStack m) => MonadBlockStack (PropertyM m) where
  getAdoptedHead = lift getAdoptedHead
  applyBlock = lift . applyBlock

agoraPropertyM
  :: Testable prop
  => DbRes  -- ^ db resources
  -> WaiUrls
  -> BlockStackCapImpl IO
  -- ^ these two caps are used by block sync worker
  -- which runs a separate thread, where caps can't be overrided
  -- during execution
  -> PropertyM (CapsT AgoraCaps IO) prop -- ^ testing action
  -> PropertyM IO prop
agoraPropertyM resc@(dbCap, _pool) urls blockCap (MkPropertyM unP) =
  MkPropertyM $ \call ->
    runAction <$> unP (fmap liftIO . call)
  where
    runAction :: CapsT AgoraCaps IO x -> IO x
    runAction act = do
      connString <- postgresTestServerConnString
      let configCap = newConfig $ testingConfig connString urls
      usingReaderT emptyCaps $
        withTzConstants testTzConstants $
        withReaderT (addCap configCap) $
        withLogging (LogConfig [] Debug) CallstackName $
        withReaderT (addCap dbCap) $
        withTezosClient $
        withDiscourseClient $
        withReaderT (addCap blockCap) $
          UIO.bracket_
            (void $ liftIO $ setUpDbSchema resc)
            (liftIO $ resetDbSchema resc)
            act

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

inmemoryConstantClientRaw :: BlockChain -> NodeEndpoints AsServer
inmemoryConstantClientRaw bc = NodeEndpoints
  { neGetBlock         = \_ -> pure . getBlock bc
  , neGetBlockMetadata = \_ -> pure . bMetadata . getBlock bc
  , neNewHeadStream = \_ -> pure $ source $ V.toList $ V.map block2HeadSafe (V.tail $ bcBlocksList bc)
  , neGetBlockHead = \_ rf -> pure $ block2HeadSafe $ getBlock bc rf
  , neGetVoters = \_ _ -> pure []
  , neGetQuorum = \_ _ -> pure $ Quorum 8000
  , neGetCheckpoint = \_ -> pure $ Checkpoint "archive"
  }

inmemoryStreamingClient :: MonadUnliftIO m => m (UIO.TChan (Either Block BlockChain), NodeEndpoints AsServer)
inmemoryStreamingClient = do
  chan <- UIO.newTChanIO
  blockchainVar <- UIO.newTVarIO genesisBlockChain
  let streamingTezosClient = fix $ \this -> NodeEndpoints
        { neGetBlock = \_ blockId -> do
            blockchain <- UIO.atomically $ UIO.readTVar blockchainVar
            pure $ getBlock blockchain blockId
        , neGetBlockMetadata = fmap bMetadata ... neGetBlock this
        , neNewHeadStream = \_ -> forever $
            UIO.atomically (UIO.readTChan chan) >>= \case
              Left block -> do
                UIO.atomically $ UIO.modifyTVar blockchainVar (appendBlock block)
                pure $ block2HeadSafe block
              Right chain -> do
                UIO.atomically $ UIO.writeTVar blockchainVar chain
                pure $ block2HeadSafe $ bcHead chain
        , neGetBlockHead = fmap block2HeadSafe ... neGetBlock this
        , neGetVoters = \_ _ -> pure []
        , neGetQuorum = \_ _ -> pure $ Quorum 8000
        , neGetCheckpoint = \_ -> pure $ Checkpoint "archive"
        }
  pure (chan, streamingTezosClient)

emitBlock :: MonadUnliftIO m => Block -> UIO.TChan (Either Block BlockChain) -> m ()
emitBlock block tchan = UIO.atomically $ UIO.writeTChan tchan (Left block)

wait :: MonadUnliftIO m => m ()
wait = UIO.threadDelay 100000 -- sleep for 100 ms

waitThenEmitBlock :: MonadUnliftIO m => Block -> UIO.TChan (Either Block BlockChain) -> m ()
waitThenEmitBlock block tchan = wait >> emitBlock block tchan

waitThenRewriteChain :: MonadUnliftIO m => BlockChain -> UIO.TChan (Either Block BlockChain) -> m ()
waitThenRewriteChain newChain tchan = do
  wait
  UIO.atomically $ UIO.writeTChan tchan (Right newChain)

fetcher2 :: NodeEndpoints AsServer
fetcher2 = fix $ \this -> NodeEndpoints
  { neGetBlock = \_ -> \case
      LevelRef (Level 0) -> pure genesisBlock
      LevelRef (Level 1) -> pure block1
      LevelRef (Level 2) -> pure block2
      HeadRef            -> pure block2
      GenesisRef         -> pure genesisBlock
      _                  -> throwError $ notFound "No such block_id type"
  , neGetBlockMetadata = \_ _ -> error "fetchBlockMetadata is not supposed to be called"
  , neNewHeadStream = \_ -> pure $ source $ map block2HeadSafe [block1, block2] 
  , neGetBlockHead = fmap block2Head ... neGetBlock this
  , neGetVoters = \_ _ -> pure []
  , neGetQuorum = \_ _ -> pure $ Quorum 8000
  , neGetCheckpoint = \_ -> pure $ Checkpoint "archive"
  }

notFound :: ByteString -> ServerError
notFound s = err404 {errBody = s} -- UIO.throwIO $ TezosNodeError $ C.FailureResponse $ C.Response status404 mempty http20 mempty

inmemoryMytezosbakerEndpoints :: MytezosbakerEndpoints AsServer
inmemoryMytezosbakerEndpoints = MytezosbakerEndpoints
  { mtzbBakers = pure testBakers
  }

inmemoryDiscourseEndpointsM :: MonadUnliftIO m => m (DiscourseEndpoints AsServer)
inmemoryDiscourseEndpointsM = do
  topics <- UIO.newTVarIO ([] :: [TopicOnePost], 0 :: DiscoursePostId)
  let proposalsCategoryId = 100
  pure $
    DiscourseEndpoints
      { dePostTopic =
          \_apiUsername _apiKey CreateTopic {..} -> do
            (tops, postsNum) <- UIO.readTVarIO topics
            let topicId = fromIntegral $ length tops
            let post = Post postsNum topicId (unRawBody ctRaw)
            let topic = MkTopic topicId ctTitle post
            UIO.atomically $ UIO.writeTVar topics (topic : tops, postsNum + 1)
            pure $ CreatedTopic postsNum topicId
      , deGetCategoryTopics =
          \categoryId mPage -> do
            (tops, _postsNum) <- UIO.readTVarIO topics
            if categoryId == proposalsCategoryId
              then do
                let page = fromMaybe 0 mPage
                let perPage = max 1 (length tops)
                if page == 0
                  then pure $ CategoryTopics perPage (map (\MkTopic {..} -> TopicHead tId tTitle) tops)
                  else pure $ CategoryTopics perPage []
              else throwError $ notFound "Servant not found"
      , deGetCategories = pure $ CategoryList [Category proposalsCategoryId testDiscourseCategory]
      , deGetTopic =
          \topicId -> do
            (tops, _postsNum) <- UIO.readTVarIO topics
            MkTopic {..} <- find ((topicId ==) . tId) tops `whenNothing` throwError (notFound "Servant not found")
            pure $ MkTopic tId tTitle (one tPosts)
      , deGetPost =
          \postId -> do
            (tops, _postsNum) <- UIO.readTVarIO topics
            MkTopic {..} <- find ((postId ==) . pId . tPosts) tops `whenNothing` throwError (notFound "Servant not found")
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

-- | Configuration which is used in tests. Accepts a `ConnString`
-- which is determined at runtime.
testingConfig :: ConnString -> WaiUrls -> AgoraConfigRec
testingConfig connString WaiUrls{..} = finaliseDeferredUnsafe $ mempty
  & option #logging ?~ basicConfig
  & option #node_addr ?~ nodeUrl
  & option #mytezosbaker_url ?~ mytezosbakerUrl
  & sub #db . option #conn_string ?~ connString
  & sub #db . option #max_connections ?~ postgresTestDbConnections
  & sub #discourse . option #host ?~ discourseUrl
  & sub #discourse . option #category ?~ testDiscourseCategory
  & option #predefined_bakers ?~
      [ BakerInfo "Foundation Baker 1" (encodeHash "tz3RDC3Jdn4j15J7bBHZd29EUee9gVB1CxD9") Nothing Nothing
      , BakerInfo  "Foundation Baker 2" (encodeHash "tz3bvNMQ95vfAYtG8193ymshqjSvmxiCUuR5") Nothing Nothing
      ]

-- | Test tezos client which does nothing.
emptyTezosClient :: NodeEndpoints AsServer
emptyTezosClient = NodeEndpoints
  { neGetBlock = error "fetchBlock isn't supposed to be called"
  , neGetBlockMetadata = error "fetchBlockMetadata isn't supposed to be called"
  , neNewHeadStream = error "headStream isn't supposed to be called"
  , neGetBlockHead = error "fetchBlockHead isn't supposed to be called"
  , neGetVoters = error "fetchVoters isn't supposed to be called"
  , neGetQuorum = error "fetchQuorum isn't supposed to be called"
  , neGetCheckpoint = error "fetchCheckpoint isn't supposed to be called"
  }

testBakers :: BakerInfoList
testBakers = fromMaybe (error "invalid bakers info file!") $
  decode' $(embedStringFile "resources/bakers_top10.json")
