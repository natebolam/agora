{-# LANGUAGE TypeOperators #-}

module Agora.Discourse.Client
       ( DiscourseClient (..)
       , MonadDiscourseClient (..)
       , withDiscourseClient

       -- * For tests
       , withDiscourseClientImpl
       , initProposalDiscourseFields
       , DiscourseError (..)
       ) where

import Control.Concurrent.STM.TBChan (TBChan, newTBChan, readTBChan, tryWriteTBChan)
import Control.Monad.Reader (withReaderT)
import qualified Data.Text as T
import Data.Time.Units (Second, toMicroseconds)
import Database.Beam.Query (all_, select, update, val_, (<-.), (==.))
import Fmt (listF, (+|), (|+))
import Loot.Log (Logging, logDebug, logError, logInfo, logWarning)
import Monad.Capabilities (CapImpl (..), CapsT, HasCap, HasNoCap, addCap, makeCap)
import Network.HTTP.Client (newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Servant.Client (ClientError, mkClientEnv)
import Servant.Client.Generic (AsClientT, genericClientHoist)
import UnliftIO (MonadUnliftIO)
import qualified UnliftIO as UIO
import qualified UnliftIO.Concurrent as UIO

import Agora.Config
import Agora.DB
import Agora.Discourse.API
import Agora.Discourse.Html
import Agora.Discourse.Types
import Agora.Types
import Agora.Util

data DiscourseClient m = DiscourseClient
  { _postProposalStubAsync :: ProposalHash -> m ()
  , _getProposalTopic      :: Text  -> m (Maybe TopicOnePost)
  }

makeCap ''DiscourseClient

data DiscourseError
  = DiscourseApiError !ClientError
  | CategoryNotFound !Text
  | TopicWithoutPosts !DiscourseTopicId !Title
  deriving (Eq, Show, Generic)

instance Exception DiscourseError

withDiscourseClient
  :: forall m caps a .
  ( HasNoCap DiscourseClient caps
  , HasAgoraConfig caps
  , HasCap Logging caps
  , HasCap PostgresConn caps
  , MonadUnliftIO m
  )
  => CapsT (DiscourseClient ': caps) m a
  -> CapsT caps m a
withDiscourseClient action = do
  manager <- liftIO (newManager tlsManagerSettings)
  host <- fromAgoraConfig $ sub #discourse . option #host
  let clientEnv = mkClientEnv manager host
      client = genericClientHoist $ hoistClientEnv DiscourseApiError clientEnv
  withDiscourseClientImpl client action

withDiscourseClientImpl
  :: forall m caps a .
  ( HasNoCap DiscourseClient caps
  , HasAgoraConfig caps
  , HasCap Logging caps
  , HasCap PostgresConn caps
  , MonadUnliftIO m
  )
  => DiscourseEndpoints (AsClientT m)
  -> CapsT (DiscourseClient ': caps) m a
  -> CapsT caps m a
withDiscourseClientImpl discourseEndpoints action = do
  categoryName <- fromAgoraConfig $ sub #discourse . option #category
  CategoryList categories <- lift (deGetCategories discourseEndpoints)
  Category{..} <- find ((categoryName ==) . cName) categories
    `whenNothing` UIO.throwIO (CategoryNotFound categoryName)

  chan <- UIO.atomically $ newTBChan 100
  UIO.withAsync (workerPoster discourseEndpoints chan cId) $ \_ ->
    UIO.withAsync (workerFetcher discourseEndpoints 60) $ \_ ->
      withReaderT (addCap $ discourseClient discourseEndpoints chan cId) action

discourseClient
  :: forall m . MonadUnliftIO m
  => DiscourseEndpoints (AsClientT m)
  -> TBChan ProposalHash
  -> DiscourseCategoryId
  -> CapImpl DiscourseClient '[AgoraConfigCap, Logging] m
discourseClient DiscourseEndpoints{..} chan catId = CapImpl $ DiscourseClient
  { _postProposalStubAsync = \ph -> do
      success <- UIO.atomically $ tryWriteTBChan chan ph
      if success then
        logDebug $ "Task to create a stub topic for " +| shortenHash ph |+ " is added to the Discourse worker queue"
      else
        logWarning $ "Task to create a stub topic for " +| shortenHash ph |+ " is NOT added to the Discourse worker queue"
  , _getProposalTopic = \shorten -> lift $ traverse convertTopic =<< traversePages shorten 0
  }
  where
    traversePages :: Text -> Int -> m (Maybe Topic)
    traversePages shorten page = do
      CategoryTopics{..} <- deGetCategoryTopics catId (Just page)
      let mTopic = find (T.isInfixOf shorten . unTitle . thTitle) ctTopics
      case mTopic of
        Just th -> Just <$> deGetTopic (thId th)
        Nothing
          | length ctTopics < ctPerPage || ctPerPage == 0 -> pure Nothing
          | otherwise                   -> traversePages shorten (page + 1)

    -- TODO maybe more robust scheme is needed here
    convertTopic :: Topic -> m TopicOnePost
    convertTopic (MkTopic tId tTitle (post :| _)) = pure $ MkTopic tId tTitle post

workerPoster
  :: ( MonadUnliftIO m
     , HasAgoraConfig caps
     , HasCap Logging caps
     , HasCap PostgresConn caps
     )
  => DiscourseEndpoints (AsClientT m)
  -> TBChan ProposalHash
  -> DiscourseCategoryId
  -> CapsT caps m ()
workerPoster DiscourseEndpoints{..} chan cId = forever $ do
  ph <- UIO.atomically $ readTBChan chan
--  let shorten = shortenHash ph
--  let retryIn = 5 -- 5 seconds
--  let retryInInt = fromIntegral retryIn :: Int
--  suppressException @SomeException
--    retryIn
--    (\e -> logError $
--      "Something went wrong in the Discourse worker: " +| displayException e |+ ". \
--          \Retry with the same proposal " +| shorten |+ " in " +| retryInInt |+ " seconds. ")
--    $
--      suppressException @DiscourseError
--        retryIn
--        (\e -> logWarning $ "Something went wrong with Discourse API in the worker: " +| displayException e |+
--                      ". Retry with the same proposal " +| shorten |+ " in " +| retryInInt |+ " seconds. ")
  (workerDo ph)
  where
    workerDo ph = do
      apiUsername <- fromAgoraConfig $ sub #discourse . option #api_username
      apiKey <- fromAgoraConfig $ sub #discourse . option #api_key
      let shorten = shortenHash ph
      let title = Title shorten
      let body = RawBody $ defaultDescription shorten
      ct <- lift $ dePostTopic (Just apiUsername) (Just apiKey) (CreateTopic title body cId)
      initProposalDiscourseFields ct ph title

workerFetcher
  :: ( MonadUnliftIO m
     , HasCap Logging caps
     , HasCap PostgresConn caps
     )
  => DiscourseEndpoints (AsClientT m)
  -> Second
  -> CapsT caps m ()
workerFetcher DiscourseEndpoints{..} retryEvery = forever $ do
--  let retryInInt = fromIntegral retryEvery :: Int
  -- pva701: wait first because of tests, which are run
  -- in isolated transaction, which starts only when tests run, not before
  UIO.threadDelay $ fromIntegral $ toMicroseconds retryEvery
--  suppressException @SomeException
--    retryEvery
--    (\e -> logError $
--      "Something went wrong in the Discourse post listener: " +| displayException e |+ ". \
--          \Retry with in " +| retryInInt |+ " seconds. ")
  workerDo
  where
    workerDo = do
      proposals <- runSelectReturningList' $ select $ all_ asProposals
      logDebug $ "Updating meta information about proposals: " +| listF (map prHash proposals) |+ ""
      UIO.forConcurrently_ proposals $ \Proposal{..} -> handleExceptions prHash $
        case (prDiscoursePostId, prDiscourseTopicId) of
          (Just postId, Just topicId) -> do
            cooked <- pCooked <$> lift (deGetPost postId)
            title <- tTitle <$> lift (deGetTopic topicId)
            case parseHtmlParts cooked of
              Left e ->
                logWarning $
                  "Discourse post template for " +| prHash |+ " doesn't correspond to\
                  \ expected one. Parsing erorr happened: " +| e |+ ""
              Right hp ->
                updateProposalDiscourseFields prHash title (toHtmlPartsMaybe (shortenHash prHash) hp)
          _ -> pass
      logInfo $ "Updated meta information about proposals: " +| listF (map prHash proposals) |+ ""

    handleExceptions ph action =
      action
        `UIO.catch` (\(e :: DiscourseError) ->
          logWarning $ "During parsing of a Discourse post for proposal hash\
          \ " +| ph |+ " API error happened: " +| displayException e |+ "")
        `UIO.catch` (\(e :: SomeException) ->
          logError $ "During parsing of a Discourse post for proposal hash\
          \ " +| ph |+ ", something went wrong: " +| displayException e |+ "")

    AgoraSchema{..} = agoraSchema

initProposalDiscourseFields
  :: (MonadIO m, MonadPostgresConn m)
  => CreatedTopic
  -> ProposalHash
  -> Title
  -> m ()
initProposalDiscourseFields CreatedTopic{..} ph title = runUpdate' $
  update asProposals (\ln ->
    (prDiscourseTitle ln <-. val_ (Just $ unTitle title)) <>
    (prDiscourseTopicId ln <-. val_ (Just ctTopicId)) <>
    (prDiscoursePostId ln <-.  val_ (Just ctId)))
  (\ln -> prHash ln ==. val_ ph)
  where
    AgoraSchema{..} = agoraSchema

updateProposalDiscourseFields
  :: (MonadIO m, MonadPostgresConn m)
  => ProposalHash
  -> Title
  -> HtmlParts (Maybe Text)
  -> m ()
updateProposalDiscourseFields ph title HtmlParts{..} =
  runUpdate' $ update asProposals (\ln ->
    (prDiscourseTitle ln <-. val_ (Just $ unTitle title)) <>
    (prDiscourseShortDesc ln <-. val_ hpShort) <>
    (prDiscourseLongDesc ln <-. val_ hpLong) <>
    (prDiscourseFile ln <-. val_ hpFileLink))
  (\ln -> prHash ln ==. val_ ph)
  where
    AgoraSchema{..} = agoraSchema
