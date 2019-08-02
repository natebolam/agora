{-# LANGUAGE TypeOperators #-}

module Agora.Discourse.Client
       ( DiscourseClient (..)
       , MonadDiscourseClient (..)
       , withDiscourseClient

       -- * For tests
       , withDiscourseClientImpl
       , initProposalDiscourseFields
       ) where

import Control.Monad.Reader (withReaderT)
import Control.Concurrent.STM.TBChan (TBChan, newTBChan, readTBChan, tryWriteTBChan)
import Database.Beam.Query ((<-.), val_, update, (==.))
import qualified Data.Text as T
import Fmt ((+|), (|+))
import Loot.Log (Logging, logDebug, logInfo, logError, logWarning)
import Monad.Capabilities (CapImpl (..), CapsT, HasNoCap, addCap, makeCap, HasCap)
import Network.HTTP.Client (newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Servant.Client (ClientM, ServantError, mkClientEnv, runClientM)
import Servant.Client.Generic (genericClientHoist, AsClientT)
import UnliftIO (MonadUnliftIO)
import qualified UnliftIO as UIO

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
  = DiscourseApiError !ServantError
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
  let hoist :: forall x . ClientM x -> m x
      hoist clientM = liftIO $
        runClientM clientM clientEnv >>= \case
          Left e  -> UIO.throwIO $ DiscourseApiError e
          Right x -> pure x
  withDiscourseClientImpl (genericClientHoist hoist) action

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
  let shorten = shortenHash ph
  let retryIn = 5 -- 5 seconds
  let retryInInt = fromIntegral retryIn :: Int
  supressException @SomeException
    retryIn
    (\e -> logError $
      "Something went wrong in the Discourse worker: " +| displayException e |+ ". \
          \Retry with the same proposal " +| shorten |+ " in " +| retryInInt |+ " seconds. ")
    $
      supressException @DiscourseError
        retryIn
        (\e -> logWarning $ "Something went wrong with Discourse API in the worker: " +| displayException e |+
                      ". Retry with the same proposal " +| shorten |+ " in " +| retryInInt |+ " seconds. ")
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

initProposalDiscourseFields
  :: ( MonadIO m
     , MonadPostgresConn m
     )
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