{-# LANGUAGE TypeOperators #-}

module Agora.Discourse.Client
       ( DiscourseClient (..)
       , MonadDiscourseClient (..)
       , withDiscourseClient
       ) where

import Control.Monad.Reader (withReaderT)
import qualified Data.Text as T
import Monad.Capabilities (CapImpl (..), CapsT, HasNoCap, addCap, makeCap)
import Network.HTTP.Client (newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Servant.Client (ClientM, ServantError, mkClientEnv, runClientM)
import Servant.Client.Generic (genericClientHoist, AsClientT)
import UnliftIO (MonadUnliftIO, throwIO)

import Agora.Config
import Agora.Discourse.API
import Agora.Discourse.Types
import Agora.Types

data DiscourseClient m = DiscourseClient
  { _postProposalTopic :: Title -> RawBody -> m TopicOnePost
  , _getProposalTopic  :: Text  -> m (Maybe TopicOnePost)
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
  , MonadUnliftIO m
  )
  => CapsT (DiscourseClient ': caps) m a
  -> CapsT caps m a
withDiscourseClient action = do
  manager <- liftIO (newManager tlsManagerSettings)
  host <- fromAgoraConfig $ sub #discourse . option #host
  categoryName <- fromAgoraConfig $ sub #discourse . option #category

  let clientEnv = mkClientEnv manager host
  let hoist :: forall x . ClientM x -> m x
      hoist clientM = liftIO $
        runClientM clientM clientEnv >>= \case
          Left e  -> throwIO $ DiscourseApiError e
          Right x -> pure x

  let discourseEndpoints = genericClientHoist hoist
  CategoryList categories <- lift (deGetCategories discourseEndpoints)
  Category{..} <- find ((categoryName ==) . cName) categories
    `whenNothing` throwIO (CategoryNotFound categoryName)

  withReaderT (addCap $ discourseClient discourseEndpoints cId) action

discourseClient
  :: forall m . MonadUnliftIO m
  => DiscourseEndpoints (AsClientT m)
  -> DiscourseCategoryId
  -> CapImpl DiscourseClient '[AgoraConfigCap] m
discourseClient DiscourseEndpoints{..} catId = CapImpl $ DiscourseClient
    { _postProposalTopic = \t b -> do
        apiUsername <- fromAgoraConfig $ sub #discourse . option #api_username
        apiKey <- fromAgoraConfig $ sub #discourse . option #api_key
        topicId <- ctTopicId <$> lift (dePostTopic (Just apiUsername) (Just apiKey) (CreateTopic t b catId))
        lift $ convertTopic =<< deGetTopic topicId
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