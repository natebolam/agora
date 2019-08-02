{-# LANGUAGE TypeOperators #-}

module Agora.Discourse.Types
       ( Title (..)
       , RawBody (..)
       , HtmlTemplate (..)
       , CreateTopic (..)
       , CreatedTopic (..)
       , Post (..)
       , Topic
       , TopicOnePost
       , Topic' (..)
       , TopicHead (..)
       , Category (..)
       , CategoryList (..)
       , CategoryTopics (..)
       ) where

import Data.Aeson.TH (deriveJSON)
import Data.Aeson (FromJSON (..), ToJSON (..), withObject, (.:))
import Text.HTML.Parser (Token)
import Fmt (Buildable (..))

import Agora.Types
import Agora.Util (snakeCaseOptions)

newtype Title = Title {unTitle :: Text}
  deriving (Eq, Show, Generic, IsString, FromJSON, ToJSON)

deriving instance Buildable Title

newtype RawBody = RawBody {unRawBody :: Text}
  deriving (Eq, Show, Generic, FromJSON, ToJSON)

newtype HtmlTemplate = HtmlTemplate [Token]

data CreateTopic = CreateTopic
  { ctTitle    :: Title
  , ctRaw      :: RawBody
  , ctCategory :: DiscourseCategoryId
  } deriving (Eq, Show, Generic)

data CreatedTopic = CreatedTopic
  { ctId      :: DiscoursePostId
  , ctTopicId :: DiscourseTopicId
  } deriving (Eq, Show, Generic)

data Post = Post
  { pId      :: DiscoursePostId
  , pTopicId :: DiscourseTopicId
  , pCooked  :: Text
  } deriving (Eq, Show, Generic)

data Topic' a = MkTopic
  { tId    :: DiscourseTopicId
  , tTitle :: Title
  , tPosts :: a
  } deriving (Eq, Show, Generic)

type Topic = Topic' (NonEmpty Post)
type TopicOnePost = Topic' Post

data TopicHead = TopicHead
  { thId    :: DiscourseTopicId
  , thTitle :: Title
  } deriving (Eq, Show, Generic)

data CategoryTopics = CategoryTopics
  { ctPerPage :: Int
  , ctTopics  :: [TopicHead]
  } deriving (Eq, Show, Generic)

data Category = Category
  { cId   :: DiscourseCategoryId
  , cName :: Text
  } deriving (Eq, Show, Generic)

newtype CategoryList = CategoryList [Category]
  deriving (Eq, Show, Generic)

instance FromJSON CategoryTopics where
  parseJSON =
    withObject "Category" $ \c ->
      withObject "TopicList" (\o ->
        CategoryTopics <$> (o .: "per_page") <*> o .: "topics") =<< c .: "topic_list"

instance FromJSON CategoryList where
  parseJSON =
    withObject "CategoryList" $ \c ->
      withObject "CategoryList" (\o -> CategoryList <$> o .: "categories") =<< c .: "category_list"

instance FromJSON Topic where
  parseJSON = withObject "Topic" $ \t ->
    withObject "PostStream" (\ps -> MkTopic <$> t .: "id" <*> t .: "title" <*> ps .: "posts") =<< t .: "post_stream"

deriveJSON snakeCaseOptions ''Category
deriveJSON snakeCaseOptions ''CreateTopic
deriveJSON snakeCaseOptions ''CreatedTopic
deriveJSON snakeCaseOptions ''Post
deriveJSON snakeCaseOptions ''TopicHead
