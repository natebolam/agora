{-# LANGUAGE DataKinds #-}

{-|
Servant type-level specification for Client API to retrieve data from Tezos Node.
-}
module Agora.Discourse.API
       ( DiscourseEndpoints (..)
       ) where

import Servant.API ((:>), Capture, Get, JSON, Post, ReqBody, QueryParam, Header)
import Servant.API.Generic ((:-))

import qualified Agora.Discourse.Types as T
import Agora.Util (ApiUsername, ApiKey)
import Agora.Types

data DiscourseEndpoints route = DiscourseEndpoints
  { dePostTopic :: route
      :- "posts"
      :> Header "Api-Username" ApiUsername
      :> Header "Api-Key" ApiKey
      :> ReqBody '[JSON] T.CreateTopic
      :> Post '[JSON]    T.CreatedTopic

  , deGetCategoryTopics :: route
      :- "c"
      :> Capture "category_id" DiscourseCategoryId
      :> QueryParam "page" Int
      :> Get '[JSON] T.CategoryTopics

  , deGetCategories :: route
      :- "categories"
      :> Get '[JSON] T.CategoryList

  , deGetTopic :: route
      :- "t"
      :> Capture "topic_id" DiscourseTopicId
      :> Get '[JSON] T.Topic
  } deriving Generic
