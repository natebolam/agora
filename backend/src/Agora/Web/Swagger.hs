{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

{-|
Definitions used for serving Swagger docs.
-}
module Agora.Web.Swagger
       ( SwaggerUI
       , WithSwaggerUI
       , withSwaggerUI

       , AgoraAPIWithDocs
       , agoraAPIWithDocs
       , agoraServerWithDocs
       ) where

import Data.Aeson (Value (..))
import qualified Data.Swagger as S
import qualified Data.Swagger.Internal.Schema as S
import Lens.Micro.Platform (zoom, (.=), (?=))
import Servant ((:<|>) (..), (:>), Server)
import Servant.Swagger (HasSwagger (..))
import Servant.Swagger.UI (SwaggerSchemaUI, swaggerSchemaUIServer)
import Servant.Swagger.UI.Core (SwaggerUiHtml)
import Servant.Util (Tag)

import Agora.Types
import Agora.Web.API
import Agora.Web.Handlers

----------------------------------------------------------------------------
-- Generic definitions
----------------------------------------------------------------------------

-- | Swagger UI we use across the project.
type SwaggerUI =
  Tag "Documentation" :>
  SwaggerSchemaUI "docs" "swagger.json"

-- | Attach a swagger UI to the given API.
type WithSwaggerUI api = api :<|> SwaggerUI

-- | Attach an UI serving given documentation to the given server.
withSwaggerUI
  :: Proxy api
  -> S.Swagger
  -> Server api
  -> Server (WithSwaggerUI api)
withSwaggerUI _ swagger server =
  server :<|> swaggerSchemaUIServer swagger

----------------------------------------------------------------------------
-- Agora docs
----------------------------------------------------------------------------

-- | Type for Agora API augmented with documentation.
type AgoraAPIWithDocs = WithSwaggerUI AgoraAPI

agoraAPIWithDocs :: Proxy AgoraAPIWithDocs
agoraAPIWithDocs = Proxy

-- | Generates swagger documentation for Agora API.
agoraApiSwagger :: S.Swagger
agoraApiSwagger = executingState (toSwagger agoraAPI) $ do
    zoom S.info $ do
      S.title .= "Tezos Agora API"
      S.version .= "1.0.0"
      S.contact ?= mempty `executingState` do
        S.name ?= "Serokell OÜ"
        S.email ?= "hi@serokell.io"
        S.url ?= S.URL "https://serokell.io"

    S.externalDocs ?= mempty `executingState` do
      S.description ?= "Find out more about Swagger"
      S.url .= S.URL "http://swagger.io"

    S.schemes ?= [S.Http, S.Https]

-- | A server which serves Agora API with docs
agoraServerWithDocs :: Server AgoraAPIWithDocs
agoraServerWithDocs = withSwaggerUI agoraAPI agoraApiSwagger agoraHandlers

----------------------------------------------------------------------------
-- Instances
----------------------------------------------------------------------------

instance S.ToSchema (SwaggerUiHtml dir api) where
  declareNamedSchema _ =
    S.plain $ mempty `executingState` do
      S.type_ .= S.SwaggerNull
      S.title ?= "Swagger UI page"

instance S.ToSchema S.Swagger where
  declareNamedSchema _ =
    S.plain $ mempty `executingState` do
      S.type_ .= S.SwaggerObject
      S.title ?= "Swagger specification"
      S.description ?= "The specification you are currently reading."

instance S.ToSchema Hash where
  declareNamedSchema _ =
    return $ S.named "Hash" $ S.byteSchema `executingState` do
      S.description ?= "Base58 hash value"

instance S.ToSchema PeriodType where
  declareNamedSchema _ =
    return $ S.named "PeriodType" $ mempty `executingState` do
      S.description ?= "Period type"
      S.enum_ ?= map String ["proposal", "exploration", "testing", "promotion"]

instance S.ToSchema Decision where
  declareNamedSchema _ =
    return $ S.named "Decision" $ mempty `executingState` do
      S.description ?= "Ballot decision"
      S.enum_ ?= map String ["yay", "nay", "pass"]

instance S.ToSchema Proposal
instance S.ToSchema Period
instance S.ToSchema VoteStats
instance S.ToSchema Ballots
instance S.ToSchema PeriodInfo
instance S.ToSchema Baker
instance S.ToSchema ProposalVote
instance S.ToSchema Ballot
