{-# LANGUAGE DataKinds #-}
{-|
Definitions used for serving Swagger docs.
-}
module Agora.Web.Swagger
       ( SwaggerUI
       , WithSwaggerUI
       , withSwaggerUI

       , AgoraAPIWithDocs
       , agoraAPIWithDocs

       , agoraApiSwagger
       , swaggerSpecFilePath
       ) where

import Data.Aeson.Options (defaultOptions)
import qualified Data.Swagger as S
import Data.Swagger.Declare (Declare)
import qualified Data.Swagger.Internal.Schema as S
import Data.Swagger.Internal.TypeShape (GenericHasSimpleShape, GenericShape)
import qualified GHC.Generics as G
import Lens.Micro.Platform (zoom, (.=), (?=))
import Servant ((:<|>) (..), (:>), Server)
import Servant.Swagger (HasSwagger (..))
import Servant.Swagger.UI (SwaggerSchemaUI, swaggerSchemaUIServer)
import Servant.Swagger.UI.Core (SwaggerUiHtml)
import Servant.Util (Tag)

import Agora.Types
import Agora.Util
import Agora.Web.API
import Agora.Web.Types

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

-- | A path to a Swagger spec file which is stored in a Git repo.
swaggerSpecFilePath :: FilePath
swaggerSpecFilePath = "specs/swagger.yaml"

----------------------------------------------------------------------------
-- Instances
----------------------------------------------------------------------------

-- | Schema generation options which match JSON generation options.
schemaOptions :: S.SchemaOptions
schemaOptions = S.fromAesonOptions defaultOptions

-- | Default implementation of 'ToSchema' via Generics.
gDeclareNamedSchema
    :: ( Generic a
       , S.GToSchema (G.Rep a)
       , GenericHasSimpleShape a "genericDeclareNamedSchemaUnrestricted" (GenericShape (G.Rep a))
       )
    => Proxy a -> Declare (S.Definitions S.Schema) S.NamedSchema
gDeclareNamedSchema = S.genericDeclareNamedSchema schemaOptions

instance S.ToSchema (SwaggerUiHtml dir api) where
  declareNamedSchema _ =
    S.plain $ mempty `executingState` do
      S.type_ ?= S.SwaggerNull
      S.title ?= "Swagger UI page"

instance S.ToSchema S.Swagger where
  declareNamedSchema _ =
    S.plain $ mempty `executingState` do
      S.type_ ?= S.SwaggerObject
      S.title ?= "Swagger specification"
      S.description ?= "The specification you are currently reading."

instance S.ToSchema (Hash a) where
  declareNamedSchema _ =
    return $ S.named "Hash" $ S.byteSchema `executingState` do
      S.description ?= "Base58 hash value"

instance S.ToSchema Decision where
  declareNamedSchema = declareNamedSchemaTag

instance S.ToSchema PeriodType where
  declareNamedSchema = declareNamedSchemaTag

instance S.ToSchema Proposal where
  declareNamedSchema = gDeclareNamedSchema

instance S.ToSchema PeriodId where
  declareNamedSchema _ =
    return $ S.named "PeriodId" $ S.toSchemaBoundedIntegral (Proxy @Int32) `executingState` do
      S.description ?= "Period number"

instance S.ToParamSchema (Id a) where
  toParamSchema = S.genericToParamSchema schemaOptions

instance S.ToParamSchema Decision where
  toParamSchema = S.genericToParamSchema schemaOptions

instance S.ToParamSchema Limit where
  toParamSchema = S.genericToParamSchema schemaOptions

instance S.ToSchema ProposalId where
  declareNamedSchema _ =
    return $ S.named "ProposalId" $ S.toSchemaBoundedIntegral (Proxy @Int32) `executingState` do
      S.description ?= "Proposal id"

instance S.ToSchema ProposalVoteId where
  declareNamedSchema _ =
    return $ S.named "ProposalVoteId" $ S.toSchemaBoundedIntegral (Proxy @Int32) `executingState` do
      S.description ?= "Proposal vote id"

instance S.ToSchema BallotId where
  declareNamedSchema _ =
    return $ S.named "BallotId" $ S.toSchemaBoundedIntegral (Proxy @Int32) `executingState` do
      S.description ?= "Ballot id"

instance S.ToSchema Level where
  declareNamedSchema _ =
    return $ S.named "Level" $ S.toSchemaBoundedIntegral (Proxy @Int32) `executingState` do
      S.description ?= "Block level"

instance S.ToSchema Cycle where
  declareNamedSchema _ =
    return $ S.named "Cycle" $ S.toSchemaBoundedIntegral (Proxy @Int32) `executingState` do
      S.description ?= "Cycle number"

instance S.ToSchema Votes where
  declareNamedSchema _ =
    return $ S.named "Votes" $ S.toSchemaBoundedIntegral (Proxy @Int32) `executingState` do
      S.description ?= "Number of votes"

instance S.ToSchema Voters where
  declareNamedSchema _ =
    return $ S.named "Voters" $ S.toSchemaBoundedIntegral (Proxy @Int32) `executingState` do
      S.description ?= "Number of voters"

instance S.ToSchema Rolls where
  declareNamedSchema _ =
    return $ S.named "Rolls" $ S.toSchemaBoundedIntegral (Proxy @Int32) `executingState` do
      S.description ?= "Number of rolls"

instance S.ToSchema Period where
  declareNamedSchema = gDeclareNamedSchema

instance S.ToSchema PeriodItemInfo where
  declareNamedSchema = gDeclareNamedSchema

instance S.ToSchema VoteStats where
  declareNamedSchema = gDeclareNamedSchema

instance S.ToSchema Ballots where
  declareNamedSchema = gDeclareNamedSchema

instance S.ToSchema PeriodInfo where
  declareNamedSchema = gDeclareNamedSchema

instance S.ToSchema Baker where
  declareNamedSchema = gDeclareNamedSchema

instance S.ToSchema ProposalVote where
  declareNamedSchema = gDeclareNamedSchema

instance S.ToSchema Ballot where
  declareNamedSchema = gDeclareNamedSchema

instance S.ToSchema Limit where
  declareNamedSchema _ =
    return $ S.named "Limit" $ S.toSchemaBoundedIntegral (Proxy @Word32) `executingState` do
      S.description ?= "Requested number of entries to return"

instance S.ToSchema Amount where
  declareNamedSchema _ =
    return $ S.named "Amount" $ S.toSchemaBoundedIntegral (Proxy @Word32) `executingState` do
      S.description ?= "Number of rest entries"

instance S.ToSchema PaginationData where
  declareNamedSchema = gDeclareNamedSchema

instance S.ToSchema a => S.ToSchema (PaginatedList a) where
  declareNamedSchema = gDeclareNamedSchema
