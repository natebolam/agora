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

       , agoraApiSwagger
       , swaggerSpecFilePath
       ) where

import Data.Aeson (Value (..))
import Data.Aeson.Options (defaultOptions)
import qualified Data.Swagger as S
import Data.Swagger.Declare (Declare)
import qualified Data.Swagger.Internal.Schema as S
import Data.Swagger.Internal.TypeShape (GenericHasSimpleShape, GenericShape)
import qualified GHC.Generics as G
import Lens.Micro.Platform (zoom, (.=), (?=))
import Servant ((:<|>) (..), (:>),  Server)
import Servant.Swagger (HasSwagger (..))
import Servant.Swagger.UI (SwaggerSchemaUI, swaggerSchemaUIServer)
import Servant.Swagger.UI.Core (SwaggerUiHtml)
import Servant.Util (Tag)

import Agora.Types
import Agora.Web.API

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
    => proxy a -> Declare (S.Definitions S.Schema) S.NamedSchema
gDeclareNamedSchema = S.genericDeclareNamedSchema schemaOptions

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

instance S.ToSchema Proposal where
  declareNamedSchema = gDeclareNamedSchema

instance S.ToSchema Period where
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
