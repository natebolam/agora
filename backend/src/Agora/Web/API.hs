{-# LANGUAGE DataKinds #-}

{-|
Servant type-level specification for Agora API.
-}
module Agora.Web.API
     ( AgoraEndpoints (..)
     , AgoraAPI
     , agoraAPI
     ) where

import Servant.API ((:>), Capture, JSON, QueryParam, StdMethod (..), Summary, Verb)
import Servant.API.Generic ((:-), AsApi, ToServant)

import Agora.Types
import Agora.Web.Types

-- | API endpoints specification.
data AgoraEndpoints route = AgoraEndpoints
  { -- | Info about a given stage.
    aeStage :: route
      :- "stage"
      :> QueryParam "id" Stage
      :> Summary "Info about given voting stage"
      :> Verb 'GET 200 '[JSON] StageInfo

    -- | Proposals for given proposal stage.
  , aeProposals :: route
      :- "proposals"
      :> Capture "stage_id" Stage
      :> Summary "Proposals for given proposal stage."
      :> Verb 'GET 200 '[JSON] [Proposal]

    -- | Info about specific proposal
  , aeProposal :: route
      :- "proposal"
      :> Capture "proposal_id" Int
      :> "stage"
      :> Capture "stage_id" Stage
      :> Summary "Info about specific proposal"
      :> Verb 'GET 200 '[JSON] Proposal

    -- | Proposal votes for a specific proposal
  , aeProposalVotes :: route
      :- "votes"
      :> "stage"
      :> Capture "stage_id" Stage
      :> Summary "Proposal votes issued for a given proposal"
      :> Verb 'GET 200 '[JSON] [ProposalVote]
      
  , aeSpecificProposalVotes :: route
      :- "proposal_votes"
      :> "stage"
      :> Capture "stage_id" Stage
      :> "proposal"
      :> Capture "proposal_id" Int
      :> "votes"
      :> Summary "Proposal votes issued for a given proposal"
      :> Verb 'GET 200 '[JSON] [ProposalVote]

  } deriving (Generic)

-- | API type specification.
type AgoraAPI =
  "api" :> "v1" :> ToServant AgoraEndpoints AsApi

agoraAPI :: Proxy AgoraAPI
agoraAPI = Proxy
