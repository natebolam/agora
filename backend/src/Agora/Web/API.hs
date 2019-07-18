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
import Servant.Util (PaginationParams)

import Agora.Types
import Agora.Util
import Agora.Web.Types

-- | API endpoints specification.
data AgoraEndpoints route = AgoraEndpoints
  { -- | Info about a given period.
    aePeriod :: route
      :- "period"
      :> QueryParam "id" PeriodId
      :> Summary "Info about given voting period"
      :> Verb 'GET 200 '[JSON] PeriodInfo

    -- | Proposals for given proposal period.
  , aeProposals :: route
      :- "proposals"
      :> Capture "period_id" PeriodId
      :> PaginationParams
      :> QueryParam "lastId" ProposalId
      :> Summary "Proposals for given proposal period."
      :> Verb 'GET 200 '[JSON] (PaginatedList Proposal)

    -- | Proposal votes for given proposal period.
  , aeProposalVotes :: route
      :- "proposal_votes"
      :> Capture "period_id" PeriodId
      :> PaginationParams
      :> QueryParam "lastId" ProposalVoteId
      :> Summary "Proposal votes for given proposal period."
      :> Verb 'GET 200 '[JSON] (PaginatedList ProposalVote)

    -- | Ballots for given voting period.
  , aeBallots :: route
      :- "ballots"
      :> Capture "period_id" PeriodId
      :> PaginationParams
      :> QueryParam "lastId" BallotId
      :> QueryParam "decision" Decision
      :> Summary "Ballots for given voting period."
      :> Verb 'GET 200 '[JSON] (PaginatedList Ballot)
  } deriving (Generic)

-- | API type specification.
type AgoraAPI =
  "api" :> "v1" :> ToServant AgoraEndpoints AsApi

agoraAPI :: Proxy AgoraAPI
agoraAPI = Proxy
