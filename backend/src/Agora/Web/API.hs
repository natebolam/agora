{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

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

-- | API endpoints specification.
data AgoraEndpoints route = AgoraEndpoints
  { -- | Info about a given period.
    aePeriod :: route
      :- "period"
      :> QueryParam "num" Word
      :> Summary "Info about given voting period"
      :> Verb 'GET 200 '[JSON] PeriodInfo

    -- | Proposals for given proposal period.
  , aeProposals :: route
      :- "proposals"
      :> Capture "period_num" Word
      :> PaginationParams
      :> Summary "Proposals for given proposal period."
      :> Verb 'GET 200 '[JSON] [Proposal]

    -- | Proposal votes for given proposal period.
  , aeProposalVotes :: route
      :- "proposal_votes"
      :> Capture "period_num" Word
      :> PaginationParams
      :> Summary "Proposal votes for given proposal period."
      :> Verb 'GET 200 '[JSON] [ProposalVote]

    -- | Ballots for given voting period.
  , aeBallots :: route
      :- "ballots"
      :> Capture "period_num" Word
      :> PaginationParams
      :> Summary "Ballots for given voting period."
      :> Verb 'GET 200 '[JSON] [Ballot]

  } deriving (Generic)

-- | API type specification.
type AgoraAPI =
  "api" :> "v1" :> ToServant AgoraEndpoints AsApi

agoraAPI :: Proxy AgoraAPI
agoraAPI = Proxy
