{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module Agora.Web.API
     ( AgoraEndpoints (..)
     , AgoraAPI
     , agoraAPI
     ) where

import Servant.API
import Servant.API.Generic
import Servant.Util (PaginationParams)

import Agora.Types

-- | API endpoints specification
data AgoraEndpoints route = AgoraEndpoints
  { -- * Info about a given period
    aePeriod :: route
      :- "period"
      :> QueryParam "num" Word
      :> Verb 'GET 200 '[JSON] PeriodInfo

  , aeProposals :: route
      :- "proposals"
      :> Capture "period_num" Word
      :> PaginationParams
      :> Verb 'GET 200 '[JSON] [Proposal]

  , aeProposalVotes :: route
      :- "proposal_votes"
      :> Capture "period_num" Word
      :> PaginationParams
      :> Verb 'GET 200 '[JSON] [ProposalVote]

  , aeVotes :: route
      :- "votes"
      :> Capture "period_num" Word
      :> PaginationParams
      :> Verb 'GET 200 '[JSON] [Vote]

  } deriving (Generic)

-- | API type specification
type AgoraAPI =
  "api" :> "v1" :> ToServant AgoraEndpoints AsApi

agoraAPI :: Proxy AgoraAPI
agoraAPI = Proxy
