{-|
API handlers implementation.
-}
module Agora.Web.Handlers
       ( agoraHandlers
       ) where

import Servant.API.Generic (ToServant)
import Servant.Server.Generic (AsServer, genericServer)

import Agora.Types
import Agora.Web.API

type AgoraHandlers = ToServant AgoraEndpoints AsServer

-- | Server handler implementation for Agora API.
agoraHandlers :: AgoraHandlers
agoraHandlers = genericServer AgoraEndpoints
  { aePeriod = \_periodNum -> pure examplePeriod
  , aeProposals = \_periodNum _pagination -> pure []
  , aeProposalVotes = \_periodNum _pagination -> pure []
  , aeBallots = \_periodNum _pagination -> pure []
  }

-- | Mock data for `GET /period`.
examplePeriod :: PeriodInfo
examplePeriod = PeriodInfo
  { _piPeriod = Period
    { _pNum = 0
    , _pType = Proposing
    , _pStartLevel = 0
    , _pCycle = 4
    }
  , _piVoteStats = Just $ VoteStats 1000 4000
  , _piProposal = Just Proposal
    { _pHash = Hash "asdfasdfasdfas"
    , _pTitle = "Pataq"
    , _pDescription = "bardaq"
    }
  , _piBallots = Just $ Ballots 30 40 5
  }
