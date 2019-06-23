module Agora.Web.Handlers where

import Servant.API.Generic
import Servant.Server.Generic (AsServer, genericServer)

import Agora.Types
import Agora.Web.API

type AgoraHandlers = ToServant AgoraEndpoints AsServer

agoraHandlers :: AgoraHandlers
agoraHandlers = genericServer AgoraEndpoints
  { aePeriod = \_periodNum -> pure examplePeriod
  , aeProposals = \_periodNum -> pure []
  , aeProposalVotes = \_periodNum -> pure []
  , aeVotes = \_periodNum -> pure []
  }

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
