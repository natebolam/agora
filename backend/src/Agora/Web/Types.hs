module Agora.Web.Types
       ( Proposal (..)
       , pHash
       , pTitle
       , pDescription
       , PeriodType (..)
       , Period (..)
       , pNum
       , pType
       , pStartLevel
       , pCycle
       , VoteStats (..)
       , vsVotesCast
       , vsVotesAvailable
       , Ballots (..)
       , bYay
       , bNay
       , bPass
       , PeriodInfo (..)
       , piPeriod
       , piVoteStats
       , piProposal
       , piBallots
       , Baker (..)
       , bkPkh
       , bkRolls
       , bkName
       , ProposalVote (..)
       , pvProposal
       , pvAuthor
       , pvOperation
       , pvTimestamp
       , Ballot (..)
       , bAuthor
       , bDecision
       , bOperation
       , bTimestamp
       ) where

import Data.Aeson.Options (defaultOptions)
import Data.Aeson.TH (deriveJSON)
import Data.Time.Clock (UTCTime)
import Lens.Micro.Platform (makeLenses)

import Agora.Types

-- | Info about the proposal.
data Proposal = Proposal
  { _pHash        :: !ProposalHash  -- ^ Proposal hash aka Protocol hash (serves as ID)
  , _pTitle       :: !Text          -- ^ Proposal title
  , _pDescription :: !Text          -- ^ Proposal description
  } deriving (Show, Eq, Generic)

-- | Info about the period.
data Period = Period
  { _pNum        :: !PeriodNum     -- ^ Period number
  , _pType       :: !PeriodType    -- ^ Period type
  , _pStartLevel :: !Level         -- ^ The level (block number) when the period starts
  , _pCycle      :: !Cycle         -- ^ Current cycle of the period (0-based)
  } deriving (Show, Eq, Generic)

-- | Delegates participation info.
data VoteStats = VoteStats
  { _vsVotesCast      :: !Votes    -- ^ All the votes (weighted by rolls) casted in this period
  , _vsVotesAvailable :: !Votes    -- ^ All the votes which may be casted in this period
  } deriving (Show, Eq, Generic)

-- | Voting stats.
data Ballots = Ballots
  { _bYay  :: !Votes -- ^ Number of votes for
  , _bNay  :: !Votes -- ^ Number of votes against
  , _bPass :: !Votes -- ^ Number of passed votes
  } deriving (Show, Eq, Generic)

-- | Full info about the period.
data PeriodInfo = PeriodInfo
  { _piPeriod    :: !Period
  , _piVoteStats :: !(Maybe VoteStats)   -- ^ `Nothing` for `Testing` period
  , _piProposal  :: !(Maybe Proposal)    -- ^ `Nothing` for `Proposal`
  , _piBallots   :: !(Maybe Ballots)     -- ^ `Nothing` for `Proposal` and `Testing`
  } deriving (Show, Eq, Generic)

-- | Info about baker.
data Baker = Baker
  { _bkPkh   :: !PublicKeyHash    -- ^ Public key hash
  , _bkRolls :: !Rolls            -- ^ Number of rolls delegated
  , _bkName  :: !Text             -- ^ Name (from BakingBad)
  } deriving (Show, Eq, Generic)

-- | Vote for the proposal to be considered in the proposal period.
data ProposalVote = ProposalVote
  { _pvProposal  :: !ProposalHash  -- ^ Hash of the corresponding proposal
  , _pvAuthor    :: !Baker         -- ^ Vote author
  , _pvOperation :: !OperationHash -- ^ Hash of the corresponding blockchain operation
  , _pvTimestamp :: !UTCTime       -- ^ Time the vote has been cast
  } deriving (Show, Eq, Generic)

-- | Vote for (or against) the proposal during one of voting periods.
data Ballot = Ballot
  { _bAuthor    :: !Baker         -- ^ Vote author
  , _bDecision  :: !Decision      -- ^ Vote decision
  , _bOperation :: !OperationHash -- ^ Hash of the corresponding blockchain operation
  , _bTimestamp :: !UTCTime       -- ^ Time the vote has been cast
  } deriving (Show, Eq, Generic)

makeLenses ''Proposal
makeLenses ''Period
makeLenses ''VoteStats
makeLenses ''Ballots
makeLenses ''PeriodInfo
makeLenses ''Baker
makeLenses ''ProposalVote
makeLenses ''Ballot

deriveJSON defaultOptions ''Proposal
deriveJSON defaultOptions ''Period
deriveJSON defaultOptions ''VoteStats
deriveJSON defaultOptions ''Ballots
deriveJSON defaultOptions ''PeriodInfo
deriveJSON defaultOptions ''Baker
deriveJSON defaultOptions ''ProposalVote
deriveJSON defaultOptions ''Ballot
