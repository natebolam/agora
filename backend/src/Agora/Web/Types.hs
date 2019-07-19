module Agora.Web.Types
       ( Proposal (..)
       , PeriodType (..)
       , Period (..)
       , VoteStats (..)
       , Ballots (..)
       , PeriodInfo (..)
       , Baker (..)
       , ProposalVote (..)
       , Ballot (..)
       , iPeriod
       , iTotalPeriods
       , pId
       , prId
       , pvId
       , bId
       ) where

import Data.Aeson.Options (defaultOptions)
import Data.Aeson.TH (deriveJSON)
import Data.Time.Clock (UTCTime)
import Lens.Micro.Platform (makeLensesFor)

import Agora.Types
import Agora.Util

-- | Full info about the period.
data PeriodInfo
  = ProposalInfo
  { _iPeriod       :: !Period     -- ^ Common info about the period
  , _iTotalPeriods :: !Word32     -- ^ Total number of periods so far
  , _piVoteStats   :: !VoteStats  -- ^ `Nothing` for `Testing` period
  }
  | ExplorationInfo
  { _iPeriod       :: !Period
  , _iTotalPeriods :: !Word32
  , _eiProposal    :: !Proposal
  , _eiVoteStats   :: !VoteStats
  , _eiBallots     :: !Ballots
  }
  | TestingInfo
  { _iPeriod        :: !Period
  , _iTotalPeriods  :: !Word32
  , _tiProposal     :: !Proposal
  }
  | PromotionInfo
  { _iPeriod       :: !Period
  , _iTotalPeriods :: !Word32
  , _piProposal    :: !Proposal
  , _piVoteStats   :: !VoteStats
  , _piBallots     :: !Ballots
  } deriving (Show, Eq, Generic)

-- | Info about the proposal.
data Proposal = Proposal
  { _prId               :: !ProposalId   -- ^ Proposal ordering ID (autoincrement in DB)
  , _prHash             :: !ProposalHash -- ^ Proposal hash (serves as ID)
  , _prTitle            :: !Text         -- ^ Proposal title
  , _prShortDescription :: !Text         -- ^ Short description
  , _prLongDescription  :: !Text         -- ^ Long description
  , _prTimeCreated      :: !UTCTime      -- ^ Time the proposal has been proposed
  , _prProposalFile     :: !(Maybe Text) -- ^ Link to the proposal file, if present
  , _prDiscourseLink    :: !(Maybe Text) -- ^ Link to the Discourse discussion, if present
  , _prProposer         :: !Baker        -- ^ A baker who initially proposed that
  } deriving (Show, Eq, Generic)

-- | Info about the period.
data Period = Period
  { _pId         :: !PeriodId     -- ^ Period ID
  , _pStartLevel :: !Level        -- ^ The level (block number) when the period starts
  , _pEndLevel   :: !Level        -- ^ The level (block number) when the period starts
  , _pStartTime  :: !UTCTime      -- ^ The moment this period started
  , _pEndTime    :: !UTCTime      -- ^ The moment this period ended (or should end)
  , _pCycle      :: !Cycle        -- ^ Current cycle of the period
  } deriving (Show, Eq, Generic)

-- | Voting stats.
data Ballots = Ballots
  { _bYay           :: !Votes   -- ^ Number of votes for
  , _bNay           :: !Votes   -- ^ Number of votes against
  , _bPass          :: !Votes   -- ^ Number of passed votes
  , _bQuorum        :: !Float    -- ^ Current quorum (num from 0 to 1)
  , _bSupermajority :: !Float    -- ^ Current supermajority (currently constant and equal to 0.8)
  } deriving (Show, Eq, Generic)

-- | Vote for the proposal to be considered in the proposal period.
data ProposalVote = ProposalVote
  { _pvId        :: !ProposalVoteId -- ^ Proposal vote ordering ID (autoincrement in DB)
  , _pvProposal  :: !ProposalHash   -- ^ Hash of the corresponding proposal
  , _pvAuthor    :: !Baker          -- ^ Vote author
  , _pvOperation :: !OperationHash  -- ^ Hash of the corresponding blockchain operation
  , _pvTimestamp :: !UTCTime        -- ^ Time the vote has been cast
  } deriving (Show, Eq, Generic)

-- | Vote for (or against) the proposal during one of voting periods.
data Ballot = Ballot
  { _bId        :: !BallotId      -- ^ Ballot ordering ID (autoincrement in DB)
  , _bAuthor    :: !Baker         -- ^ Vote author
  , _bDecision  :: !Decision      -- ^ Vote decision
  , _bOperation :: !OperationHash -- ^ Hash of the corresponding blockchain operation
  , _bTimestamp :: !UTCTime       -- ^ Time the vote has been cast
  } deriving (Show, Eq, Generic)

-- | Delegates participation info.
data VoteStats = VoteStats
  { _vsVotesCast      :: !Votes    -- ^ All the votes (weighted by rolls) casted in this period
  , _vsVotesAvailable :: !Votes    -- ^ All the votes which may be casted in this period
  } deriving (Show, Eq, Generic)

-- | Info about baker.
data Baker = Baker
  { _bkPkh     :: !PublicKeyHash  -- ^ Public key hash
  , _bkRolls   :: !Rolls          -- ^ Number of rolls delegated
  , _bkName    :: !Text           -- ^ Name (from BakingBad)
  , _bkLogoUrl :: !(Maybe Text)   -- ^ Logo URL, if present
  } deriving (Show, Eq, Generic)

instance HasId Proposal where
  type IdT Proposal = ProposalId
  getId = _prId

instance HasId Period where
  type IdT Period = PeriodId
  getId = _pId

instance HasId ProposalVote where
  type IdT ProposalVote = ProposalVoteId
  getId = _pvId

instance HasId Ballot where
  type IdT Ballot = BallotId
  getId = _bId

makeLensesFor [("_iPeriod", "iPeriod"), ("_iTotalPeriods", "iTotalPeriods")] ''PeriodInfo
makeLensesFor [("_pId", "pId")] ''Period
makeLensesFor [("_prId", "prId")] ''Proposal
makeLensesFor [("_pvId", "pvId")] ''ProposalVote
makeLensesFor [("_bId", "bId")] ''Ballot

deriveJSON defaultOptions ''Proposal
deriveJSON defaultOptions ''Period
deriveJSON defaultOptions ''VoteStats
deriveJSON defaultOptions ''Ballots
deriveJSON defaultOptions ''PeriodInfo
deriveJSON defaultOptions ''Baker
deriveJSON defaultOptions ''ProposalVote
deriveJSON defaultOptions ''Ballot
