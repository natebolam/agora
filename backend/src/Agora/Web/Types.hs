module Agora.Web.Types
       ( Proposal (..)
       , PeriodType (..)
       , Period (..)
       , PeriodTimeInfo (..)
       , VoteStats (..)
       , Ballots (..)
       , PeriodInfo (..)
       , Baker (..)
       , ProposalVote (..)
       , Ballot (..)
       , iPeriod
       , iTotalPeriods
       , eiProposal
       , pId
       , prId
       , prDiscourseLink
       , pvId
       , bId
       , bYay
       , bNay
       , bPass
       , bQuorum
       , bSupermajority
       ) where

import Data.Aeson.Options (defaultOptions)
import Data.Aeson.TH (deriveJSON)
import Data.Time.Clock (UTCTime)
import Fmt (Buildable (..))
import Lens.Micro.Platform (makeLenses, makeLensesFor)
import Servant.Util (ForResponseLog (..), buildListForResponse)

import Agora.Types
import Agora.Util

-- | Full info about the period.
data PeriodInfo
  = ProposalInfo
  { _iPeriod        :: !Period           -- ^ Common info about the period
  , _iTotalPeriods  :: !Word32           -- ^ Total number of periods so far
  , _iPeriodTimes   :: ![PeriodTimeInfo] -- ^ Info about start and end times of all periods
  , _piVoteStats    :: !VoteStats
  , _iDiscourseLink :: !Text
  }
  | ExplorationInfo
  { _iPeriod        :: !Period
  , _iTotalPeriods  :: !Word32
  , _iPeriodTimes   :: ![PeriodTimeInfo]
  , _eiProposal     :: !Proposal
  , _eiVoteStats    :: !VoteStats
  , _eiBallots      :: !Ballots
  , _iDiscourseLink :: !Text
  }
  | TestingInfo
  { _iPeriod        :: !Period
  , _iTotalPeriods  :: !Word32
  , _iPeriodTimes   :: ![PeriodTimeInfo]
  , _tiProposal     :: !Proposal
  , _iDiscourseLink :: !Text
  }
  | PromotionInfo
  { _iPeriod        :: !Period
  , _iTotalPeriods  :: !Word32
  , _iPeriodTimes   :: ![PeriodTimeInfo]
  , _piProposal     :: !Proposal
  , _piVoteStats    :: !VoteStats
  , _piBallots      :: !Ballots
  , _iDiscourseLink :: !Text
  } deriving (Show, Eq, Generic)

-- | Info about the proposal.
data Proposal = Proposal
  { _prId               :: !ProposalId   -- ^ Proposal ordering ID (autoincrement in DB)
  , _prPeriod           :: !PeriodId     -- ^ Period id where proposal was sent
  , _prHash             :: !ProposalHash -- ^ Proposal hash (serves as ID)
  , _prTitle            :: !(Maybe Text) -- ^ Proposal title
  , _prShortDescription :: !(Maybe Text) -- ^ Short description
  , _prLongDescription  :: !(Maybe Text) -- ^ Long description
  , _prTimeCreated      :: !UTCTime      -- ^ Time the proposal has been proposed
  , _prProposalFile     :: !(Maybe Text) -- ^ Link to the proposal file, if present
  , _prDiscourseLink    :: !(Maybe Text) -- ^ Link to the Discourse discussion, if present
  , _prProposer         :: !Baker        -- ^ A baker who initially proposed that
  , _prVotesCasted      :: !Votes        -- ^ Votes are cast for this proposal so far
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

-- | Info only about start and end times of period (for displaying in the nav dropdown)
data PeriodTimeInfo = PeriodTimeInfo
  { _pitStartTime :: !UTCTime
  , _pitEndTime   :: !UTCTime
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
  , _vsNumVoters      :: !Int      -- ^ The number of the bakers voted in this period
  , _vsNumVotersTotal :: !Int      -- ^ The number of bakers who can vote in this period
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

instance Buildable Proposal where
  build = buildFromJSON

instance Buildable ProposalVote where
  build = buildFromJSON

instance Buildable Ballot where
  build = buildFromJSON

instance Buildable PeriodInfo where
  build = buildFromJSON

instance Buildable Baker where
  build = buildFromJSON

deriving instance Buildable (ForResponseLog Proposal)
deriving instance Buildable (ForResponseLog ProposalVote)
deriving instance Buildable (ForResponseLog Ballot)
deriving instance Buildable (ForResponseLog PeriodInfo)
deriving instance Buildable (ForResponseLog Baker)

instance Buildable (ForResponseLog [Baker]) where
  build = buildListForResponse (take 5)

instance Buildable (ForResponseLog [Proposal]) where
    build = buildListForResponse (take 5)

makeLensesFor [("_iPeriod", "iPeriod"), ("_iTotalPeriods", "iTotalPeriods"), ("_eiProposal", "eiProposal")] ''PeriodInfo
makeLensesFor [("_pId", "pId")] ''Period
makeLensesFor [("_prId", "prId"), ("_prDiscourseLink", "prDiscourseLink")] ''Proposal
makeLensesFor [("_pvId", "pvId")] ''ProposalVote
makeLensesFor [("_bId", "bId")] ''Ballot
makeLenses ''Ballots

deriveJSON defaultOptions ''Proposal
deriveJSON defaultOptions ''Period
deriveJSON defaultOptions ''PeriodTimeInfo
deriveJSON defaultOptions ''VoteStats
deriveJSON defaultOptions ''Ballots
deriveJSON defaultOptions ''PeriodInfo
deriveJSON defaultOptions ''Baker
deriveJSON defaultOptions ''ProposalVote
deriveJSON defaultOptions ''Ballot
