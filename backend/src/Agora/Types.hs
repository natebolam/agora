module Agora.Types
       ( Hash (..)
       , Proposal (..)
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
       , Decision (..)
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

import Data.Aeson (FromJSON (..), ToJSON (..), Value (..), withText)
import Data.Aeson.Options (defaultOptions)
import Data.Aeson.TH (deriveJSON)
import Data.Time.Clock (UTCTime)
import Lens.Micro.Platform (makeLenses)

-- | Basic type which represents all hashes in the system.
newtype Hash = Hash ByteString
  deriving (Show, Eq, Ord, Generic)

-- | Info about the proposal.
data Proposal = Proposal
  { _pHash        :: !Hash  -- ^ Proposal hash (serves as ID)
  , _pTitle       :: !Text  -- ^ Proposal title
  , _pDescription :: !Text  -- ^ Proposal description
  } deriving (Show, Eq, Generic)

-- | Enum for period type.
data PeriodType
  = Proposing     -- ^ Proposal phase (named `Proposing` to avoid name clashes with @Proposal@ datatype)
  | Exploration   -- ^ Exploration phase
  | Testing       -- ^ Testing phase
  | Promotion     -- ^ Promotion phase
  deriving (Show, Eq, Ord, Enum, Bounded, Generic)

-- | Info about the period.
data Period = Period
  { _pNum        :: !Word         -- ^ Period number
  , _pType       :: !PeriodType   -- ^ Period type
  , _pStartLevel :: !Word         -- ^ The level (block number) when the period starts
  , _pCycle      :: !Word         -- ^ Current cycle of the period
  } deriving (Show, Eq, Generic)

-- | Delegates participation info.
data VoteStats = VoteStats
  { _vsVotesCast      :: !Word    -- ^ All the votes (weighted by rolls) casted in this period
  , _vsVotesAvailable :: !Word    -- ^ All the votes which may be casted in this period
  } deriving (Show, Eq, Generic)

-- | Voting decision on proposal.
data Decision = Yay | Nay | Pass
  deriving (Show, Eq, Ord, Enum, Bounded)

-- | Voting stats.
data Ballots = Ballots
  { _bYay  :: !Word   -- ^ Number of votes for
  , _bNay  :: !Word   -- ^ Number of votes against
  , _bPass :: !Word   -- ^ Number of passed votes
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
  { _bkPkh   :: !Hash  -- ^ Public key hash
  , _bkRolls :: !Word  -- ^ Number of rolls delegated
  , _bkName  :: !Text  -- ^ Name (from BakingBad)
  } deriving (Show, Eq, Generic)

-- | Vote for the proposal to be considered in the proposal period.
data ProposalVote = ProposalVote
  { _pvProposal  :: !Hash      -- ^ Hash of the corresponding proposal
  , _pvAuthor    :: !Baker     -- ^ Vote author
  , _pvOperation :: !Hash      -- ^ Hash of the corresponding blockchain operation
  , _pvTimestamp :: !UTCTime   -- ^ Time the vote has been cast
  } deriving (Show, Eq, Generic)

-- | Vote for (or against) the proposal during one of voting periods.
data Ballot = Ballot
  { _bAuthor    :: !Baker      -- ^ Vote author
  , _bDecision  :: !Decision   -- ^ Vote decision
  , _bOperation :: !Hash       -- ^ Hash of the corresponding blockchain operation
  , _bTimestamp :: !UTCTime    -- ^ Time the vote has been cast
  } deriving (Show, Eq, Generic)

makeLenses ''Proposal
makeLenses ''Period
makeLenses ''VoteStats
makeLenses ''Ballots
makeLenses ''PeriodInfo
makeLenses ''Baker
makeLenses ''ProposalVote
makeLenses ''Ballot

instance FromJSON Hash where
  parseJSON = withText "Hash" $ pure . Hash . encodeUtf8
instance ToJSON Hash where
  toJSON (Hash h) = String $ decodeUtf8 h

instance FromJSON Decision where
  parseJSON = withText "Decision" $ \case
    "yay"  -> pure Yay
    "nay"  -> pure Nay
    "pass" -> pure Pass
    other  -> fail $ "Invalid decision: " ++ toString other
instance ToJSON Decision where
  toJSON d = String $ case d of
    Yay  -> "yay"
    Nay  -> "nay"
    Pass -> "pass"

instance FromJSON PeriodType where
  parseJSON = withText "PeriodType" $ \case
    "proposal"    -> pure Proposing
    "exploration" -> pure Exploration
    "testing"     -> pure Testing
    "promotion"   -> pure Promotion
    other         -> fail $ "Invalid period type: " ++ toString other
instance ToJSON PeriodType where
  toJSON ptype = String $ case ptype of
    Proposing   -> "proposal"
    Exploration -> "exploration"
    Testing     -> "testing"
    Promotion   -> "promotion"

deriveJSON defaultOptions ''Proposal
deriveJSON defaultOptions ''Period
deriveJSON defaultOptions ''VoteStats
deriveJSON defaultOptions ''Ballots
deriveJSON defaultOptions ''PeriodInfo
deriveJSON defaultOptions ''Baker
deriveJSON defaultOptions ''ProposalVote
deriveJSON defaultOptions ''Ballot
