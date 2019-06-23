module Agora.Types where

import Data.Aeson (FromJSON (..), ToJSON (..), Value (..), withText)
import Data.Aeson.Options (defaultOptions)
import Data.Aeson.TH (deriveJSON)
import Data.Time.Clock (UTCTime)
import Lens.Micro.Platform (makeLenses)

-- | Basic type which represents all hashes in the system
newtype Hash = Hash ByteString
  deriving (Show, Eq, Ord)

-- | Info about the proposal
data Proposal = Proposal
  { _pHash        :: !Hash
  , _pTitle       :: !Text
  , _pDescription :: !Text
  } deriving (Show, Eq, Generic)

-- | Enum for period type
data PeriodType
  = Proposing
  | Exploration
  | Testing
  | Promotion
  deriving (Show, Eq, Ord, Enum, Bounded)

-- | Info about the period
data Period = Period
  { _pNum        :: !Word
  , _pType       :: !PeriodType
  , _pStartLevel :: !Word
  , _pCycle      :: !Word
  } deriving (Show, Eq, Generic)

-- | Delegates participation info
data VoteStats = VoteStats
  { _vsVotesCast      :: !Word
  , _vsVotesAvailable :: !Word
  } deriving (Show, Eq, Generic)

-- | Voting decision on proposal
data Decision = Yay | Nay | Pass
  deriving (Show, Eq, Ord, Enum, Bounded)

-- | Voting stats
data Ballots = Ballots
  { _bYay  :: !Word
  , _bNay  :: !Word
  , _bPass :: !Word
  } deriving (Show, Eq, Generic)

-- | Full info about the period
data PeriodInfo = PeriodInfo
  { _piPeriod    :: !Period
  , _piVoteStats :: !(Maybe VoteStats)   -- ^ `Nothing` for `Testing` period
  , _piProposal  :: !(Maybe Proposal)    -- ^ `Nothing` for `Proposal` and `Testing`
  , _piBallots   :: !(Maybe Ballots)     -- ^ `Nothing` for `Proposal` and `Testing`
  } deriving (Show, Eq, Generic)

-- | Info about baker
data Baker = Baker
  { _bkPkh   :: !Hash  -- ^ Public key hash
  , _bkRolls :: !Word  -- ^ Number of rolls delegated
  , _bkName  :: !Text  -- ^ Name (from BakingBad)
  } deriving (Show, Eq, Generic)

data ProposalVote = ProposalVote
  { _pvProposal  :: !Hash
  , _pvAuthor    :: !Baker
  , _pvOperation :: !Hash
  , _pvTimestamp :: !UTCTime
  } deriving (Show, Eq, Generic)

data Vote = Vote
  { _vAuthor    :: !Baker
  , _vDecision  :: !Decision
  , _vOperation :: !Hash
  , _vTimestamp :: !UTCTime
  } deriving (Show, Eq, Generic)

makeLenses ''Proposal
makeLenses ''Period
makeLenses ''VoteStats
makeLenses ''Ballots
makeLenses ''PeriodInfo
makeLenses ''Baker
makeLenses ''ProposalVote
makeLenses ''Vote

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
deriveJSON defaultOptions ''Vote
