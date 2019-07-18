{-|
Basic types derived from Tezos blockchain.
-}

module Agora.Types
       ( Hash (..)
       , PublicKeyHash
       , ProposalHash
       , BlockHash
       , OperationHash
       , PeriodId
       , ProposalId
       , BallotId
       , ProposalVoteId

       , Cycle (..)
       , Level (..)
       , Id (..)
       , Votes (..)
       , Rolls (..)
       , Quorum (..)
       , Decision (..)
       , PeriodType (..)
       , VoteType (..)
       ) where

import Data.Aeson (FromJSON (..), ToJSON (..), Value (..), withText)
import Data.Aeson.Options (defaultOptions)
import Data.Aeson.TH (deriveJSON)
import Servant.API (FromHttpApiData (..), ToHttpApiData (..))

import Agora.Util

-- | General representation of Hash for
-- any data.
newtype Hash a = Hash ByteString
  deriving (Show, Eq, Ord, Generic)

-- | Generalised id
newtype Id a = Id Int32
  deriving (Show, Eq, Ord, Generic, Num, Real, Integral, Enum, FromHttpApiData)

data PublicKeyTag = PublicKeyTag
  deriving (Show, Eq, Ord, Generic)

data PeriodTag = PeriodTag
  deriving (Show, Eq, Ord, Generic)

data ProposalTag = ProposalTag
  deriving (Show, Eq, Ord, Generic)

data BlockTag = BlockTag
  deriving (Show, Eq, Ord, Generic)

data OperationTag = OperationTag
  deriving (Show, Eq, Ord, Generic)

data BallotTag = BallotTag
  deriving (Show, Eq, Ord, Generic)

data ProposalVoteTag = ProposalVoteTag
  deriving (Show, Eq, Ord, Generic)

-- Tagged Hashes not to misuse different kinds of hashes.
type PublicKeyHash = Hash PublicKeyTag
type ProposalHash = Hash ProposalTag
type BlockHash = Hash BlockTag
type OperationHash = Hash OperationTag

type PeriodId = Id PeriodTag
type ProposalId = Id ProposalTag
type BallotId = Id BallotTag
type ProposalVoteId = Id ProposalVoteTag

-- | Cycle of blocks. One cycle consists of 4049 blocks.
newtype Cycle = Cycle Int32
  deriving (Show, Eq, Ord, Generic, Num, Enum, Integral, Real)

-- | Level of a block. Level is basically
-- index number of the block in the blockchain.
newtype Level = Level Int32
  deriving (Show, Eq, Ord, Generic, Num, Enum)

-- | Sum of votes, it can be upvotes, as well ass sum of ballots.
newtype Votes = Votes Int32
  deriving (Show, Eq, Ord, Generic, Num, Enum)

-- | Number of rolls belonging to a baker.
newtype Rolls = Rolls Int32
  deriving (Show, Eq, Ord, Generic, Num, Enum)

-- | Quorum value multiplied by 100
newtype Quorum = Quorum Int32
  deriving (Show, Eq, Ord, Generic, Num, Enum)

instance FromJSON (Hash a) where
  parseJSON = withText "Hash" $ pure . Hash . encodeUtf8

instance ToJSON (Hash a) where
  toJSON (Hash h) = String $ decodeUtf8 h

instance FromJSON (Id a) where
  parseJSON = fmap Id . parseJSON

instance ToJSON (Id a) where
  toJSON (Id i) = toJSON i

instance ToHttpApiData (Hash a) where
  toUrlPiece (Hash h) = decodeUtf8 h

-- | Enum for period type.
data PeriodType
  = Proposing     -- ^ Proposal phase (named `Proposing` to avoid name clashes with @Proposal@ datatype)
  | Exploration   -- ^ Exploration phase
  | Testing       -- ^ Testing phase
  | Promotion     -- ^ Promotion phase
  deriving (Show, Eq, Ord, Enum, Bounded, Generic)

-- | Voting decision on proposal.
data Decision = Yay | Nay | Pass
  deriving (Show, Eq, Ord, Enum, Bounded, Generic)

instance FromHttpApiData Decision where
  parseQueryParam "yay"  = Right Yay
  parseQueryParam "nay"  = Right Nay
  parseQueryParam "pass" = Right Pass
  parseQueryParam _      = Left "unexpected decision param"

instance TagEnum PeriodType where
  enumDesc _ = "Period type"
  toTag Proposing   = "proposal"
  toTag Exploration = "testing_vote"
  toTag Testing     = "testing"
  toTag Promotion   = "promotion_vote"

instance TagEnum Decision where
  enumDesc _ = "Ballot decision"
  toTag Yay  = "yay"
  toTag Nay  = "nay"
  toTag Pass = "pass"

-- | Enum for vote type (Exploration or Promotion).
data VoteType
  = ExplorationVote
  | PromotionVote
  deriving (Show, Eq, Ord, Enum, Bounded, Generic)

deriveJSON defaultOptions ''Cycle
deriveJSON defaultOptions ''Level
deriveJSON defaultOptions ''Votes
deriveJSON defaultOptions ''Rolls
