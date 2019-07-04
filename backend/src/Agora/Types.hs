{-|
Basic types derived from Tezos blockchain.
-}

module Agora.Types
       ( Hash (..)
       , PublicKeyHash
       , ProposalHash
       , BlockHash
       , OperationHash

       , Cycle (..)
       , Level (..)
       , PeriodNum (..)
       , Votes (..)
       , Rolls (..)
       , Decision (..)
       , PeriodType (..)
       ) where

import Data.Aeson (FromJSON (..), ToJSON (..), Value (..), withText)
import Data.Aeson.Options (defaultOptions)
import Data.Aeson.TH (deriveJSON)
import Servant.API (ToHttpApiData (..), FromHttpApiData (..))

import Agora.Util
-- | General representation of Hash for
-- any data.
newtype Hash a = Hash ByteString
  deriving (Show, Eq, Ord, Generic)

data PublicKeyTag = PublicKeyTag
  deriving (Show, Eq, Ord, Generic)

data ProposalTag = ProposalTag
  deriving (Show, Eq, Ord, Generic)

data BlockTag = BlockTag
  deriving (Show, Eq, Ord, Generic)

data OperationTag = OperationTag
  deriving (Show, Eq, Ord, Generic)

-- Tagged Hashes not to misuse different kinds of hashes.
type PublicKeyHash = Hash PublicKeyTag
type ProposalHash = Hash ProposalTag
type BlockHash = Hash BlockTag
type OperationHash = Hash OperationTag

-- | Cycle of blocks. One cycle consists of 4049 blocks.
newtype Cycle = Cycle Word32
  deriving (Show, Eq, Ord, Generic, Num, Enum, Integral, Real)

-- | Level of a block. Level is basically
-- index number of the block in the blockchain.
newtype Level = Level Word32
  deriving (Show, Eq, Ord, Generic, Num, Enum)

-- | Number of period. Period consists of 8 cycles.
newtype PeriodNum = PeriodNum Word32
  deriving (Show, Eq, Ord, Generic, Num, Enum, FromHttpApiData)

-- | Sum of votes, it can be upvotes, as well ass sum of ballots.
newtype Votes = Votes Word32
  deriving (Show, Eq, Ord, Generic, Num, Enum)

-- | Number of rolls belonging to a baker.
newtype Rolls = Rolls Word32
  deriving (Show, Eq, Ord, Generic, Num, Enum)

-- | Voting decision on proposal.
data Decision = Yay | Nay | Pass
  deriving (Show, Eq, Ord, Enum, Bounded)

instance FromJSON (Hash a) where
  parseJSON = withText "Hash" $ pure . Hash . encodeUtf8

instance ToJSON (Hash a) where
  toJSON (Hash h) = String $ decodeUtf8 h

instance ToHttpApiData (Hash a) where
  toUrlPiece (Hash h) = decodeUtf8 h

-- | Enum for period type.
data PeriodType
  = Proposing     -- ^ Proposal phase (named `Proposing` to avoid name clashes with @Proposal@ datatype)
  | Exploration   -- ^ Exploration phase
  | Testing       -- ^ Testing phase
  | Promotion     -- ^ Promotion phase
  deriving (Show, Eq, Ord, Enum, Bounded, Generic)

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

deriveJSON defaultOptions ''Cycle
deriveJSON defaultOptions ''Level
deriveJSON defaultOptions ''PeriodNum
deriveJSON defaultOptions ''Votes
deriveJSON defaultOptions ''Rolls
