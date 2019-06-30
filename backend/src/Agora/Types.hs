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

-- | Enum for period type.
data PeriodType
  = Proposing     -- ^ Proposal phase (named `Proposing` to avoid name clashes with @Proposal@ datatype)
  | Exploration   -- ^ Exploration phase
  | Testing       -- ^ Testing phase
  | Promotion     -- ^ Promotion phase
  deriving (Show, Eq, Ord, Enum, Bounded, Generic)

instance FromJSON PeriodType where
  parseJSON = withText "PeriodType" $ \case
    "proposal"     -> pure Proposing
    "testing_vote" -> pure Exploration
    "testing"      -> pure Testing
    "promotion"    -> pure Promotion
    other          -> fail $ "Invalid period type: " ++ toString other

instance ToJSON PeriodType where
  toJSON ptype = String $ case ptype of
    Proposing   -> "proposal"
    Exploration -> "testing_vote"
    Testing     -> "testing"
    Promotion   -> "promotion"

deriveJSON defaultOptions ''Cycle
deriveJSON defaultOptions ''Level
deriveJSON defaultOptions ''PeriodNum
deriveJSON defaultOptions ''Votes
deriveJSON defaultOptions ''Rolls
