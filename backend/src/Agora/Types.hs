{-|
Basic types derived from Tezos blockchain.
-}

module Agora.Types
       ( Hash (..)
       , encodeHash
       , shortenHash
       , PublicKeyHash
       , ProposalHash
       , BlockHash
       , OperationHash
       , PeriodId
       , ProposalId
       , BallotId
       , ProposalVoteId
       , DiscourseCategoryId
       , DiscourseTopicId
       , DiscoursePostId

       , Cycle (..)
       , Level (..)
       , Id (..)
       , Votes (..)
       , Rolls (..)
       , sumRolls
       , Quorum (..)
       , Decision (..)
       , PeriodType (..)
       , VoteType (..)
       ) where

import Data.Aeson (FromJSON (..), ToJSON (..), Value (..), withText)
import Data.Aeson.Options (defaultOptions)
import Data.Aeson.TH (deriveJSON)
import qualified Data.Text as T
import Fmt (Buildable (..), listF)
import Servant.API (FromHttpApiData (..), ToHttpApiData (..))

import Agora.Util

-- | General representation of Hash for
-- any data.
newtype Hash a = Hash ByteString
  deriving (Show, Eq, Ord, Generic)

encodeHash :: Text -> Hash a
encodeHash = Hash . encodeUtf8

shortenHash :: Hash a -> Text
shortenHash (Hash x) = T.take 9 $ decodeUtf8 x

-- | Generalised id
newtype Id a = Id Int32
  deriving (Show, Eq, Ord, Generic, Num, Real, Integral, Enum, FromHttpApiData, Buildable, ToHttpApiData)

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

data DiscourseCategoryIdTag = DiscourseCategoryIdTag
  deriving (Show, Eq, Ord, Generic)

data DiscourseTopicIdTag = DiscourseTopicIdTag
  deriving (Show, Eq, Ord, Generic)

data DiscoursePostIdTag = DiscoursePostIdTag
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
type DiscourseCategoryId = Id DiscourseCategoryIdTag
type DiscourseTopicId    = Id DiscourseTopicIdTag
type DiscoursePostId     = Id DiscoursePostIdTag

-- | Cycle of blocks. One cycle consists of 4049 blocks.
newtype Cycle = Cycle Int32
  deriving (Show, Eq, Ord, Generic, Num, Enum, Integral, Real)

-- | Level of a block. Level is basically
-- index number of the block in the blockchain.
newtype Level = Level Int32
  deriving (Show, Eq, Ord, Generic, Real, Integral, Num, Enum, Buildable)

-- | Sum of votes, it can be upvotes, as well ass sum of ballots.
newtype Votes = Votes Int32
  deriving (Show, Eq, Ord, Generic, Num, Enum, Real, Integral)

-- | Number of rolls belonging to a baker.
-- Int64 as underlying type because sum_ from beam
-- fails with Int32.
newtype Rolls = Rolls Int64
  deriving (Show, Eq, Ord, Generic, Num, Enum, Integral, Real, Buildable)

sumRolls :: [Rolls] -> Votes
sumRolls = fromIntegral . sum

-- | Quorum value multiplied by 100
newtype Quorum = Quorum Int32
  deriving (Show, Eq, Ord, Generic, Num, Enum, Real, Integral)

instance FromJSON (Hash a) where
  parseJSON = withText "Hash" $ pure . encodeHash

instance ToJSON (Hash a) where
  toJSON (Hash h) = String $ decodeUtf8 h

instance FromJSON (Id a) where
  parseJSON = fmap Id . parseJSON

instance ToJSON (Id a) where
  toJSON (Id i) = toJSON i

instance Buildable (Hash a) where
  build (Hash h) = fromString $ decodeUtf8 h

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

instance Buildable Decision where
  build = buildTag

instance FromHttpApiData [Decision] where
  parseQueryParam t
    | "[" `T.isPrefixOf` t && "]" `T.isSuffixOf` t =
      let res = T.splitOn "," $ T.dropEnd 1 $ T.drop 1 t in
      case res of
        [""] -> Right []
        _    -> traverse parseQueryParam res
    | otherwise = Left "expected list of decisions"

instance Buildable [Decision] where
  build = listF

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

instance FromJSON PeriodType where
  parseJSON = parseJSONTag

instance ToJSON PeriodType where
  toJSON = toJSONTag

-- | Enum for vote type (Exploration or Promotion).
data VoteType
  = ExplorationVote
  | PromotionVote
  deriving (Show, Eq, Ord, Enum, Bounded, Generic)

deriveJSON defaultOptions ''Decision
deriveJSON defaultOptions ''Cycle
deriveJSON defaultOptions ''Level
deriveJSON defaultOptions ''Votes
deriveJSON defaultOptions ''Rolls
deriveJSON defaultOptions ''Quorum
