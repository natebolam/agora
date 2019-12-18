{-|
Database schema definition and initialization.

Literally _everything_ should be exported from that file,
hence not using explicit exports.
-}
module Agora.DB.Schema where

import Data.FileEmbed (embedStringFile)
import Data.Time.Clock (UTCTime)
import Database.Beam.Backend (MonadBeam, SqlSerial, runNoReturn)
import Database.Beam.Postgres (Postgres)
import Database.Beam.Postgres.Syntax (PgCommandSyntax (..), PgCommandType (..), emit)
import Database.Beam.Schema (Beamable, C, Database, DatabaseSettings, Nullable, Table (..),
                             TableEntity, defaultDbSettings)

import Agora.Types

data PeriodMetaT f = PeriodMeta
  { pmId             :: C f PeriodId
  , pmType           :: C f PeriodType
  , pmVotesCast      :: C f Votes
  , pmVotesAvailable :: C f Votes
  , pmVotersNum      :: C f Voters
  , pmTotalVotersNum :: C f Voters
  , pmQuorum         :: C f Quorum
  , pmWhenStarted    :: C f UTCTime
  , pmStartLevel     :: C f Level
  , pmEndLevel       :: C f Level
  , pmLastBlockLevel :: C f Level
  , pmLastBlockHash  :: C f BlockHash
  , pmPrevBlockHash  :: C f BlockHash
  , pmBallotsYay     :: C f Votes -- should be equal to zero when period is proposing or testing
  , pmBallotsNay     :: C f Votes -- should be equal to zero when period is proposing or testing
  , pmBallotsPass    :: C f Votes -- should be equal to zero when period is proposing or testing
  } deriving (Generic)

data VoterT f = Voter
  { voterPbkHash    :: C f PublicKeyHash
  , voterName       :: C (Nullable f) Text
  , voterLogoUrl    :: C (Nullable f) Text
  , voterProfileUrl :: C (Nullable f) Text
  , voterRolls      :: C f Rolls
  , voterPeriod     :: PrimaryKey PeriodMetaT f
  } deriving (Generic)

data ProposalT f = Proposal
  { prId                 :: C f (SqlSerial Int)
  , prPeriod             :: PrimaryKey PeriodMetaT f
  , prHash               :: C f ProposalHash
  , prTimeProposed       :: C f UTCTime
  , prProposer           :: PrimaryKey VoterT f
  , prVotesCast          :: C f Votes
  , prVotersNum          :: C f Voters

  , prDiscourseTitle     :: C (Nullable f) Text
  , prDiscourseShortDesc :: C (Nullable f) Text
  , prDiscourseLongDesc  :: C (Nullable f) Text
  , prDiscourseFile      :: C (Nullable f) Text
  , prDiscourseTopicId   :: C (Nullable f) DiscourseTopicId
  , prDiscoursePostId    :: C (Nullable f) DiscoursePostId
  } deriving (Generic)

data BlockMetaT f = BlockMeta
  { blLevel            :: C f Level
  , blHash             :: C f BlockHash
  , blPredecessor      :: C f BlockHash
  , blBlockTime        :: C f UTCTime
  , blVotingPeriodType :: C f PeriodType
  } deriving (Generic)

data ProposalVoteT f = ProposalVote
  { pvId          :: C f (SqlSerial Int)
  , pvVoter       :: PrimaryKey VoterT f
  , pvProposal    :: PrimaryKey ProposalT f
  , pvCastedRolls :: C f Rolls
  , pvOperation   :: C f OperationHash
  , pvVoteTime    :: C f UTCTime
  , pvBlock       :: PrimaryKey BlockMetaT f
  } deriving (Generic)

data BallotT f = Ballot
  { bId             :: C f (SqlSerial Int)
  , bVoteType       :: C f VoteType
  , bVoter          :: PrimaryKey VoterT f
  , bPeriod         :: PrimaryKey PeriodMetaT f
  , bProposal       :: PrimaryKey ProposalT f
  , bCastedRolls    :: C f Rolls
  , bOperation      :: C f OperationHash
  , bBallotTime     :: C f UTCTime
  , bBallotDecision :: C f Decision
  , bBlock          :: PrimaryKey BlockMetaT f
  } deriving (Generic)

data CouncilT f = Council
  { cPbkHash    :: C f PublicKeyHash
  , cStage      :: C f Stage
  } deriving (Generic)

data StkrProposalT f = StkrProposal
  { spId                 :: C f (SqlSerial Int)
  , spStage              :: C f Stage
  , spHash               :: C f ProposalHash
  , spProposer           :: PrimaryKey CouncilT f
  , spVotersNum          :: C f Voters

  , spDiscourseTitle     :: C (Nullable f) Text
  , spDiscourseShortDesc :: C (Nullable f) Text
  , spDiscourseLongDesc  :: C (Nullable f) Text
  , spDiscourseFile      :: C (Nullable f) Text
  , spDiscourseTopicId   :: C (Nullable f) DiscourseTopicId
  , spDiscoursePostId    :: C (Nullable f) DiscoursePostId
  } deriving (Generic)

data PolicyT f = Policy
  { pcHash :: C f PolicyHash
  , pcUrl  :: C f Text
  } deriving (Generic)

data VoteT f = Vote
  { vId          :: C f (SqlSerial Int)
  , vVoter       :: PrimaryKey CouncilT f
  , vProposal    :: PrimaryKey StkrProposalT f
  , vBlock       :: PrimaryKey BlockMetaT f
  } deriving (Generic)


type PeriodMeta = PeriodMetaT Identity
type Voter = VoterT Identity
type Proposal = ProposalT Identity
type ProposalVote = ProposalVoteT Identity
type Ballot = BallotT Identity
type BlockMeta = BlockMetaT Identity
type Council = CouncilT Identity
type StkrProposal = StkrProposalT Identity
type Policy = PolicyT Identity
type Vote = VoteT Identity

deriving instance Show PeriodMeta
deriving instance Show (PrimaryKey PeriodMetaT Identity)
deriving instance Show Voter
deriving instance Show (PrimaryKey VoterT Identity)
deriving instance Show Proposal
deriving instance Show (PrimaryKey ProposalT Identity)
deriving instance Show ProposalVote
deriving instance Show (PrimaryKey ProposalVoteT Identity)
deriving instance Show Ballot
deriving instance Show (PrimaryKey BallotT Identity)
deriving instance Show BlockMeta
deriving instance Show (PrimaryKey BlockMetaT Identity)

deriving instance Show Council
deriving instance Show (PrimaryKey CouncilT Identity)
deriving instance Show StkrProposal
deriving instance Show (PrimaryKey StkrProposalT Identity)
deriving instance Show Policy
deriving instance Show (PrimaryKey PolicyT Identity)
deriving instance Show Vote
deriving instance Show (PrimaryKey VoteT Identity)

deriving instance Eq PeriodMeta
deriving instance Eq (PrimaryKey PeriodMetaT Identity)
deriving instance Eq Voter
deriving instance Eq (PrimaryKey VoterT Identity)
deriving instance Eq Proposal
deriving instance Eq (PrimaryKey ProposalT Identity)
deriving instance Eq ProposalVote
deriving instance Eq (PrimaryKey ProposalVoteT Identity)
deriving instance Eq Ballot
deriving instance Eq (PrimaryKey BallotT Identity)
deriving instance Eq BlockMeta
deriving instance Eq (PrimaryKey BlockMetaT Identity)

deriving instance Eq Council
deriving instance Eq (PrimaryKey CouncilT Identity)
deriving instance Eq StkrProposal
deriving instance Eq (PrimaryKey StkrProposalT Identity)
deriving instance Eq Policy
deriving instance Eq (PrimaryKey PolicyT Identity)
deriving instance Eq Vote
deriving instance Eq (PrimaryKey VoteT Identity)

---------------------------------------------------------------------------
-- `Table` and `Beamable` instances
---------------------------------------------------------------------------

instance Table PeriodMetaT where
  newtype PrimaryKey PeriodMetaT f = PeriodMetaId {unPeriodMetaId :: C f PeriodId}
    deriving (Generic)
  primaryKey = PeriodMetaId . pmId

instance Table VoterT where
  newtype PrimaryKey VoterT f = VoterHash {unVoterHash :: C f PublicKeyHash}
    deriving (Generic)
  primaryKey = VoterHash . voterPbkHash

instance Table ProposalT where
  newtype PrimaryKey ProposalT f = ProposalId {unProposalId :: C f (SqlSerial Int)}
    deriving (Generic)
  primaryKey = ProposalId . prId

instance Table ProposalVoteT where
  newtype PrimaryKey ProposalVoteT f = ProposalVoteId (C f (SqlSerial Int))
    deriving (Generic)
  primaryKey = ProposalVoteId . pvId

instance Table BallotT where
  newtype PrimaryKey BallotT f = BallotId (C f (SqlSerial Int))
    deriving (Generic)
  primaryKey = BallotId . bId

instance Table BlockMetaT where
  newtype PrimaryKey BlockMetaT f = BlockMetaId {unBlockMetaId :: C f Level}
    deriving (Generic)
  primaryKey = BlockMetaId . blLevel
  
instance Table CouncilT where
  newtype PrimaryKey CouncilT f = CouncilId (C f PublicKeyHash)
    deriving (Generic)
  primaryKey = CouncilId . cPbkHash
  
instance Table StkrProposalT where
  newtype PrimaryKey StkrProposalT f = StkrProposalId (C f (SqlSerial Int))
    deriving (Generic)
  primaryKey = StkrProposalId . spId

instance Table PolicyT where
  newtype PrimaryKey PolicyT f = PolicyId (C f PolicyHash)
    deriving (Generic)
  primaryKey = PolicyId . pcHash

instance Table VoteT where
  newtype PrimaryKey VoteT f = VoteId (C f (SqlSerial Int))
    deriving (Generic)
  primaryKey = VoteId . vId

instance Beamable PeriodMetaT
instance Beamable (PrimaryKey PeriodMetaT)

instance Beamable VoterT
instance Beamable (PrimaryKey VoterT)

instance Beamable ProposalT
instance Beamable (PrimaryKey ProposalT)

instance Beamable ProposalVoteT
instance Beamable (PrimaryKey ProposalVoteT)

instance Beamable BallotT
instance Beamable (PrimaryKey BallotT)

instance Beamable BlockMetaT
instance Beamable (PrimaryKey BlockMetaT)

instance Beamable CouncilT
instance Beamable (PrimaryKey CouncilT)

instance Beamable StkrProposalT
instance Beamable (PrimaryKey StkrProposalT)

instance Beamable PolicyT
instance Beamable (PrimaryKey PolicyT)

instance Beamable VoteT
instance Beamable (PrimaryKey VoteT)

---------------------------------------------------------------------------
-- Database schema definition and initialization
---------------------------------------------------------------------------

data AgoraSchema f = AgoraSchema
  { asPeriodMetas   :: f (TableEntity PeriodMetaT)
  , asVoters        :: f (TableEntity VoterT)
  , asProposals     :: f (TableEntity ProposalT)
  , asProposalVotes :: f (TableEntity ProposalVoteT)
  , asBallots       :: f (TableEntity BallotT)
  , asBlockMetas    :: f (TableEntity BlockMetaT)
  , asCouncil       :: f (TableEntity CouncilT)
  , asStkrProposals :: f (TableEntity StkrProposalT)
  , asPolicy        :: f (TableEntity PolicyT)
  , asVotes         :: f (TableEntity VoteT)
  } deriving (Generic)

instance Database be AgoraSchema

-- | Agora DB schema naming settings.
-- The schema is assumed to have names of form "proposal_votes" (for tables),
-- "casted_rolls" (for fields) or "proposal__id" (for foreign keys).
agoraSchema :: DatabaseSettings Postgres AgoraSchema
agoraSchema = defaultDbSettings

-- | Schema definition in SQL.
-- TODO: should be a pre-generated file, dunno if there is a way to
-- obtain it automatically from `AgoraSchema` (which is stupid)
agoraSchemaDefinition :: IsString s => s
agoraSchemaDefinition = $(embedStringFile "database/schema.sql")

-- | Create tables if absent.
ensureSchemaIsSetUp :: MonadBeam Postgres m => m ()
ensureSchemaIsSetUp = runNoReturn $
  PgCommandSyntax PgCommandTypeDataUpdate $ emit agoraSchemaDefinition

-- | Clears all the data from the database (used in tests)
resetSchema :: MonadBeam Postgres m => m ()
resetSchema = runNoReturn $
  PgCommandSyntax PgCommandTypeDataUpdate $
  emit "set client_min_messages to warning;\
       \drop schema public cascade;\
       \set client_min_messages to notice;\
       \create schema public;"
