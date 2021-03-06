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

data BlockMetaT f = BlockMeta
  { blLevel            :: C f Level
  , blHash             :: C f BlockHash
  , blPredecessor      :: C f BlockHash
  , blBlockTime        :: C f UTCTime
  } deriving (Generic)

data CouncilT f = Council
  { cPbkHash    :: C f PublicKeyHash
  , cStage      :: C f Stage
  } deriving (Generic)

data StkrProposalT f = StkrProposal
  { spId                 :: C f Int
  , spStage              :: C f Stage
  , spEpoch             :: C f Epoch
  , spHash               :: C f ProposalHash
  , spTimeProposed       :: C f UTCTime
  , spDescription        :: C f Text

  , spDiscourseTitle     :: C (Nullable f) Text
  , spDiscourseShortDesc :: C (Nullable f) Text
  , spDiscourseLongDesc  :: C (Nullable f) Text
  , spDiscourseTopicId   :: C (Nullable f) DiscourseTopicId
  , spDiscoursePostId    :: C (Nullable f) DiscoursePostId
  } deriving (Generic)

data VoteT f = Vote
  { vSeq            :: C f (SqlSerial Int)
  , vStage          :: C f Stage
  , vEpoch          :: C f Epoch
  , vVoterPbkHash   :: C f PublicKeyHash
  , vProposalNumber :: C f Int
  , vVoteTime      :: C f UTCTime
  } deriving (Generic)

data PolicyT f = Policy
  { pProposalId  :: C f Int
  , pEpoch      :: C f Epoch
  , pHash        :: C f UrlHash
  , pDescription :: C f Text
  , pUrl         :: C f Text
  } deriving (Generic)

type BlockMeta = BlockMetaT Identity
type Council = CouncilT Identity
type StkrProposal = StkrProposalT Identity
type Vote = VoteT Identity
type Policy = PolicyT Identity

deriving instance Show BlockMeta
deriving instance Show (PrimaryKey BlockMetaT Identity)

deriving instance Show Council
deriving instance Show (PrimaryKey CouncilT Identity)
deriving instance Show StkrProposal
deriving instance Show (PrimaryKey StkrProposalT Identity)
deriving instance Show Vote
deriving instance Show (PrimaryKey VoteT Identity)
deriving instance Show Policy
deriving instance Show (PrimaryKey PolicyT Identity)

deriving instance Eq BlockMeta
deriving instance Eq (PrimaryKey BlockMetaT Identity)

deriving instance Eq Council
deriving instance Eq (PrimaryKey CouncilT Identity)
deriving instance Eq StkrProposal
deriving instance Eq (PrimaryKey StkrProposalT Identity)
deriving instance Eq Vote
deriving instance Eq (PrimaryKey VoteT Identity)
deriving instance Eq Policy
deriving instance Eq (PrimaryKey PolicyT Identity)

---------------------------------------------------------------------------
-- `Table` and `Beamable` instances
---------------------------------------------------------------------------

instance Table BlockMetaT where
  newtype PrimaryKey BlockMetaT f = BlockMetaId {unBlockMetaId :: C f Level}
    deriving (Generic)
  primaryKey = BlockMetaId . blLevel

instance Table CouncilT where
  data PrimaryKey CouncilT f = CouncilId (C f PublicKeyHash) (C f Stage)
    deriving (Generic)
  primaryKey c = CouncilId (cPbkHash c) (cStage c)

instance Table StkrProposalT where
  data PrimaryKey StkrProposalT f = StkrProposalId (C f Int) (C f Epoch)
    deriving (Generic)
  primaryKey p = StkrProposalId (spId p) (spEpoch p)

instance Table VoteT where
  data PrimaryKey VoteT f = VoteId (C f Epoch) (C f Stage) (C f PublicKeyHash) (C f Int)
    deriving (Generic)
  primaryKey v = VoteId (vEpoch v) (vStage v) (vVoterPbkHash v) (vProposalNumber v)

instance Table PolicyT where
  data PrimaryKey PolicyT f = PolicyId (C f Int) (C f Epoch) (C f UrlHash)
    deriving (Generic)
  primaryKey p = PolicyId (pProposalId p) (pEpoch p) (pHash p)

instance Beamable BlockMetaT
instance Beamable (PrimaryKey BlockMetaT)

instance Beamable CouncilT
instance Beamable (PrimaryKey CouncilT)

instance Beamable StkrProposalT
instance Beamable (PrimaryKey StkrProposalT)

instance Beamable VoteT
instance Beamable (PrimaryKey VoteT)

instance Beamable PolicyT
instance Beamable (PrimaryKey PolicyT)

---------------------------------------------------------------------------
-- Database schema definition and initialization
---------------------------------------------------------------------------

data AgoraSchema f = AgoraSchema
  { asBlockMetas    :: f (TableEntity BlockMetaT)
  , asCouncil       :: f (TableEntity CouncilT)
  , asStkrProposals :: f (TableEntity StkrProposalT)
  , asVotes         :: f (TableEntity VoteT)
  , asPolicy        :: f (TableEntity PolicyT)
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
