module Agora.Web.Types
       ( Proposal (..)
       , StageType (..)
       , StageItemInfo (..)
       , VoteStats (..)
       , StageInfo (..)
       , ProposalVote (..)
       , iTotalStages
       , iStageTimes
       , piWinner
       , prId
       , prDiscourseLink
       , pvId
       ) where

import Data.Aeson.Options (defaultOptions)
import Data.Aeson.TH (deriveJSON)
import Data.Time.Clock (UTCTime)
import Fmt (Buildable (..))
import Lens.Micro.Platform (makeLensesFor)
import Servant.Util (ForResponseLog (..), buildListForResponse)

import Agora.Types
import Agora.Util

-- | Full info about the stage.
data StageInfo
  = ProposalInfo
  { _iStageType   :: !StageType
  , _iStage       :: !Stage           -- ^ Stage
  , _iTotalStages :: !Word32          -- ^ Total number of stages so far
  , _iStageTimes  :: ![StageItemInfo] -- ^ Info about start and end times of all stages
  , _iDiscourseLink :: !Text
  }
  | EvaluationInfo
  { _iStageType   :: !StageType
  , _iStage       :: !Stage
  , _iTotalStages :: !Word32
  , _iStageTimes  :: ![StageItemInfo]
  , _iDiscourseLink :: !Text
  }
  | VotingInfo
  { _iStageType     :: !StageType
  , _iStage         :: !Stage
  , _iTotalStages   :: !Word32
  , _iStageTimes    :: ![StageItemInfo]
  , _piVoteStats    :: !VoteStats
  , _piWinner       :: !(Maybe Proposal)
  , _iDiscourseLink :: !Text
  }
  | ImplementationInfo
  { _iStageType   :: !StageType
  , _iStage       :: !Stage
  , _iTotalStages :: !Word32
  , _iStageTimes  :: ![StageItemInfo]
  , _tiProposal   :: !Proposal
  , _iDiscourseLink :: !Text
  } deriving (Show, Eq, Generic)

-- | Info about the proposal.
data Proposal = Proposal
  { _prId               :: !ProposalId   -- ^ Proposal ordering ID (autoincrement in DB)
  , _prStage            :: !Stage        -- ^ Stages id where proposal was sent
  , _prHash             :: !ProposalHash -- ^ Proposal hash (serves as ID)
  , _prTitle            :: !(Maybe Text) -- ^ Proposal title
  , _prShortDescription :: !(Maybe Text) -- ^ Short description
  , _prLongDescription  :: !(Maybe Text) -- ^ Long description
  , _prTimeCreated      :: !UTCTime      -- ^ Time the proposal has been proposed
  , _prProposalFile     :: !(Maybe Text) -- ^ Link to the proposal file, if present
  , _prDiscourseLink    :: !(Maybe Text) -- ^ Link to the Discourse discussion, if present
  , _prVotesCasted      :: !Votes        -- ^ Votes are cast for this proposal so far
  } deriving (Show, Eq, Generic)

-- | Info only about start and end times of stage (for displaying in the nav dropdown)
data StageItemInfo = StageItemInfo
  { _piiStage     :: !Stage
  , _piiStageType :: !StageType
  } deriving (Show, Eq, Generic)

-- | Vote for the proposal to be considered in the proposal stage.
data ProposalVote = ProposalVote
  { _pvId            :: !ProposalVoteId -- ^ Proposal vote ordering ID (autoincrement in DB)
  , _pvProposal      :: !ProposalHash   -- ^ Hash of the corresponding proposal
  , _pvProposalTitle :: !(Maybe Text)   -- ^ Title of the corresponding proposal, if present (on Discourse)
  , _pvAuthor        :: !PublicKeyHash  -- ^ Vote author
  , _pvTimestamp     :: !UTCTime        -- ^ Time the vote has been cast
  } deriving (Show, Eq, Generic)

-- | Delegates participation info.
data VoteStats = VoteStats
  { _vsNumVoters      :: !Voters -- ^ The number of the bakers voted in this stage
  , _vsNumVotersTotal :: !Voters -- ^ The number of bakers who can vote in this stage
  } deriving (Show, Eq, Generic)

instance HasId Proposal where
  type IdT Proposal = ProposalId
  getId = _prId

instance HasId ProposalVote where
  type IdT ProposalVote = ProposalVoteId
  getId = _pvId

instance Buildable Proposal where
  build = buildFromJSON

instance Buildable ProposalVote where
  build = buildFromJSON

instance Buildable StageInfo where
  build = buildFromJSON

deriving instance Buildable (ForResponseLog Proposal)
deriving instance Buildable (ForResponseLog ProposalVote)
deriving instance Buildable (ForResponseLog StageInfo)

instance Buildable (ForResponseLog [Proposal]) where
    build = buildListForResponse (take 5)

instance Buildable (ForResponseLog [ProposalVote]) where
    build = buildListForResponse (take 5)

makeLensesFor [ ("_iTotalStages", "iTotalStages")
              , ("_piWinner", "piWinner")
              , ("_eiProposal", "eiProposal")
              , ("_iStageTimes", "iStageTimes")
              ] ''StageInfo
makeLensesFor [("_piiEndTime", "piiEndTime")] ''StageItemInfo
makeLensesFor [("_prId", "prId"), ("_prDiscourseLink", "prDiscourseLink")] ''Proposal
makeLensesFor [("_pvId", "pvId")] ''ProposalVote

deriveJSON defaultOptions ''Proposal
deriveJSON defaultOptions ''StageItemInfo
deriveJSON defaultOptions ''VoteStats
deriveJSON defaultOptions ''StageInfo
deriveJSON defaultOptions ''ProposalVote
