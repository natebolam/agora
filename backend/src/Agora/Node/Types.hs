{-# LANGUAGE DataKinds #-}

{-|
Servant type-level specification for Client API to retrieve data from Tezos Node.
-}
module Agora.Node.Types
     ( BlockId (..)
     , ChainId (..)
     , Block (..)
     , BlockMetadata (..)
     , Operations (..)
     , Operation (..)
     , BlockHead (..)
     , block2Head
     , headWPred
     , isPeriodStart

      -- * Predefined data
     , onePeriod
     , block1
     , metadata1
     , genesisBlockHead
     ) where

import Data.Aeson (FromJSON (..), Object, ToJSON (..), Value, encode, withArray, withObject,
                   withText, (.:))
import Data.Aeson.Options (defaultOptions)
import Data.Aeson.TH (deriveJSON)
import Data.Aeson.Types (Parser)
import Fmt (Buildable (..), (+|), (|+), Builder)
import Servant.API (ToHttpApiData (..))

import Agora.Types

-- | Block id.
-- A block can be reffered by 'head', 'genesis', level or block hash
data BlockId
  = HeadRef
  | GenesisRef
  | LevelRef Level
  | BlockHashRef BlockHash
-- pva701: Relative indexing allowing to use 'head~N', '<hash>+N', etc.
-- as block reference may be implemented later, if it's really needed.

instance ToHttpApiData BlockId where
  toUrlPiece HeadRef              = "head"
  toUrlPiece GenesisRef           = "genesis"
  toUrlPiece (LevelRef (Level x)) = toUrlPiece x
  toUrlPiece (BlockHashRef hash)  = toUrlPiece hash

-- | Chain id.
-- Either mainnet or testnet (alphanet).
data ChainId
  = MainChain
  | TestChain

instance ToHttpApiData ChainId where
  toUrlPiece MainChain = "main"
  toUrlPiece TestChain = "test"

-- | Operations related to voting.
data Operation
  = ProposalOp OperationHash PublicKeyHash PeriodId [ProposalHash]
  | BallotOp OperationHash PublicKeyHash PeriodId ProposalHash Decision
  deriving (Generic, Show, Eq)

-- | List of operations related to voting.
newtype Operations = Operations [Operation]
  deriving (Generic, Show, Eq)

instance FromJSON Operations where
  parseJSON = withArray "Operations" $ \a -> Operations <$>
      concatMapM (withArray "Operations_inner" (concatMapM parseOp)) (toList a)
    where
      parseOp :: Value -> Parser [Operation]
      parseOp = withObject "Operation" $ \o -> do
        opHash <- o .: "hash"
        contents <- o .: "contents"
        opsMb <- withArray "Operation.Alpha"
                    (mapM (withObject "Operation.Alpha.element" (parseOpEl opHash)))
                    contents
        pure $ catMaybes $ toList opsMb

      parseOpEl :: OperationHash -> Object -> Parser (Maybe Operation)
      parseOpEl opHash o = do
        kind <- o .: "kind"
        flip (withText "Operation.Kind") kind $ \case
          "proposals" -> fmap Just $
            ProposalOp opHash
              <$> (o .: "source")
              <*> (o .: "period")
              <*> (o .: "proposals")
          "ballot"    -> fmap Just $
            BallotOp opHash
              <$> (o .: "source")
              <*> (o .: "period")
              <*> (o .: "proposal")
              <*> (o .: "ballot")
          _   -> pure Nothing

-- | Subset of fields of a result of /monitor/heads call
data BlockHead = BlockHead
  { bhHash        :: BlockHash
  , bhLevel       :: Level
  , bhPredecessor :: BlockHash
  } deriving (Generic, Show, Eq)

instance Buildable BlockHead where
  build BlockHead{..} =
    "Head[hash: " +| bhHash |+
        ", level: " +| bhLevel |+
        "]"

headWPred :: BlockHead -> Builder
headWPred BlockHead{..} =
    "Head[hash: " +| bhHash |+
        ", level: " +| bhLevel |+
        ", predecessor: " +| bhPredecessor |+
        "]"

-- | Subset of fields of a block
data Block = Block
  { bHash        :: BlockHash
  , bOperations  :: Operations
  , bMetadata    :: BlockMetadata
  , bPredecessor :: BlockHash
  } deriving (Generic, Show, Eq)

instance Buildable Block where
  build b = fromString $ decodeUtf8 $ encode b

instance FromJSON Block where
  parseJSON = withObject "Block" $ \o -> do
    bHash <- o .: "hash"
    bOperations <- o .: "operations"
    bMetadata <- o .: "metadata"
    header <- o .: "header"
    flip (withObject "Block.level") header $ \h -> do
      bPredecessor <- h .: "predecessor"
      pure $ Block {..}

block2Head :: Block -> BlockHead
block2Head Block{..} = BlockHead bHash (bmLevel bMetadata) bPredecessor

-- | Subset of fields of a block metadata
data BlockMetadata = BlockMetadata
  { bmLevel                :: Level
  , bmCycle                :: Cycle
  , bmCyclePosition        :: Word32
  , bmVotingPeriod         :: PeriodId
  , bmVotingPeriodPosition :: Word32
  , bmVotingPeriodType     :: PeriodType
  } deriving (Generic, Show, Eq)

isPeriodStart :: BlockMetadata -> Bool
isPeriodStart BlockMetadata{..} = bmLevel `mod` onePeriod == 1

instance FromJSON BlockMetadata where
  parseJSON = withObject "BlockMetadata" $ \o -> do
    bmVotingPeriodType <- o .: "voting_period_kind"
    level <- o .: "level"
    flip (withObject "BlockMetadata.level") level $ \lv -> do
      bmLevel <- lv .: "level"
      bmCycle <- lv .: "cycle"
      bmCyclePosition <- lv .: "cycle_position"
      bmVotingPeriod <- lv .: "voting_period"
      bmVotingPeriodPosition <- lv .: "voting_period_position"
      pure $ BlockMetadata {..}

---------------------------------------------------------------------------
-- Predefined data from the real blockchain
---------------------------------------------------------------------------

onePeriod :: Level
onePeriod = Level $ 8 * 4096

-- pva701: The reason of this hardcoding that
-- chains/main/blocks/1 returns block,
-- where "metadata" field doesn't contain "level" field
-- I hope this exception is only for the first block
-- We could just prefill a database with the first block
-- but I found this hardcoding more robust, because we don't need to
-- refill the database every time when we change its format.
block1 :: Block
block1 = Block
  { bHash = Hash $ encodeUtf8 ("BLSqrcLvFtqVCx8WSqkVJypW2kAVRM3eEj2BHgBsB6kb24NqYev" :: Text)
  , bOperations = Operations []
  , bMetadata = metadata1
  , bPredecessor = Hash $ encodeUtf8 ("BLockGenesisGenesisGenesisGenesisGenesisf79b5d1CoW2" :: Text)
  }

metadata1 :: BlockMetadata
metadata1 = BlockMetadata
    { bmLevel = Level 1
    , bmCycle = Cycle 0
    , bmCyclePosition = 0
    , bmVotingPeriod = Id 0
    , bmVotingPeriodPosition = 0
    , bmVotingPeriodType = Proposing
    }

genesisBlockHead :: BlockHead
genesisBlockHead = BlockHead
  { bhHash = Hash $ encodeUtf8 ("BLockGenesisGenesisGenesisGenesisGenesisf79b5d1CoW2" :: Text)
  , bhLevel = Level 0
  , bhPredecessor = Hash $ encodeUtf8 ("BLockGenesisGenesisGenesisGenesisGenesisf79b5d1CoW2" :: Text)
  }


deriveJSON defaultOptions ''BlockHead

-- Pay attention that these instances
-- don't satisfy a == decode (encode a).
-- They are needed only for logging
instance ToJSON Block
instance ToJSON Operation
instance ToJSON Operations
instance ToJSON BlockMetadata
