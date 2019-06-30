{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

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
     ) where

import Data.Aeson (FromJSON (..), Object, ToJSON (..), Value, withArray, withObject, withText, (.:))
import Data.Aeson.Options (defaultOptions)
import Data.Aeson.TH (deriveJSON)
import Data.Aeson.Types (Parser)
import Servant.API (ToHttpApiData (..))

import Agora.Types

-- | Block id.
-- A block can be reffered by 'head', 'genesis', level or block hash
data BlockId
  = HeadRef
  | GenesisRef
  | LevelRef Word32
  | BlockHashRef BlockHash
-- pva701: Relative indexing allowing to use 'head~N', '<hash>+N', etc.
-- as block reference may be implemented later, if it's really needed.

instance ToHttpApiData BlockId where
  toUrlPiece HeadRef             = "head"
  toUrlPiece GenesisRef          = "genesis"
  toUrlPiece (LevelRef x)        = toUrlPiece x
  toUrlPiece (BlockHashRef hash) = toUrlPiece hash

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
  = ProposalOp OperationHash PublicKeyHash PeriodNum [ProposalHash]
  | BallotOp OperationHash PublicKeyHash PeriodNum ProposalHash Decision
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
  { bhBlockHash :: BlockHash
  } deriving (Generic, Show, Eq)

instance FromJSON BlockHead where
  parseJSON = withObject "BlockHead" $ \o -> BlockHead <$> (o .: "hash")

-- | Subset of fields of a block

data Block = Block
  { bHash       :: BlockHash
  , bOperations :: Operations
  , bMetadata   :: BlockMetadata
  } deriving (Generic, Show, Eq)

-- | Subset of fields of a block metadata
data BlockMetadata = BlockMetadata
  { bmLevel                :: Level
  , bmCycle                :: Cycle
  , bmCyclePosition        :: Word32
  , bmVotingPeriod         :: PeriodNum
  , bmVotingPeriodPosition :: Word32
  , bmVotingPeriodType     :: PeriodType
  } deriving (Generic, Show, Eq)

instance FromJSON BlockMetadata where
  parseJSON = withObject "BlockMetadata" $ \o -> do
    level <- o .: "level"
    flip (withObject "BlockMetadata.level") level $ \lv -> do
      bmLevel <- lv .: "level"
      bmCycle <- lv .: "cycle"
      bmCyclePosition <- lv .: "cycle_position"
      bmVotingPeriod <- lv .: "voting_period"
      bmVotingPeriodPosition <- lv .: "voting_period_position"
      bmVotingPeriodType <- o .: "voting_period_kind"
      pure $ BlockMetadata {..}

-- Pay attention that these instances
-- don't satisfy a == decode (encode a).
-- They are needed only for logging
instance ToJSON Operation
instance ToJSON Operations
instance ToJSON BlockMetadata
deriveJSON defaultOptions ''Block
