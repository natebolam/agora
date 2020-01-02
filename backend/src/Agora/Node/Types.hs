{-# LANGUAGE DataKinds #-}

{-|
Servant type-level specification for Client API to retrieve data from Tezos Node.
-}
module Agora.Node.Types
     ( BlockId (..)
     , ChainId (..)
     , Block (..)
     , blockTimestamp
     , BlockMetadata (..)
     , Operations (..)
     , Operation (..)
     , source
     , opHash
     , isProposalOp
     , isBallotOp
     , BlockHead (..)
     , BlockHeader (..)
     , block2Head
     , headWPred

      -- * Useful functions
     , parseUTCTime

      -- * Predefined data
     , block1
     , blockHead1
     , metadata1
     , genesisBlockHead
     ) where

import Prelude hiding (cycle, id)

import Data.Aeson (FromJSON (..), Object, ToJSON (..), Value (..), encode, withArray, withObject,
                   withText, (.:))
import Data.Aeson.Options (defaultOptions)
import Data.Aeson.TH (deriveJSON)
import Data.Aeson.Types (Parser, object, (.=))
import Data.Time.Clock (UTCTime)
import Data.Time.Format (defaultTimeLocale, parseTimeOrError)
import Fmt (Buildable (..), Builder, (+|), (|+))
import Servant.API (ToHttpApiData (..), FromHttpApiData (..))

import Agora.Types
import Data.Text (unpack)
import Data.Text.Read (decimal)

-- | Block id.
-- A block can be reffered by 'head', 'genesis', level or block hash
data BlockId
  = HeadRef
  | GenesisRef
  | LevelRef Level
  | BlockHashRef BlockHash
-- pva701: Relative indexing allowing to use 'head~N', '<hash>+N', etc.
-- as block reference may be implemented later, if it's really needed.

-- | Chain id.
-- Either mainnet or testnet (alphanet).
data ChainId
  = MainChain
  | TestChain
  deriving (Eq, Show)

instance FromHttpApiData ChainId where
  parseQueryParam "main"  = Right MainChain
  parseQueryParam "test"  = Right TestChain
  parseQueryParam _ = Left "unexpected decision param"

instance ToHttpApiData ChainId where
  toUrlPiece MainChain = "main"
  toUrlPiece TestChain = "test"

-- | Operations related to voting.
data Operation
  = ProposalOp OperationHash PublicKeyHash PeriodId [ProposalHash]
  | BallotOp OperationHash PublicKeyHash PeriodId ProposalHash Decision
  deriving (Generic, Show, Eq)

isProposalOp :: Operation -> Bool
isProposalOp ProposalOp{} = True
isProposalOp _            = False

isBallotOp :: Operation -> Bool
isBallotOp BallotOp{} = True
isBallotOp _          = False

source :: Operation -> PublicKeyHash
source (ProposalOp _ s _ _) = s
source (BallotOp _ s _ _ _) = s

opHash :: Operation -> OperationHash
opHash (ProposalOp op _ _ _) = op
opHash (BallotOp op _ _ _ _) = op

-- | List of operations related to voting.
newtype Operations = Operations {unOperations :: [Operation]}
  deriving (Generic, Show, Eq)

-- | Subset of fields of a block metadata
data BlockMetadata = BlockMetadata
  { bmLevel                :: Level
  , bmCycle                :: Cycle
  , bmCyclePosition        :: Word32
  , bmVotingPeriod         :: PeriodId
  , bmVotingPeriodPosition :: Word32
  , bmVotingPeriodType     :: StageType
  } deriving (Generic, Show, Eq)

-- | Subset of fields of a result of /monitor/heads call
data BlockHead = BlockHead
  { bhHash        :: BlockHash
  , bhLevel       :: Level
  , bhPredecessor :: BlockHash
  } deriving (Generic, Show, Eq)

data BlockHeader = BlockHeader
  { bhrPredecessor :: BlockHash
  , bhrTimestamp   :: UTCTime
  } deriving (Generic, Show, Eq)

-- | Subset of fields of a block
data Block = Block
  { bHash       :: BlockHash
  , bHeader     :: BlockHeader
  , bOperations :: Operations
  , bMetadata   :: BlockMetadata
  } deriving (Generic, Show, Eq)

block2Head :: Block -> BlockHead
block2Head Block{..} = BlockHead bHash (bmLevel bMetadata) (bhrPredecessor bHeader)

blockTimestamp :: Block -> UTCTime
blockTimestamp Block{..} = bhrTimestamp bHeader

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

instance Buildable Block where
  build b = fromString $ decodeUtf8 $ encode b

instance FromJSON Operations where
  parseJSON = withArray "Operations" $ \a -> Operations <$>
      concatMapM (withArray "Operations_inner" (concatMapM parseOp)) (toList a)
    where
      parseOp :: Value -> Parser [Operation]
      parseOp = withObject "Operation" $ \o -> do
        oph <- o .: "hash"
        contents <- o .: "contents"
        opsMb <- withArray "Operation.Alpha"
                    (mapM (withObject "Operation.Alpha.element" (parseOpEl oph)))
                    contents
        pure $ catMaybes $ toList opsMb

      parseOpEl :: OperationHash -> Object -> Parser (Maybe Operation)
      parseOpEl oph o = do
        kind <- o .: "kind"
        flip (withText "Operation.Kind") kind $ \case
          "proposals" -> fmap Just $
            ProposalOp oph
              <$> (o .: "source")
              <*> (o .: "period")
              <*> (o .: "proposals")
          "ballot"    -> fmap Just $
            BallotOp oph
              <$> (o .: "source")
              <*> (o .: "period")
              <*> (o .: "proposal")
              <*> (o .: "ballot")
          _   -> pure Nothing

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

instance ToHttpApiData BlockId where
  toUrlPiece HeadRef              = "head"
  toUrlPiece GenesisRef           = "genesis"
  toUrlPiece (LevelRef (Level x)) = toUrlPiece x
  toUrlPiece (BlockHashRef hash)  = toUrlPiece hash

instance FromHttpApiData BlockId where
  parseQueryParam "head"  = Right HeadRef
  parseQueryParam "genesis"  = Right GenesisRef
  parseQueryParam a = case decimal a of
    Right (x, unpack->"") -> Right $ LevelRef (Level x)
    _ -> Right $ BlockHashRef $ encodeHash a

---------------------------------------------------------------------------
-- Predefined data from the real blockchain
---------------------------------------------------------------------------

parseUTCTime :: String -> UTCTime
parseUTCTime = parseTimeOrError False defaultTimeLocale "%Y-%m-%dT%H:%M:%SZ"

-- pva701: The reason of this hardcoding that
-- chains/main/blocks/1 returns block,
-- where "metadata" field doesn't contain "level" field
-- I hope this exception is only for the first block
-- We could just prefill a database with the first block
-- but I found this hardcoding more robust, because we don't need to
-- refill the database every time when we change its format.
block1 :: Block
block1 = Block
  { bHash = encodeHash "BKoXjPWmhM4BYTG6JiSYPYvak7AJ9GuebQLkchzcKp1NvyQPpsQ"
  , bHeader = BlockHeader
    { bhrPredecessor = encodeHash "BL4qALVg56d6Ds4j43Gpby7SutqRF9KAvLcuSK9b3niogEbs8jH"
    , bhrTimestamp = parseUTCTime "2019-12-30T04:58:46Z"
    }
  , bOperations = Operations []
  , bMetadata = metadata1
  }

metadata1 :: BlockMetadata
metadata1 = BlockMetadata
    { bmLevel = Level 170000
    , bmCycle = Cycle 0
    , bmCyclePosition = 0
    , bmVotingPeriod = Id 0
    , bmVotingPeriodPosition = 0
    , bmVotingPeriodType = Proposing
    }

blockHead1 :: BlockHead
blockHead1 = BlockHead
  { bhHash = encodeHash "BKoXjPWmhM4BYTG6JiSYPYvak7AJ9GuebQLkchzcKp1NvyQPpsQ"
  , bhLevel = Level 170000
  , bhPredecessor = encodeHash "BL4qALVg56d6Ds4j43Gpby7SutqRF9KAvLcuSK9b3niogEbs8jH"
  }

genesisBlockHead :: BlockHead
genesisBlockHead = BlockHead
  { bhHash = encodeHash "BLuv1F97kJdz66uxikwsioA4QfwmE78hhxBvoCPHcBzTATsvijU"
  , bhLevel = Level 169999
  , bhPredecessor = encodeHash "BMDinXtFUNkXiwrP2yUmLvHjhzm9sVzbU5Myjnryso8h2qzpEnB"
  }

deriveJSON defaultOptions ''BlockHead
deriveJSON defaultOptions ''BlockHeader
deriveJSON defaultOptions ''Block

instance ToJSON Operation where
  toJSON (ProposalOp hash keyHash id proposalHashs) =
    object
      [ "hash" .= hash
      , "contents" .=
        [ object
            ["kind" .= ("proposals" :: Text), "source" .= keyHash, "period" .= id, "proposals" .= toJSON proposalHashs]
        ]
      ]
  toJSON (BallotOp hash keyHash id proposalHash decision) =
    object
      [ "hash" .= hash
      , "contents" .=
        [ object
            [ "kind" .= ("ballot" :: Text)
            , "source" .= keyHash
            , "period" .= id
            , "proposal" .= proposalHash
            , "ballot" .= decision
            ]
        ]
      ]

instance ToJSON Operations where
  toJSON (Operations ops) = toJSONList [toJSON ops]

instance ToJSON BlockMetadata where
  toJSON (BlockMetadata level cycle cyclePosition votingPeriod votingPeriodPosition votingPeriodType) =
    object
      [ "voting_period_kind" .= votingPeriodType
      , "level" .=
        object
          [ "level" .= level
          , "cycle" .= cycle
          , "cycle_position" .= cyclePosition
          , "voting_period" .= votingPeriod
          , "voting_period_position" .= votingPeriodPosition
          ]
      ]