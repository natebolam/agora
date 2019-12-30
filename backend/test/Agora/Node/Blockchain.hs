{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module Agora.Node.Blockchain
      ( BlockChain (..)
      , bcLen
      , appendBlock
      , rewriteBlock
      , testTzConstants
      , genEmptyBlockChain
      , genBlockChainSkeleton
      , takeBlocks
      , block2HeadSafe
      , modifyBlock
      , appendGenBlock
      , distributeOperations
      , genesisBlockChain
      , bcHead
      , bcStable
      , genesisBlock
      , getBlock
      , block2
      ) where

import Data.List ((!!))
import qualified Data.Map as M
import Data.Time.Clock (NominalDiffTime, addUTCTime)
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as VM
import Test.QuickCheck (Gen, arbitrary, choose, vectorOf)

import Agora.Arbitrary ()
import Agora.Node
import Agora.Types

data BlockChain = BlockChain
  { bcBlocks     :: !(Map BlockHash Block)
  , bcBlocksList :: !(V.Vector Block)
  } deriving (Show, Generic)

bcLen :: BlockChain -> Level
bcLen BlockChain{..} = fromIntegral (V.length bcBlocksList) - 1

appendBlock :: Block -> BlockChain -> BlockChain
appendBlock block BlockChain{..} =
  let level = fromIntegral $ bmLevel $ bMetadata block in
  if level == V.length bcBlocksList then
    BlockChain (M.insert (bHash block) block bcBlocks) (V.snoc bcBlocksList block)
  else
    error "block is not a continuation of the chain"

rewriteBlock :: Block -> BlockChain -> BlockChain
rewriteBlock block bc@BlockChain{..} =
  let level = fromIntegral $ bmLevel $ bMetadata block in
  if level == V.length bcBlocksList then appendBlock block bc
  else if level < V.length bcBlocksList then
    let prevBlock = bcBlocksList V.! level in
    BlockChain (M.insert (bHash block) block $ M.delete (bHash prevBlock) $ bcBlocks)
               (V.modify (\mv -> VM.write mv level block) bcBlocksList)
  else error "slot for rewriting block doesn't exist yet"

testTzConstants :: TzConstants
testTzConstants = TzConstants
  { tzEmptyPeriods = Id 0
  , tzCycleLength = 64
  , tzNumOfCycles = 8
  }

-- Generate n+1 sequential blocks: 0th is genesis one (always the same)
-- other n blocks are generated.
genEmptyBlockChain :: Int32 -> Gen BlockChain
genEmptyBlockChain = genBlockChainSkeleton []

genBlockChainSkeleton :: [PeriodType] -> Int32 -> Gen BlockChain
genBlockChainSkeleton periodTypes n = do
  unless (checkTypesConsistent periodTypes) $
    error "period types are not consistent"

  blocks <- genBlocks 2 [block1, genesisBlock]
  pure $ BlockChain
    (M.fromList $ zip (map bHash blocks) blocks)
    (V.fromList $ reverse blocks)
  where
    TzConstants{..} = testTzConstants
    onePeriod' = fromIntegral $ tzOnePeriod testTzConstants
    genBlocks :: Int32 -> [Block] -> Gen [Block]
    genBlocks _ [] = error "impossible"
    genBlocks !lev blocks@(lst : _) = do
      let period = fromIntegral $ (lev - 1) `div` onePeriod'
      let periodType = if period < length periodTypes then periodTypes !! period else Proposing
      let metadata = BlockMetadata
            { bmLevel = Level lev
            , bmCycle = Cycle $ (lev - 1) `div` fromIntegral tzCycleLength
            , bmCyclePosition = fromIntegral $ (lev - 1) `mod` fromIntegral tzCycleLength
            , bmVotingPeriod = fromIntegral period
            , bmVotingPeriodPosition = fromIntegral $ (lev - 1) `mod` onePeriod'
            , bmVotingPeriodType = periodType
            }
      hash <- arbitrary
      let oneMinute = 60 :: NominalDiffTime
      let prevTime = blockTimestamp lst
      let block = Block
                    { bHash = hash
                    , bOperations = Operations []
                    , bMetadata = metadata
                    , bHeader = BlockHeader (bHash lst) (addUTCTime oneMinute prevTime)
                    }
      if lev < n then
        genBlocks (lev + 1) (block : blocks)
      else
        pure (block : blocks)

takeBlocks :: Int -> BlockChain -> BlockChain
takeBlocks ((+1) -> n) BlockChain{..} =
  let blocks = toList bcBlocksList in
    BlockChain
      (M.fromList $ zip (map bHash blocks)  blocks)
      (V.take n bcBlocksList)

appendGenBlock
  :: PeriodType
  -> Operation
  -> BlockChain
  -> Gen BlockChain
appendGenBlock ptype op bc@BlockChain{..} = do
  let TzConstants{..} = testTzConstants
  let level = fromIntegral (V.length bcBlocksList - 1)
  let lst = V.last bcBlocksList
  let onePeriod = tzOnePeriod testTzConstants
  let metadata = BlockMetadata
        { bmLevel                = Level (level + 1)
        , bmCycle                = Cycle $ level `div` fromIntegral tzCycleLength
        , bmCyclePosition        = fromIntegral level `mod` fromIntegral tzCycleLength
        , bmVotingPeriod         = Id $ level `div` fromIntegral onePeriod
        , bmVotingPeriodPosition = fromIntegral level `mod` fromIntegral onePeriod
        , bmVotingPeriodType     = ptype
        }
  hash <- arbitrary
  let oneMinute = 60 :: NominalDiffTime
  let operations = Operations $ one op
  let block = Block
                { bHash = hash
                , bOperations = operations
                , bMetadata = metadata
                , bHeader = BlockHeader
                              (bHash lst)
                              (addUTCTime oneMinute $ blockTimestamp lst)
                }
  pure $ appendBlock block bc

modifyBlock
  :: Level
  -> Operations
  -> BlockChain
  -> BlockChain
modifyBlock (fromIntegral -> lev) ops'@(Operations ops) BlockChain{..} =
  let curBlock = bcBlocksList V.! lev in
  let newBlock =
        case bmVotingPeriodType (bMetadata curBlock) of
          Exploration
            | any isProposalOp ops -> error "inconsistent blockchain: proposal operation in exploration period"
          Promotion
            | any isProposalOp ops -> error "inconsistent blockchain: proposal operation in promotion period"
          Proposing
            | any isBallotOp ops   -> error "inconsistent blockchain: ballot operation in proposal period"
          Testing
            | not (null ops)       -> error "inconsistent blockchain: not empty list of operation in testing period"
          _ -> curBlock {bOperations = ops'} in
  let newBlocksList = V.modify (\mv -> VM.write mv lev newBlock) bcBlocksList in
  let newBlocks = M.insert (bHash curBlock) newBlock bcBlocks in
  BlockChain newBlocks newBlocksList

-- | Fill blocks with operations.
-- The order of operations will be preserved.
distributeOperations
  :: [Operation]
  -> (Level, Level) -- blocks range to fill
  -> BlockChain -- blockchain to update
  -> Gen ( BlockChain -- updated blockchain
         , [BlockHash]    -- block hashes where operations belong to
         )
distributeOperations initOps range initBc = do
  let numOps = length initOps
  levs <- vectorOf numOps $ choose range
  let count = M.toList . foldl (\mp x -> M.alter (\case
                                          Nothing -> Just 1
                                          Just c  -> Just (c + 1)
                                          ) x mp) mempty
  let levsWithLen = sortOn fst $ count levs
  let fillLevel (bc, bkhs, ops) (lev, cnt) =
        (modifyBlock lev (Operations $ take cnt ops) bc
        , replicate cnt (bHash $ bcBlocksList bc V.! fromIntegral lev) ++ bkhs
        , drop cnt ops)
  let (resBs, resBkhs, _) = foldl fillLevel (initBc, [], initOps) levsWithLen
  pure (resBs, reverse resBkhs)

genesisBlockChain :: BlockChain
genesisBlockChain =
  BlockChain (M.singleton (bHash genesisBlock) genesisBlock) (V.singleton genesisBlock)

genesisBlock :: Block
genesisBlock = Block
  { bHash = encodeHash ("BLockGenesisGenesisGenesisGenesisGenesisf79b5d1CoW2" :: Text)
  , bOperations = Operations []
  , bMetadata = error "Imitating absence of metadata field in the real genesis block"
  , bHeader = BlockHeader
      { bhrPredecessor = encodeHash "BLockGenesisGenesisGenesisGenesisGenesisf79b5d1CoW2"
      , bhrTimestamp = parseUTCTime "2018-06-30T16:07:32Z"
      }
  }

bcHead :: BlockChain -> Block
bcHead BlockChain {..} = V.last bcBlocksList

bcStable :: BlockChain -> Block
bcStable BlockChain {..} = bcBlocksList V.! (V.length bcBlocksList - 2)

getBlock :: BlockChain -> BlockId -> Block
getBlock BlockChain{..} = \case
  HeadRef      -> V.last bcBlocksList
  GenesisRef   -> V.head bcBlocksList
  LevelRef lev -> bcBlocksList V.! (fromIntegral lev)
  BlockHashRef h -> M.findWithDefault (error "block not found") h bcBlocks

block2HeadSafe :: Block -> BlockHead
block2HeadSafe b@Block{..} =
  let genesisHash = encodeHash ("BLockGenesisGenesisGenesisGenesisGenesisf79b5d1CoW2" :: Text) in
  if bHash == genesisHash then
    BlockHead
    { bhHash = genesisHash
    , bhLevel = 0
    , bhPredecessor = genesisHash
    }
  else
    block2Head b

checkTypesConsistent
  :: [PeriodType]
  -> Bool
checkTypesConsistent []          = True
checkTypesConsistent [Proposing] = True
checkTypesConsistent [_]         = False
checkTypesConsistent xs          = checkTypesConsistentDo xs

checkTypesConsistentDo
  :: [PeriodType]
  -> Bool
checkTypesConsistentDo []                             = True
checkTypesConsistentDo [_]                            = True
checkTypesConsistentDo (Proposing : Exploration : xs) = checkTypesConsistentDo (Exploration : xs)
checkTypesConsistentDo (Proposing : Proposing : xs)   = checkTypesConsistentDo (Proposing : xs)
checkTypesConsistentDo (Exploration : Proposing : xs) = checkTypesConsistentDo (Proposing : xs)
checkTypesConsistentDo (Exploration : Testing : xs)   = checkTypesConsistentDo (Testing : xs)
checkTypesConsistentDo (Testing : Promotion : xs)     = checkTypesConsistentDo (Promotion : xs)
checkTypesConsistentDo (Promotion : Proposing : xs)   = checkTypesConsistentDo (Proposing : xs)
checkTypesConsistentDo _                              = False

block2 :: Block
block2 = Block
  { bHash = encodeHash "BLmqJnPDU9iLNXu2DEXTuxqY5URks8aTfbUgh28XHofNkY14mJD"
  , bHeader = BlockHeader
    { bhrPredecessor = encodeHash "BLSqrcLvFtqVCx8WSqkVJypW2kAVRM3eEj2BHgBsB6kb24NqYev"
    , bhrTimestamp = parseUTCTime "2018-06-30T17:40:57Z"
    }
  , bOperations = Operations []
  , bMetadata = BlockMetadata
      { bmLevel = Level 2
      , bmCycle = Cycle 0
      , bmCyclePosition = 1
      , bmVotingPeriod = Id 0
      , bmVotingPeriodPosition = 1
      , bmVotingPeriodType = Proposing
      }
  }
