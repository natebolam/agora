{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module Agora.Node.Blockchain
      ( BlockChain (..)
      , genBlockChain
      , genesisBlockChain
      , bcHead
      , genesisBlock
      , getBlock
      ) where

import qualified Data.Map as M
import Data.Time.Clock (NominalDiffTime, addUTCTime)
import qualified Data.Vector as V
import Test.QuickCheck (Gen, arbitrary)

import Agora.Arbitrary ()
import Agora.Node.Types
import Agora.Types

data BlockChain = BlockChain
  { bcBlocks     :: !(Map BlockHash Block)
  , bcBlocksList :: !(V.Vector Block)
  } deriving (Show, Generic)

-- Generate n+1 sequential blocks: 0th is genesis one (always the same)
-- other n blocks are generated.
genBlockChain :: Int32 -> Gen BlockChain
genBlockChain n = do
  blocks <- genBlocks 1 [genesisBlock]
  pure $ BlockChain
    (M.fromList $ zip (map bHash blocks) blocks)
    (V.fromList $ reverse blocks)
  where
    onePeriod' = fromIntegral onePeriod
    genBlocks :: Int32 -> [Block] -> Gen [Block]
    genBlocks _ [] = error "impossible"
    genBlocks !lev blocks@(lst : _) = do
      let metadata = BlockMetadata
            { bmLevel = Level lev
            , bmCycle = Cycle $ (lev - 1) `div` onePeriod'
            , bmCyclePosition = fromIntegral $ (lev - 1) `mod` onePeriod'
            , bmVotingPeriod  = fromIntegral $ (lev - 1) `div` onePeriod'
            , bmVotingPeriodPosition = fromIntegral $ (lev - 1) `mod` onePeriod'
            , bmVotingPeriodType = Proposing
            }
      hash <- arbitrary
      let oneMinute = 60 :: NominalDiffTime
      let prevTime = bhrTimestamp (bHeader lst)
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

getBlock :: BlockChain -> BlockId -> Block
getBlock BlockChain{..} = \case
  HeadRef      -> V.last bcBlocksList
  GenesisRef   -> V.head bcBlocksList
  LevelRef lev -> bcBlocksList V.! (fromIntegral lev)
  BlockHashRef h -> M.findWithDefault (error "block not found") h bcBlocks
