{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module Agora.Node.Blockchain
      ( BlockChain (..)
      , genBlockChain
      , bcHead
      , genesisBlock
      , getBlock
      ) where

import qualified Data.Map as M
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
    levelsInCycle = 4096
    levelsInPeriod = 8 * levelsInCycle

    genBlocks :: Int32 -> [Block] -> Gen [Block]
    genBlocks _ [] = error "impossible"
    genBlocks !lev blocks@(lst : _) = do
      let metadata = BlockMetadata
            { bmLevel = Level lev
            , bmCycle = Cycle $ (lev - 1) `div` levelsInCycle
            , bmCyclePosition = (lev - 1) `mod` levelsInCycle
            , bmVotingPeriod  = fromIntegral $ (lev - 1) `div` levelsInPeriod
            , bmVotingPeriodPosition = (lev - 1) `mod` levelsInPeriod
            , bmVotingPeriodType = Proposing
            }
      hash <- arbitrary
      let block = Block hash (Operations []) metadata (bHash lst)
      if lev < n then
        genBlocks (lev + 1) (block : blocks)
      else
        pure (block : blocks)

genesisBlock :: Block
genesisBlock = Block
  { bHash = Hash $ encodeUtf8 ("BLockGenesisGenesisGenesisGenesisGenesisf79b5d1CoW2" :: Text)
  , bOperations = Operations []
  , bMetadata = error "Imitating absence of metadata field in the real genesis block"
  , bPredecessor = Hash $ encodeUtf8 ("BLockGenesisGenesisGenesisGenesisGenesisf79b5d1CoW2" :: Text)
  }

bcHead :: BlockChain -> Block
bcHead BlockChain {..} = V.last bcBlocksList

getBlock :: BlockChain -> BlockId -> Block
getBlock BlockChain{..} = \case
  HeadRef      -> V.last bcBlocksList
  GenesisRef   -> V.head bcBlocksList
  LevelRef lev -> bcBlocksList V.! (fromIntegral lev)
  BlockHashRef h -> M.findWithDefault (error "block not found") h bcBlocks
