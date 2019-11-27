{-# LANGUAGE DataKinds #-}

{-|
Servant type-level specification for Client API to retrieve data from Tezos Node.
-}
module Agora.Node.API
       ( NodeEndpoints (..)
       , MytezosbakerEndpoints (..)
       ) where

import Servant.API ((:>), Capture, Get, JSON)
import Servant.API.Generic ((:-))
import Servant.API.Stream (NewlineFraming, SourceIO, StreamGet)

import Agora.Node.Types
import Agora.Types

-- | Definition of a subset of Tezos node RPC API.
data NodeEndpoints route = NodeEndpoints
  { neGetBlock :: route
      :- "chains"
      :> Capture "chain_id" ChainId
      :> "blocks"
      :> Capture "block_id" BlockId
      :> Get '[JSON] Block

  , neGetBlockMetadata :: route
      :- "chains"
      :> Capture "chain_id" ChainId
      :> "blocks"
      :> Capture "block_id" BlockId
      :> "metadata"
      :> Get '[JSON] BlockMetadata

  , neGetBlockHead :: route
      :- "chains"
      :> Capture "chain_id" ChainId
      :> "blocks"
      :> Capture "block_id" BlockId
      :> "header"
      :> Get '[JSON] BlockHead

  , neGetVoters :: route
      :- "chains"
      :> Capture "chain_id" ChainId
      :> "blocks"
      :> Capture "block_id" BlockId
      :> "votes"
      :> "listings"
      :> Get '[JSON] [Voter]

  , neGetQuorum :: route
      :- "chains"
      :> Capture "chain_id" ChainId
      :> "blocks"
      :> Capture "block_id" BlockId
      :> "votes"
      :> "current_quorum"
      :> Get '[JSON] Quorum

  , neGetCheckpoint :: route
      :- "chains"
      :> Capture "chain_id" ChainId
      :> "checkpoint"
      :> Get '[JSON] Checkpoint

  , neNewHeadStream :: route
      :- "monitor"
      :> "heads"
      :> Capture "chain_id" ChainId
      :> StreamGet NewlineFraming JSON (SourceIO BlockHead)
  } deriving Generic

-- | Definition of a portion of Mytezosbaker API
data MytezosbakerEndpoints route = MytezosbakerEndpoints
  { mtzbBakers :: route
      :- "bakers"
      :> Get '[JSON] BakerInfoList
  } deriving Generic
