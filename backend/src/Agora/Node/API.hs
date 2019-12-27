{-# LANGUAGE DataKinds #-}

{-|
Servant type-level specification for Client API to retrieve data from Tezos Node.
-}
module Agora.Node.API
       ( NodeEndpoints (..)
       ) where

import Servant.API ((:>), Capture, Get, JSON)
import Servant.API.Generic ((:-))
import Servant.API.Stream (NewlineFraming, SourceIO, StreamGet)
import Tezos.V005.Micheline (Expression)

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

  , neNewHeadStream :: route
      :- "monitor"
      :> "heads"
      :> Capture "chain_id" ChainId
      :> StreamGet NewlineFraming JSON (SourceIO BlockHead)

  , neGetContractStorage :: route
      :- "chains"
      :> Capture "chain_id" ChainId
      :> "blocks"
      :> Capture "block_id" BlockId
      :> "context"
      :> "contracts"
      :> Capture "contract" ContractHash
      :> "storage"
      :> Get '[JSON] Expression

  } deriving Generic
