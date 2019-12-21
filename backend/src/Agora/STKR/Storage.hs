{-# LANGUAGE DeriveAnyClass #-}
module Agora.STKR.Storage where

import Fmt (Buildable(..), Builder, blockMapF, jsonListF, mapF', base64F, (+|), (|+))

import Lorentz
import Tezos.Crypto (formatKeyHash)
import qualified Agora.DB as ADB (Council, StkrProposal, Vote, Policy, MonadPostgresConn)
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Loot.Log.Internal.Logging (MonadLogging)
import Agora.Types (Stage)
import qualified Agora.DB.Schema as DB
import Agora.DB.Connection (runSelectReturningList')
import Database.Beam.Query (select, all_, max_, filter_, (<=.), aggregate_, guard_, just_, (==.), val_)

type Hash = ByteString
type URL = MText

newtype Blake2BHash = Blake2BHash ByteString
  deriving newtype (IsoValue, Show, Eq)

blake2B_ :: ByteString & s :-> Blake2BHash & s
blake2B_ = blake2B # coerce_

type Policy =
  ( "urls" :! Map MText (Hash, URL)
  )

type Proposal =
  ( "description" :! MText
  , "newPolicy" :! Policy
  )

type ProposalAndHash = ("proposal" :! Proposal, "proposalHash" :! Blake2BHash)

data Storage = Storage
  { owner :: Address
  , councilKeys :: Set KeyHash
  , policy :: Policy
  , proposals :: [ProposalAndHash]
  , votes :: Map KeyHash ("proposalId" :! Natural)
  , stageCounter :: Natural
  -- ^ @stageCounter `div` 4@ is current epoch and @stageCounter `mod` 4@
  -- denotes current stage within an epoch
  , totalSupply :: Natural
  , ledger :: Natural
  }
  deriving stock Generic
  deriving anyclass IsoValue
  deriving Show

instance Buildable (Map MText (Hash, URL)) where
  build urls =
    mapF' build (
        \(hash, value) ->
          "(" +| base64F hash |+ "," +| build value |+ ""
        ) urls

instance Buildable Proposal where
  build (desc, newPolicy) = build desc |+ ": " +| build newPolicy

instance Buildable ProposalAndHash where
  build (proposal, _) = build proposal

instance Buildable Storage where
  build Storage{..} = blockMapF @[(Text, Builder)] $
    [ ("owner", build owner)
    , ("councilKeys", jsonListF councilKeys)
    , ("policy", build policy)
    , ("proposals", jsonListF proposals)
    , ("votes", mapF' (build . formatKeyHash) build votes)
    , ("stageCounter", build stageCounter)
    , ("totalSupply", build totalSupply)
    , ("ledger", "BigMap values should not be displayed")
    ]

data StageStorage = StageStorage
  { ssCouncil :: [ADB.Council]
  , ssProposals :: [ADB.StkrProposal]
  , ssPolicy :: [ADB.Policy]
  , ssVotes :: [ADB.Vote]
  }

getCurrentStorage :: (MonadUnliftIO m, ADB.MonadPostgresConn m, MonadLogging m) => Stage -> m StageStorage
getCurrentStorage stage = do
  let DB.AgoraSchema {..} = DB.agoraSchema
  ssCouncil <-
    runSelectReturningList' $
    select $ do
      council <- all_ asCouncil
      lstr <- aggregate_ (max_ . DB.cStage) $ filter_ (\cs -> DB.cStage cs <=. val_ stage) $ all_ asCouncil
      guard_ (just_ (DB.cStage council) ==. lstr)
      pure council
  let proposalsFilter = filter_ (\prop -> DB.spStage prop ==. val_ stage) $ all_ asStkrProposals
  ssProposals <- runSelectReturningList' $ select $ proposalsFilter
  ssVotes <- runSelectReturningList' $ select $ do
    proposals <- proposalsFilter
    vtrs <- all_ asVotes
    guard_ (DB.vProposal vtrs ==.  (DB.StkrProposalId . DB.spId) proposals)
    pure vtrs
  ssPolicy <- runSelectReturningList' $ select $ all_ asPolicy
  pure $ StageStorage {..}
