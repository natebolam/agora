{-# LANGUAGE DeriveAnyClass #-}
module Agora.STKR.Storage where

import Control.Monad.IO.Unlift (MonadUnliftIO)
import qualified Data.Map as M (fromList, map, mapKeys)
import qualified Data.Set as S
import Database.Beam.Query
  ( (==.)
  , aggregate_
  , all_
  , filter_
  , guard_
  , just_
  , max_
  , references_
  , related_
  , select
  , val_, orderBy_, desc_
  )
import Fmt (Buildable(..), Builder, (+|), (|+), base64F, blockMapF, jsonListF, mapF')
import GHC.Natural (naturalToInt)
import Loot.Log.Internal.Logging (MonadLogging)
import Lorentz hiding (map)
import Tezos.Crypto (formatKeyHash, KeyHash (..))

import qualified Agora.DB as DB
import Agora.DB.Connection (runSelectReturningList')
import qualified Agora.Types as AT (ProposalHash, PublicKeyHash, Stage(..), encodeHash)

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
  { ssStage     :: AT.Stage
  , ssCouncil   :: Set AT.PublicKeyHash
  , ssProposals :: [AT.ProposalHash]
  , ssVotes     :: Map AT.PublicKeyHash Int
  } deriving Show

-- | Convert contract storage type to agora related storage type
convertStorage :: Storage -> StageStorage
convertStorage Storage {..} = StageStorage {..}
  where
    ssStage = AT.Stage $ fromIntegral $ naturalToInt stageCounter
    ssCouncil = S.map (AT.encodeHash . formatKeyHash) councilKeys
    ssProposals = flip map proposals $ \(_, arg #proposalHash -> Blake2BHash hash) -> AT.encodeHash $ formatKeyHash $ KeyHash hash
    ssVotes = M.mapKeys (\hash -> AT.encodeHash $ formatKeyHash hash) $
      M.map (\(arg #proposalId -> propId) -> naturalToInt propId) votes

-- | Return current or stage related storage
-- If there is no such stage, return Nothing
getStorage :: (MonadUnliftIO m, DB.MonadPostgresConn m, MonadLogging m) => Maybe AT.Stage -> m (Maybe StageStorage)
getStorage stage = do
  let DB.AgoraSchema {..} = DB.agoraSchema
  stageWithCouncil <- getCurrentStageWithCouncil stage
  case stageWithCouncil of
    Nothing -> pure Nothing
    Just (ssStage, council) -> do
      let councilFilter = filter_ (\c -> DB.cStage c ==. val_ ssStage) $ all_ asCouncil
          proposalFilter = orderBy_ (desc_ . DB.spId) $ filter_ (\p -> DB.spStage p ==. val_ ssStage) $ all_ asStkrProposals
      proposals <- runSelectReturningList' $ select proposalFilter
      votes <- runSelectReturningList' $ select $ do
        currentStageProposals <- proposalFilter
        currentStageCouncil <- councilFilter
        votes <- all_ asVotes
        guard_ (DB.StkrProposalId (DB.vProposalNumber votes) (DB.vStage votes) `references_` currentStageProposals)
        guard_ (DB.CouncilId (DB.vVoterPbkHash votes) (DB.vStage votes) `references_` currentStageCouncil)
        voteProposal <- related_ asStkrProposals (DB.StkrProposalId (DB.vProposalNumber votes) (DB.vStage votes))
        voteCouncil <- related_ asCouncil (DB.CouncilId (DB.vVoterPbkHash votes) (DB.vStage votes))
        pure (voteCouncil, voteProposal)
      let ssCouncil = S.fromList $ map DB.cPbkHash council
          ssProposals = map DB.spHash proposals
          ssVotes = M.fromList $ map (\(c, p) -> (DB.cPbkHash c, DB.spId p)) votes
      pure $ Just $ StageStorage {..}


-- | Return current stage or last stage if present and nothing if
-- there is no such stage or db is empty
getCurrentStageWithCouncil :: (MonadUnliftIO m, DB.MonadPostgresConn m)=> Maybe AT.Stage -> m (Maybe (AT.Stage, [DB.CouncilT Identity]))
getCurrentStageWithCouncil stage = do
  let DB.AgoraSchema {..} = DB.agoraSchema
  currentStage <- case stage of
    Nothing -> runSelectReturningList' $ select $ do
      council <- all_ asCouncil
      currentStage <- aggregate_ (max_ . DB.cStage) $ all_ asCouncil
      guard_ (just_ (DB.cStage council) ==. currentStage)
      pure council
    Just st -> runSelectReturningList' $ select $ filter_ (\council -> DB.cStage council ==. val_ st) $ all_ asCouncil
  pure $ case currentStage of
    (c : _) -> Just (DB.cStage c, currentStage)
    _ -> Nothing
