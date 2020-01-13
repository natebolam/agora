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
import GHC.Natural (naturalToInt)
import Loot.Log.Internal.Logging (MonadLogging)
import Lorentz hiding (map)
import Tezos.Crypto (formatKeyHash, KeyHash (..))

import qualified Agora.DB as DB
import Agora.DB.Connection (runSelectReturningList')
import qualified Agora.Types as AT (UrlHash, ProposalHash, PublicKeyHash, Stage(..), encodeHash, stageToEpoch)
import Michelson.Text (unMText)
import Lorentz.Contracts.STKR.Governance.TypeDefs (Blake2BHash (..))
import Lorentz.Contracts.STKR.Client (AlmostStorage (..))

data StorageProposal = StorageProposal
  { hash :: AT.ProposalHash
  , description :: Text
  , newPolicy :: Map Text (AT.UrlHash, Text)
  } deriving Show

data StageStorage = StageStorage
  { ssStage     :: AT.Stage
  , ssCouncil   :: Set AT.PublicKeyHash
  , ssProposals :: [StorageProposal]
  , ssVotes     :: Map AT.PublicKeyHash Int
  } deriving Show

-- | Convert contract storage type to agora related storage type
convertStorage :: AlmostStorage -> StageStorage
convertStorage AlmostStorage {..} = StageStorage {..}
  where
    ssStage = AT.Stage $ fromIntegral $ naturalToInt stageCounter
    ssCouncil = S.map (AT.encodeHash . formatKeyHash) councilKeys
    ssProposals = flip map proposals $
      \(arg #proposal -> (arg #description -> dsc, arg #newPolicy -> arg #urls -> proposalPolicy),
        arg #proposalHash -> Blake2BHash byteHash) ->
        let hash = AT.encodeHash $ formatKeyHash $ KeyHash byteHash in
        let description = unMText dsc in
        let newPolicy = M.mapKeys unMText $
              M.map (\(urlHash, url) -> (AT.encodeHash $ formatKeyHash $ KeyHash urlHash, unMText url)) proposalPolicy in
        StorageProposal {..}
    ssVotes = M.mapKeys (\hash -> AT.encodeHash $ formatKeyHash hash) $
      M.map (\(arg #proposalId -> propId) -> naturalToInt propId) votes

-- | Return current or stage related storage
-- If there is no such stage, return Nothing
getStorage :: (MonadUnliftIO m, DB.MonadPostgresConn m, MonadLogging m) => Maybe AT.Stage -> m (Maybe StageStorage)
getStorage stage = do
  let DB.AgoraSchema {..} = DB.agoraSchema
  (ssStage, council) <- getCurrentStageWithCouncil stage
  let currentEpoch = AT.stageToEpoch ssStage
      councilFilter = filter_ (\c -> DB.cStage c ==. val_ ssStage) $ all_ asCouncil
      proposalFilter = orderBy_ (desc_ . DB.spId) $ filter_ (\p -> DB.spEpoch p ==. val_ currentEpoch) $ all_ asStkrProposals
  proposals <- runSelectReturningList' $ select proposalFilter
  votes <- runSelectReturningList' $ select $ do
    currentStageProposals <- proposalFilter
    currentStageCouncil <- councilFilter
    votes <- all_ asVotes
    guard_ (DB.StkrProposalId (DB.vProposalNumber votes) (DB.vEpoch votes) `references_` currentStageProposals)
    guard_ (DB.CouncilId (DB.vVoterPbkHash votes) (DB.vStage votes) `references_` currentStageCouncil)
    voteProposal <- related_ asStkrProposals (DB.StkrProposalId (DB.vProposalNumber votes) (DB.vEpoch votes))
    voteCouncil <- related_ asCouncil (DB.CouncilId (DB.vVoterPbkHash votes) (DB.vStage votes))
    pure (voteCouncil, voteProposal)
  let ssCouncil = S.fromList $ map DB.cPbkHash council
      ssProposals = flip map proposals $ \proposal -> StorageProposal
        { hash = DB.spHash proposal
        , description = mempty
        , newPolicy = mempty
        }
      ssVotes = M.fromList $ map (\(c, p) -> (DB.cPbkHash c, DB.spId p)) votes
  pure $ Just $ StageStorage {..}


-- | Return current stage or last stage if present and nothing if
-- there is no such stage or db is empty
getCurrentStageWithCouncil :: (MonadUnliftIO m, DB.MonadPostgresConn m)=> Maybe AT.Stage -> m (AT.Stage, [DB.CouncilT Identity])
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
    (c : _) -> (DB.cStage c, currentStage)
    _ -> (fromMaybe (AT.Stage 0) stage, [])
