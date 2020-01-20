{-# LANGUAGE DeriveAnyClass #-}
module Agora.STKR.Storage where

import Control.Monad.IO.Unlift (MonadUnliftIO)
import qualified Data.Map as M (fromList, map, mapKeys)
import qualified Data.Set as S
import Database.Beam.Query
  ( (==.)
  , all_
  , guard_
  , select
  , val_, orderBy_, desc_
  )
import GHC.Natural (naturalToInt)
import Loot.Log.Internal.Logging (MonadLogging)
import Lorentz hiding (map)
import Tezos.Crypto (formatKeyHash, KeyHash (..))

import Agora.DB.Connection (runSelectReturningList')
import Michelson.Text (unMText)
import Lorentz.Contracts.STKR.Governance.TypeDefs (Blake2BHash (..))
import Lorentz.Contracts.STKR.Client (AlmostStorage (..))

import qualified Agora.DB as DB
import qualified Agora.Types as AT (UrlHash, ProposalHash, PublicKeyHash, Stage(..), encodeHash, stageToEpoch)


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

-- | Return Agora storage for the given stage.
--
-- Note that it is possible that there is no information about the current stage
-- yet, in which case the function returns an "empty" storage.
getStorage :: (MonadUnliftIO m, DB.MonadPostgresConn m, MonadLogging m) => AT.Stage -> m StageStorage
getStorage ssStage = do
    let epoch = AT.stageToEpoch ssStage
    let DB.AgoraSchema{..} = DB.agoraSchema
    ssCouncil <- fmap S.fromList $ runSelectReturningList'
      $ select
      $ do
        member <- all_ asCouncil
        guard_ $ DB.cStage member ==. val_ ssStage
        pure $ DB.cPbkHash member
    ssProposals <- fmap (map toStorageProposal) $ runSelectReturningList'
      $ select
      $ orderBy_ (desc_ . DB.spId)
      $ do
        proposal <- all_ asStkrProposals
        guard_ $ DB.spEpoch proposal ==. val_ epoch
        pure proposal
    ssVotes <- fmap M.fromList $ runSelectReturningList'
      $ select
      $ do
        vote <- all_ asVotes
        guard_ $ DB.vEpoch vote ==. val_ epoch
        pure (DB.vVoterPbkHash vote, DB.vProposalNumber vote)
    pure StageStorage{..}
  where
    toStorageProposal proposal = StorageProposal
      { hash = DB.spHash proposal
      , description = mempty
      , newPolicy = mempty
      }
