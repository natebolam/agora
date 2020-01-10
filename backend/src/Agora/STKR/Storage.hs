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
import qualified Agora.Types as AT (UrlHash, ProposalHash, PublicKeyHash, Stage(..), encodeHash, stageToEpoche)
import Michelson.Text (unMText)

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

type GetBalanceParams = ("owner" :! Address)

type VoteForProposalParams =
  ( "proposalId" :! Natural
  , "votePk" :! PublicKey
  , "voteSig" :! Signature
  )

data PublicEntrypointParam
  = VoteForProposal VoteForProposalParams
  | GetBalance (View GetBalanceParams Natural)
  | GetTotalSupply (View () Natural)
  deriving stock Generic
  deriving anyclass IsoValue

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
  , frozen :: Bool
  , successor :: Maybe (Lambda PublicEntrypointParam Operation)
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
convertStorage :: Storage -> StageStorage
convertStorage Storage {..} = StageStorage {..}
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
  let currentEpoche = AT.stageToEpoche ssStage
      councilFilter = filter_ (\c -> DB.cStage c ==. val_ ssStage) $ all_ asCouncil
      proposalFilter = orderBy_ (desc_ . DB.spId) $ filter_ (\p -> DB.spEpoche p ==. val_ currentEpoche) $ all_ asStkrProposals
  proposals <- runSelectReturningList' $ select proposalFilter
  votes <- runSelectReturningList' $ select $ do
    currentStageProposals <- proposalFilter
    currentStageCouncil <- councilFilter
    votes <- all_ asVotes
    guard_ (DB.StkrProposalId (DB.vProposalNumber votes) (DB.vEpoche votes) `references_` currentStageProposals)
    guard_ (DB.CouncilId (DB.vVoterPbkHash votes) (DB.vStage votes) `references_` currentStageCouncil)
    voteProposal <- related_ asStkrProposals (DB.StkrProposalId (DB.vProposalNumber votes) (DB.vEpoche votes))
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
