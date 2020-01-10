{- Module which is responsible for application block to db. -}

{-# LANGUAGE TypeOperators #-}

module Agora.BlockStack
       ( MonadBlockStack (..)
       , BlockStack (..)
       , withBlockStack
       , blockStackCapOverDb
       , blockStackCapOverDbImplM
       , BlockStackCapImpl

       , insertBlockMeta
       ) where

import Control.Monad.Reader (withReaderT)
import qualified Data.List as L (length)
import qualified Data.Map as M
import qualified Data.Set as S
import Database.Beam.Query (default_, insert, insertExpressions, insertValues, val_, (==.), delete, (&&.), in_)
import qualified Database.Beam.Query as B
import Distribution.Utils.MapAccum (mapAccumM)
import Fmt (build, listF, (+|), (|+))
import Loot.Log (Logging, MonadLogging, logDebug, logInfo)
import Monad.Capabilities (CapImpl (..), CapsT, HasCap, HasNoCap, addCap, makeCap)
import UnliftIO (MonadUnliftIO)
import qualified UnliftIO as UIO

import Agora.Config
import Agora.DB
import Agora.Discourse
import Agora.Node.Client
import Agora.Node.Constants
import Agora.Node.Types
import Agora.STKR.Storage
import Agora.Types
import Agora.Util
import Loot.Log.Internal.Logging (logWarning)
import Michelson.Interpret.Unpack (UnpackError (..))
import Data.Time.Clock (UTCTime)

data BlockStack m = BlockStack
  { _getAdoptedHead :: m BlockHead
  , _applyBlock     :: Block -> m ()
  }

makeCap ''BlockStack

withBlockStack
  :: forall m caps a .
  ( HasCap PostgresConn caps
  , HasCap TezosClient caps
  , HasCap Logging caps
  , HasTzConstants caps
  , HasCap DiscourseClient caps
  , HasAgoraConfig caps
  , HasNoCap BlockStack caps
  , MonadUnliftIO m
  )
  => CapsT (BlockStack ': caps) m a
  -> CapsT caps m a
withBlockStack action = do
  blockStack <- lift blockStackCapOverDbImplM
  withReaderT (addCap blockStack) action

type BlockStackCapImpl m
  = CapImpl BlockStack '[DiscourseClient, PostgresConn, TezosClient, Logging, TzConstantsCap, AgoraConfigCap] m
type BlockStackMode m =
  ( MonadUnliftIO m
  , MonadPostgresConn m
  , MonadTezosClient m
  , MonadLogging m
  , MonadTzConstants m
  , MonadDiscourseClient m
  , MonadAgoraConfig m
  )

blockStackCapOverDbImplM :: MonadUnliftIO m => m (BlockStackCapImpl m)
blockStackCapOverDbImplM = do
  cache <- UIO.newTVarIO (Nothing :: Maybe BlockHead)
  pure $ CapImpl $ blockStackCapOverDb cache

blockStackCapOverDb :: BlockStackMode m => TVar (Maybe BlockHead) -> BlockStack m
blockStackCapOverDb cache = BlockStack
  { _getAdoptedHead = readAdoptedHead cache
  , _applyBlock = onBlock cache
  }

-- | Errors which can be raised during block application
data ApplyBlockError
  = VoterNotExist PublicKeyHash
  | ProposalNotExist ProposalHash
  deriving (Generic, Show, Eq)

instance Exception ApplyBlockError

readAdoptedHead ::
  ( MonadPostgresConn m
  , MonadUnliftIO m
  )
  => TVar (Maybe BlockHead) -> m BlockHead
readAdoptedHead cache = do
  mb <- UIO.readTVarIO cache
  case mb of
    Just x -> pure x
    Nothing -> do
      bhMb <- runSelectReturningOne' $ B.select $ do
        mx <- B.aggregate_ (B.max_ . blLevel) (B.all_ $ asBlockMetas agoraSchema)
        x <- B.all_ (asBlockMetas agoraSchema)
        B.guard_ (mx ==. B.just_ (blLevel x))
        pure (blHash x, blLevel x, blPredecessor x)
      let ret = case bhMb of
                  Nothing        -> genesisBlockHead
                  Just (h, l, p) -> BlockHead h l p
      UIO.atomically $ UIO.writeTVar cache (Just ret)
      pure ret

-- | Analyse the passed block and update corresponding tables.
-- If the passed block is the first in the period then:
-- * Current quorum is updated using /votes/current_quorum
-- * Voters' rolls are updated using /votes/listings
-- For any block:
-- * If block period is Proposal then new proposal are added
-- * New votes are added either to Ballots or ProposalVotes, depending on period type
-- * Info about current period is updated in PeriodMeta
onBlock
  :: forall m . BlockStackMode m
  => TVar (Maybe BlockHead)
  -> Block
  -> m ()
onBlock cache b@Block{..} = do
  let BlockHeader{..} = bHeader
      BlockMetadata{..} = bMetadata
  adopted <- readAdoptedHead cache
  if bmLevel > bhLevel adopted then do
    discourseStubs <- transact $ do
      insertBlockMeta b
      insertStorage b

    UIO.atomically $ UIO.writeTVar cache (Just $ block2Head b)
    logInfo $ "Block " +| block2Head b |+ " is applied to the database"
    mapM_ postProposalStubAsync discourseStubs
  else
    logInfo $
      "Block's " +| block2Head b |+ " is equal to or behind last adopted one " +| adopted |+ ".\
      \The block was ignored."

insertStorage :: forall m . BlockStackMode m => Block -> m [(Text, ProposalHash)]
insertStorage Block{..} = do
  let BlockHeader{..} = bHeader
      BlockMetadata{..} = bMetadata
      AgoraSchema{..} = agoraSchema
  contractAddress <- fromAgoraConfig $ sub #contract . option #address
  currentStorage <- getStorage Nothing
  storageExpression <- getContractStorage MainChain (LevelRef bmLevel) contractAddress
  case exprToValue @(Storage) storageExpression of
      Right storage -> do
        let blockStorage = convertStorage storage
        case currentStorage of
          Just currentStorage' -> do
           let differentEpochs = (stageToEpoch $ ssStage currentStorage') /= (stageToEpoch $ ssStage blockStorage)
           insertCouncil (ssStage currentStorage') blockStorage $ ssCouncil currentStorage'
           discourseStubs <- insertStkrProposal bhrTimestamp blockStorage $
             if differentEpochs then [] else map hash $ ssProposals currentStorage'
           insertVotes bhrTimestamp blockStorage $
             if differentEpochs then mempty else ssVotes currentStorage'
           pure discourseStubs
          Nothing -> do
            insertCouncil (Stage 0) blockStorage S.empty
            discourseStubs <- insertStkrProposal bhrTimestamp blockStorage []
            insertVotes bhrTimestamp blockStorage M.empty
            pure discourseStubs
      Left e -> (logWarning $ "Contract fetching error: " +| (build $ unUnpackError e)) >> pure []

insertCouncil :: forall m. BlockStackMode m => Stage -> StageStorage -> Set PublicKeyHash -> m ()
insertCouncil curStage storage existingCouncil = do
  let AgoraSchema {..} = agoraSchema
      newCouncil = ssCouncil storage S.\\ existingCouncil
      outdatedCouncil = existingCouncil S.\\ ssCouncil storage
  if (curStage < ssStage storage) then do
    let fff = [curStage + 1 .. ssStage storage]
    (runInsert' $ insert asCouncil $ insertExpressions $
      flip concatMap fff $
        \st -> flip map (S.toList $ ssCouncil storage) $
        \hash -> Council {cPbkHash = val_ hash, cStage = val_ $ st})
  else do
    runInsert' $ insert asCouncil $ insertExpressions $
      flip map (S.toList $ newCouncil) $ \hash -> Council {cPbkHash = val_ hash, cStage = val_ $ ssStage storage}
    runDelete' $ delete asCouncil $
      \c -> cStage c ==. val_ (ssStage storage) &&. in_ (cPbkHash c) (map val_ (S.toList outdatedCouncil))

insertStkrProposal :: forall m. BlockStackMode m => UTCTime -> StageStorage -> [ProposalHash] -> m [(Text, ProposalHash)]
insertStkrProposal time storage existingProposals = do
  let AgoraSchema {..} = agoraSchema
      existedHashes = S.fromList existingProposals
      newProposals = reverse $ filter (\p -> S.notMember (hash p) existedHashes) $ ssProposals storage
      newHashesWithDescription = map (\p -> (description p, hash p)) newProposals
      proposalNumber = L.length existingProposals
  (_, topicInfo) <- mapAccumM (\number (_, ph) -> do
      let shorten = shortenHash ph
      mTopic <- getProposalTopic shorten
      case mTopic of
        Just topic -> do
          hparts <- case parseHtmlParts (pCooked $ tPosts topic) of
            Left e -> do
              logDebug $ "Coudln't parse Discourse topic, reason: " +| e |+ ""
              pure $ HtmlParts Nothing Nothing
            Right hp -> pure $ toHtmlPartsMaybe shorten hp
          pure (number + 1, (number, Just topic, hparts))
        Nothing ->
          pure (number + 1, (number, Nothing, HtmlParts Nothing Nothing))) proposalNumber newHashesWithDescription
  let newHashesWithTopics = zip topicInfo newHashesWithDescription
      newProposalsWithTopics = concatMap (\((n, _, _), what) -> map (\mp -> (n, mp)) $ M.toList $ newPolicy what) $
        zip topicInfo newProposals
  runInsert' $ insert asStkrProposals $ insertExpressions $
    flip map newHashesWithTopics $ \((number, t, hp), (desc, what)) ->
      StkrProposal
      { spId                 = val_ number
      , spStage              = val_ $ ssStage storage
      , spEpoch             = val_ $ stageToEpoch $ ssStage storage
      , spHash               = val_ what
      , spTimeProposed       = val_ time
      , spDescription        = val_ desc
      , spDiscourseTitle     = val_ $ unTitle . tTitle <$> t
      , spDiscourseShortDesc = val_ $ hpShort hp
      , spDiscourseLongDesc  = val_ $ hpLong hp
      , spDiscourseTopicId   = val_ $ pTopicId . tPosts <$> t
      , spDiscoursePostId    = val_ $ pId . tPosts <$> t
      }
  runInsert' $ insert asPolicy $ insertExpressions $
      flip map newProposalsWithTopics $ \(number, (desc, (hash, url))) ->
        Policy
        { pProposalId  = val_ number
        , pEpoch      = val_ $ stageToEpoch $ ssStage storage
        , pHash        = val_ $ hash
        , pDescription = val_ $ desc
        , pUrl         = val_ $ url
        }
  let discourseStubs = mapMaybe (\((_, t, _), ph) -> if isNothing t then Just ph else Nothing) newHashesWithTopics
  unless (null newHashesWithDescription) $
    logInfo $ "New proposals were added: " +| listF (map snd newHashesWithDescription) |+ ""
  pure discourseStubs

insertVotes :: forall m. BlockStackMode m => UTCTime -> StageStorage -> Map PublicKeyHash Int -> m ()
insertVotes time storage existingVotes =
  let AgoraSchema {..} = agoraSchema in
  let newVotes = M.toList $ M.difference (ssVotes storage) existingVotes in
  runInsert' $ insert asVotes $ insertExpressions $
    flip map newVotes $ \(hash, number) ->
      Vote
      { vId             = default_
      , vStage          = val_ $ ssStage storage
      , vEpoch         = val_ $ stageToEpoch $ ssStage storage
      , vVoterPbkHash   = val_ hash
      , vProposalNumber = val_ number
      , vVoteTime       = val_ time
      }

insertBlockMeta :: (MonadPostgresConn m, MonadIO m) => Block -> m ()
insertBlockMeta Block{..} = do
  runInsert' $
    insert (asBlockMetas agoraSchema) $
    insertValues $ one $ BlockMeta
      { blLevel = bmLevel bMetadata
      , blHash  = bHash
      , blPredecessor = bhrPredecessor bHeader
      , blBlockTime   = bhrTimestamp bHeader
      }
