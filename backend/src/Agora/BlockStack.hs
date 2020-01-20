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
import Data.Time.Calendar (fromGregorian)
import Data.Time.Clock (UTCTime (UTCTime))
import Database.Beam.Query (default_, insert, insertExpressions, insertValues, val_, (==.), delete, (&&.), in_, select)
import Database.Beam.Schema (primaryKey)
import Distribution.Utils.MapAccum (mapAccumM)
import Fmt (build, listF, (+|), (|+))
import Loot.Log (Logging, MonadLogging, logDebug, logInfo)
import Monad.Capabilities (CapImpl (..), CapsT, HasCap, HasNoCap, addCap, makeCap)
import UnliftIO (MonadUnliftIO)

import qualified Data.List as L (length)
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Database.Beam.Query as B
import qualified Database.Beam.Postgres.Full as Pg
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
import Lorentz.Contracts.STKR.Client (AlmostStorage)

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
    (if bmLevel `mod` 1000 == 0 then logInfo else logDebug)
      $ "Block " +| block2Head b |+ " is applied to the database"

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
  let contractStartTime = UTCTime (fromGregorian 2020 1 1) 0
  let blockStage = timeToStage contractStartTime bhrTimestamp

  contractAddress <- fromAgoraConfig $ sub #contract . option #address

  storageExpression <- getContractStorage MainChain (LevelRef bmLevel) contractAddress
  case exprToValue @(AlmostStorage) storageExpression of
    Right storage -> do
      let blockStorage = convertStorage storage

      when (ssStage blockStorage > blockStage) $
        logWarning $ "Stage in contract storage = "+|ssStage blockStorage|+
          " is in the future"
      -- HACK: allow the contract storage to override calculated stage.
      -- We use it for demonstration purposes to show stages from the future.
      let actStage = max blockStage (ssStage blockStorage)

      ourStorage <- getStorage actStage

      updateCouncil ourStorage (ssCouncil blockStorage)

      discourseStubs <-
        insertStkrProposal bhrTimestamp ourStorage (ssProposals blockStorage)

      insertVotes bhrTimestamp blockStorage $ ssVotes ourStorage

      pure discourseStubs
    Left e -> (logWarning $ "Contract fetching error: " +| (build $ unUnpackError e)) >> pure []

updateCouncil
  :: forall m. BlockStackMode m
  => StageStorage  -- ^ Our view of the storage
  -> Set PublicKeyHash  -- ^ Council from the block
  -> m ()
updateCouncil ourStorage blockCouncil = do
  let stage = ssStage ourStorage
  let AgoraSchema {..} = agoraSchema
      newCouncil = blockCouncil S.\\ ssCouncil ourStorage
      outdatedCouncil = ssCouncil ourStorage S.\\ blockCouncil

  -- HACK: should not be needed, but is here for the same reason as the hack above
  -- FIXME: Not idea why it doesn't work without join
  lastKnownStage <- fmap (fromMaybe 0 . join) $ runSelectReturningOne'
    $ select
    $ B.aggregate_ (B.max_ . cStage) (B.all_ asCouncil)
  runInsert'
    $ Pg.insert asCouncil
    ( insertExpressions
        [ Council {cPbkHash = val_ hash, cStage = val_ stage'}
        | hash <- S.toList (ssCouncil ourStorage)
        , stage' <- [lastKnownStage + 1 .. stage - 1]
        ]
    )
    $ Pg.onConflict (Pg.conflictingFields primaryKey) $ Pg.onConflictDoNothing

  runInsert'
    $ Pg.insert asCouncil
    ( insertExpressions
        [ Council {cPbkHash = val_ hash, cStage = val_ stage}
        | hash <- S.toList newCouncil
        ]
    )
    $ Pg.onConflict (Pg.conflictingFields primaryKey) $ Pg.onConflictDoNothing

  runDelete' $ delete asCouncil $ \c ->
    cStage c ==. val_ stage &&. in_ (cPbkHash c) (map val_ (S.toList outdatedCouncil))

insertStkrProposal
  :: forall m. BlockStackMode m
  => UTCTime  -- ^ Block timestamp
  -> StageStorage  -- ^ Our view of the storage
  -> [StorageProposal]  -- ^ Proposals from the block
  -> m [(Text, ProposalHash)]
insertStkrProposal time storage blockProposals = do
  let AgoraSchema {..} = agoraSchema
      existingHashes = S.fromList (hash <$> ssProposals storage)
      newProposals
        = reverse
        $ filter (\p -> S.notMember (hash p) existingHashes)
        $ blockProposals
      newHashesWithDescription = map (\p -> (description p, hash p)) newProposals
      proposalNumber = L.length (ssProposals storage)
  (_, topicInfo) <- mapAccumM (\number (_, ph) -> do
      let shorten = shortenHash ph
      mTopic <- getProposalTopic shorten
      case mTopic of
        Just topic -> do
          hparts <- case parseHtmlParts (pCooked $ tPosts topic) of
            Left e -> do
              logInfo $ "Coudln't parse Discourse topic, reason: " +| e |+ ""
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

insertVotes
  :: forall m. BlockStackMode m
  => UTCTime  -- ^ Block timestamp
  -> StageStorage  -- ^ Our view of the storage
  -> Map PublicKeyHash Int  -- ^ Votes from the block
  -> m ()
insertVotes time storage blockVotes =
  let AgoraSchema {..} = agoraSchema in
  runInsert'
    $ Pg.insert asVotes
    ( insertExpressions
      ( map (\(hash, number) -> Vote
          { vSeq            = default_
          , vStage          = val_ $ ssStage storage
          , vEpoch          = val_ $ stageToEpoch $ ssStage storage
          , vVoterPbkHash   = val_ hash
          , vProposalNumber = val_ number
          , vVoteTime       = val_ time
          })
      . M.toList
      $ blockVotes
      )
    )
    $ Pg.onConflict (Pg.conflictingFields primaryKey) $ Pg.onConflictDoNothing

insertBlockMeta :: (MonadPostgresConn m, MonadIO m) => Block -> m ()
insertBlockMeta Block{..} = do
  runDelete' $ delete (asBlockMetas agoraSchema) $ \_ -> val_ True
  runInsert' $
    insert (asBlockMetas agoraSchema) $
    insertValues $ one $ BlockMeta
      { blLevel = bmLevel bMetadata
      , blHash  = bHash
      , blPredecessor = bhrPredecessor bHeader
      , blBlockTime   = bhrTimestamp bHeader
      }
