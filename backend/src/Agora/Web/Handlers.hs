{-|
API handlers implementation.
-}
module Agora.Web.Handlers
       ( agoraHandlers

       -- * For tests
       , getStageInfo
       , getProposals
       , getProposalVotes
       , getSpecificProposalVotes
       ) where

import Database.Beam.Query (aggregate_, all_, as_, asc_, countAll_, count_, desc_, filter_, group_,
                            guard_, just_, leftJoin_, max_, maybe_, nothing_, nub_, orderBy_,
                            references_, select, val_, (&&.), (==.))
import Fmt (build, fmt, (+|), (|+))
import Servant.API.Generic (ToServant)
import Servant.Server.Generic (AsServerT, genericServerT)
import UnliftIO (throwIO)

import Agora.Config
import Agora.DB
import qualified Agora.DB as DB
import Agora.Mode
import Agora.Types
import Agora.Web.API
import Agora.Web.Error
import Agora.Web.Types
import qualified Agora.Web.Types as T
import Data.Maybe (fromJust)

type AgoraHandlers m = ToServant AgoraEndpoints (AsServerT m)

-- | Server handler implementation for Agora API.
agoraHandlers :: forall m . AgoraWorkMode m => AgoraHandlers m
agoraHandlers = genericServerT AgoraEndpoints
  { aeStage                 = getStageInfo
  , aeProposals             = getProposals
  , aeProposal              = getProposal
  , aeSpecificProposalVotes = getSpecificProposalVotes
  , aeProposalVotes         = getProposalVotes
  }

-- | Fetch info about specific stage.
-- If Nothing passed the last known period will be used.
getStageInfo :: AgoraWorkMode m => Maybe Stage -> m StageInfo
getStageInfo periodIdMb = do
  let AgoraSchema {..} = agoraSchema
  lastStage <- getLastStage
  let _iStage@(Stage a) = fromMaybe lastStage periodIdMb
      stageType = toEnum $ fromIntegral $ a `mod` 4

  stages <- runSelectReturningList' $ select $ nub_ $ fmap cStage $ orderBy_ (asc_ . cStage) $ all_ asCouncil
  let _iStageTimes = flip map stages $ \s@(Stage stage) ->
        StageItemInfo
        { _piiStage     = s
        , _piiStageType = toEnum $ fromIntegral $ stage `mod` 4
        }
      _iTotalStages = fromIntegral $ length stages
      _iStageType = toEnum $ fromIntegral $ _iStage `mod` 4
  councilSize <- runSelectReturningOne' $ select $ aggregate_ (const countAll_) $
    filter_ (\c -> cStage c ==. val_ _iStage) $ all_ asCouncil
  councilVoted <- runSelectReturningOne' $ select $ aggregate_ (const countAll_) $
    filter_ (\v -> vStage v ==. val_ _iStage) $ all_ asVotes
  let voteStats = VoteStats (Voters $ fromIntegral $ fromMaybe 0 councilVoted )
        (Voters $ fromIntegral $ fromMaybe 0 councilSize)

      notInLastPeriod :: a -> Maybe a
      notInLastPeriod val = if _iStage == lastStage then Nothing else Just val
  _iDiscourseLink <- askDiscourseHost
  case stageType of
    Proposing -> do
      _piWinner <- join . notInLastPeriod . rightToMaybe <$> getWinner (_iStage + 2)
      pure $ ProposalInfo {..}
    Evaluation -> do
      _piWinner <- join . notInLastPeriod . rightToMaybe <$> getWinner (_iStage + 1)
      pure $ EvaluationInfo{..}
    Voting -> do
      let _piVoteStats = voteStats
      _piWinner <- join . notInLastPeriod . rightToMaybe <$> getWinner _iStage
      pure $ VotingInfo{..}
    Implementation -> do
      _tiProposal <- getWinnerOrThrow (_iStage - 1)
      pure $ ImplementationInfo{..}

-- | Return the winner of proposal stage.
-- Implying that this function is calling
-- only when a winner exists.
getWinner :: AgoraWorkMode m => Stage -> m (Either Text T.Proposal)
getWinner stage = do
  proposals <- getProposals stage
  case sortOn (Down . _prVotesCasted) proposals of
    [] -> pure $ Left $ "No proposals in period "+|stage|+" are found."
    [prop] -> pure $ Right prop
    (prop1 : prop2 : _) ->
      if _prVotesCasted prop1 > _prVotesCasted prop2
      then pure $ Right prop1
      else pure $ Left $ "No clear winner in proposal period " +| stage |+ "."

-- | Version of `getWinner` which throws when winner is not found.
getWinnerOrThrow :: AgoraWorkMode m => Stage -> m T.Proposal
getWinnerOrThrow stage = getWinner stage >>= either (throwIO . InternalError) pure

-- | Fetch last known stage.
-- It's possible that the database is empty and
-- last stage is unknown, in this case @StageMetasNotFilledYet@ will be thrown.
getLastStage :: AgoraWorkMode m => m Stage
getLastStage = do
  perMb <- runSelectReturningOne' $ select $
    aggregate_ (max_ . cStage) (all_ $ asCouncil agoraSchema)
  case perMb of
    Just (Just pid) -> pure pid
    _               -> throwIO StageMetasNotFilledYet

-- | Get all proposals for passed period.
getProposals
  :: AgoraWorkMode m
  => Stage
  -> m [T.Proposal]
getProposals stage = do
  let AgoraSchema {..} = agoraSchema
  host <- askDiscourseHost
  results <- runSelectReturningList' $ select $
    orderBy_ (\(_, v) -> desc_ v) $
    aggregate_ (\(p, v) -> (group_ p,  as_ @Int $ count_ (as_ @(Maybe Int) (maybe_ (nothing_) (\_ -> just_ 1) v)))) $ do
      prop <- all_ asStkrProposals
      guard_ (DB.spEpoche prop ==. val_ (stageToEpoche stage))
      votes <- leftJoin_ (all_ asVotes) $ (\v -> DB.StkrProposalId (DB.vProposalNumber v) (DB.vEpoche v) `references_` prop)
      pure (prop, votes)
  pure $ map (convertProposal host) results

-- | Get info about proposal by proposal id.
getProposal
  :: AgoraWorkMode m
  => Int
  -> Stage
  -> m T.Proposal
getProposal propId stage = do
  let AgoraSchema {..} = agoraSchema
  host <- askDiscourseHost
  resultMb <- runSelectReturningOne' $ select $
    orderBy_ (\(_, v) -> desc_ v) $
    aggregate_ (\(p, v) -> (group_ p,  as_ @Int $ count_ (as_ @(Maybe Int) (maybe_ (nothing_) (\_ -> just_ 1) v)))) $ do
      prop <- all_ asStkrProposals
      guard_ (DB.spEpoche prop ==. val_ (stageToEpoche stage) &&. DB.spId prop ==. val_ propId)
      votes <- leftJoin_ (all_ asVotes) $ (\v -> DB.StkrProposalId (DB.vProposalNumber v) (DB.vEpoche v) `references_` prop)
      pure (prop, votes)
  result <- resultMb `whenNothing` throwIO (NotFound "On given stage proposal with given id not exist")
  pure $ convertProposal host result

getProposalVotes :: AgoraWorkMode m => Stage -> m [T.ProposalVote]
getProposalVotes stage = do
  let AgoraSchema {..} = agoraSchema
  resultMb <- runSelectReturningList' $ select $ orderBy_ (\(_, v) -> asc_ (vVoteTime v)) $ do
    prop <- all_ asStkrProposals
    guard_ (DB.spEpoche prop ==. val_ (stageToEpoche stage))
    votes <- leftJoin_ (all_ asVotes) $ (\v -> DB.StkrProposalId (DB.vProposalNumber v) (DB.vEpoche v) `references_` prop)
    pure (prop, votes)
  case resultMb of
    [] -> pure []
    vs -> pure $ map (uncurry contertVote) $ map (\(p, v) -> (p, fromJust v)) $ filter (isJust . snd) vs

getSpecificProposalVotes :: AgoraWorkMode m => Stage -> Int -> m [T.ProposalVote]
getSpecificProposalVotes stage propId = do
  let AgoraSchema {..} = agoraSchema
  resultMb <- runSelectReturningList' $ select $ orderBy_ (\(_, v) -> asc_ (vVoteTime v)) $ do
    prop <- all_ asStkrProposals
    guard_ (DB.spEpoche prop ==. val_ (stageToEpoche stage) &&. DB.spId prop ==. val_ propId)
    votes <- leftJoin_ (all_ asVotes) $ (\v -> DB.StkrProposalId (DB.vProposalNumber v) (DB.vEpoche v) `references_` prop)
    pure (prop, votes)
  case resultMb of
    [] -> pure []
    vs@((prop, _) : _ ) -> pure $ map (contertVote prop) (map fromJust $ filter isJust $ map snd vs)

askDiscourseHost :: MonadAgoraConfig m => m Text
askDiscourseHost = fmt . build <$> fromAgoraConfig (sub #discourse . option #host)

---------------------------------------------------------------------------
-- Converters from db datatypes to corresponding web ones
---------------------------------------------------------------------------

contertVote :: DB.StkrProposal -> DB.Vote -> T.ProposalVote
contertVote DB.StkrProposal{..} DB.Vote{..} =
  T.ProposalVote
  { _pvId = Id $ fromIntegral vId
  , _pvProposal = spHash
  , _pvProposalTitle = spDiscourseTitle
  , _pvAuthor = vVoterPbkHash
  , _pvTimestamp = vVoteTime
  }

convertProposal :: Text -> (DB.StkrProposal, Int) -> T.Proposal
convertProposal discourseHost (DB.StkrProposal{..}, castedNumber) =
  T.Proposal
  { _prId = fromIntegral spId
  , _prStage = spStage
  , _prHash = spHash
  , _prTitle = spDiscourseTitle
  , _prShortDescription = spDiscourseShortDesc
  , _prLongDescription = spDiscourseLongDesc
  , _prTimeCreated = spTimeProposed
  , _prProposalFile = spDiscourseFile
  , _prDiscourseLink = liftA2 sl (Just $ discourseHost `sl` "t") (fmt . build <$> spDiscourseTopicId)
  , _prVotesCasted = fromIntegral castedNumber
  }
  where
    sl a b = a <> "/" <> b
