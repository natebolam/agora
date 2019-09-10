{-|
API handlers implementation.
-}
module Agora.Web.Handlers
       ( agoraHandlers

       -- * For tests
       , getPeriodInfo
       , getProposals
       , getProposalVotes
       , getSpecificProposalVotes
       , getBallots
       , isVotePassed
       ) where

import Data.List ((!!))
import qualified Data.Set as S
import Data.Time.Clock (addUTCTime, getCurrentTime)
import Database.Beam.Postgres (Postgres)
import Database.Beam.Query (Projectible, Q, QBaseScope, QExpr, aggregate_, all_, asc_, countAll_,
                            desc_, except_, filter_, guard_, in_, limit_, max_, oneToMany_,
                            orderBy_, references_, related_, select, val_, (&&.), (<.), (==.))
import Database.Beam.Query.Internal (QNested)
import Fmt (build, fmt, (+|), (|+))
import Servant.API.Generic (ToServant)
import Servant.Server.Generic (AsServerT, genericServerT)
import qualified Universum.Unsafe as U
import UnliftIO (throwIO)

import Agora.Config
import Agora.DB
import qualified Agora.DB as DB
import Agora.Mode
import Agora.Node
import Agora.Types
import Agora.Util
import Agora.Web.API
import Agora.Web.Error
import Agora.Web.Types
import qualified Agora.Web.Types as T

type AgoraHandlers m = ToServant AgoraEndpoints (AsServerT m)

-- | Server handler implementation for Agora API.
agoraHandlers :: forall m . AgoraWorkMode m => AgoraHandlers m
agoraHandlers = genericServerT AgoraEndpoints
  { aePeriod                = getPeriodInfo
  , aeProposals             = getProposals
  , aeProposal              = getProposal
  , aeSpecificProposalVotes = getSpecificProposalVotes
  , aeProposalVotes         = getProposalVotes
  , aeBallots               = getBallots
  , aeNonVoters             = getNonVoters
  }

-- | Fetch info about specific period.
-- If Nothing passed the last known period will be used.
getPeriodInfo :: AgoraWorkMode m => Maybe PeriodId -> m PeriodInfo
getPeriodInfo periodIdMb = do
  lastPeriodId <- getLastPeriod
  let periodId = fromMaybe lastPeriodId periodIdMb

  pMb <- runSelectReturningOne' $ select $ do
    pm <- all_ (asPeriodMetas agoraSchema)
    guard_ (pmId pm ==. val_ periodId)
    pure pm
  oneCycle <- tzCycleLength <$> askTzConstants
  periodStarts <- runSelectReturningList' $ select $
    orderBy_ (asc_ . (^. _1)) $ do
      pm <- all_ (asPeriodMetas agoraSchema)
      pure (pmId pm, pmWhenStarted pm, pmType pm, pmEndLevel pm - pmLastBlockLevel pm + 1)

  PeriodMeta{..} <- pMb `whenNothing` throwIO noSuchPeriod
  let mkPItemInfos [] = pure []
      mkPItemInfos [(_, started, ptype, leftLevels)] = do
        curTime <- liftIO getCurrentTime
        pure $ one $ PeriodItemInfo started (addUTCTime (fromIntegral leftLevels * 60) curTime) ptype
      mkPItemInfos ((_, started, ptype, _) : xs@((_, startedNext, _, _) : _)) =
        (PeriodItemInfo started startedNext ptype : ) <$> mkPItemInfos xs

  _iPeriodTimes <- mkPItemInfos periodStarts

  let periodInfo = _iPeriodTimes !! fromIntegral periodId
  let _iPeriod =
          Period
          { _pId = periodId
          , _pStartLevel = pmStartLevel
          , _pCurLevel   = pmLastBlockLevel
          , _pEndLevel   = pmEndLevel
          , _pStartTime  = _piiStartTime periodInfo
          , _pEndTime    = _piiEndTime periodInfo
          , _pCycle      = fromIntegral $ (pmLastBlockLevel - pmStartLevel + 1) `div` oneCycle
          }
  let _iTotalPeriods = fromIntegral lastPeriodId + 1
  _iDiscourseLink <- askDiscourseHost

  let voteStats = VoteStats pmVotesCast pmVotesAvailable pmVotersNum pmTotalVotersNum

      notInLastPeriod :: a -> Maybe a
      notInLastPeriod val = if periodId == lastPeriodId then Nothing else Just val

  case pmType of
    Proposing -> do
      let _piVoteStats = voteStats
      _piWinner <- join . notInLastPeriod . rightToMaybe <$> getWinner periodId
      pure $ ProposalInfo {..}

    Exploration -> do
      _eiProposal <- getWinnerOrThrow (periodId - 1)
      let _eiVoteStats = voteStats
      let _eiBallots = Ballots pmBallotsYay pmBallotsNay pmBallotsPass (fromIntegral pmQuorum / 100.0) 80.0
      let _eiAdvanced = notInLastPeriod $ isVotePassed pmVotesAvailable _eiBallots
      pure $ ExplorationInfo{..}

    Testing -> do
      _tiProposal <- getWinnerOrThrow (periodId - 2)
      let _tiAdvanced = notInLastPeriod True
      pure $ TestingInfo{..}

    Promotion -> do
      _piProposal <- getWinnerOrThrow (periodId - 3)
      let _piVoteStats = voteStats
      let _piBallots = Ballots pmBallotsYay pmBallotsNay pmBallotsPass (fromIntegral pmQuorum / 100.0) 80.0
      let _piAdvanced = notInLastPeriod $ isVotePassed pmVotesAvailable _piBallots
      pure $ PromotionInfo{..}
  where
    noSuchPeriod = NotFound "Period with given number does not exist"

-- | Return the winner of proposal period.
-- Implying that this function is calling
-- only when a winner exists.
getWinner :: AgoraWorkMode m => PeriodId -> m (Either Text T.Proposal)
getWinner period = do
  proposals <- getProposals period
  case sortOn (Down . _prVotesCasted) proposals of
    [] -> pure $ Left $ "No proposals in period "+|period|+" are found."
    [prop] -> pure $ Right prop
    (prop1 : prop2 : _) ->
      if _prVotesCasted prop1 > _prVotesCasted prop2
      then pure $ Right prop1
      else pure $ Left $ "No clear winner in proposal period "+|period|+"."

-- | Version of `getWinner` which throws when winner is not found.
getWinnerOrThrow :: AgoraWorkMode m => PeriodId -> m T.Proposal
getWinnerOrThrow period = getWinner period >>= either (throwIO . InternalError) pure

-- | Determines if the vote has passed or not.
isVotePassed :: Votes -> Ballots -> Bool
isVotePassed votesTotal Ballots{..} =
  let participation = (fromIntegral (_bYay + _bNay + _bPass) / fromIntegral votesTotal) * 100
      majority = (fromIntegral _bYay / fromIntegral (_bYay + _bNay)) * 100
  in participation >= _bQuorum && majority >= _bSupermajority

-- | Fetch last known period.
-- It's possible that the database is empty and
-- last period is unknow, in this case @PeriodMetasNotFilledYet@ will be thrown.
getLastPeriod :: AgoraWorkMode m => m PeriodId
getLastPeriod = do
  perMb <- runSelectReturningOne' $ select $
    aggregate_ (max_ . pmId) (all_ $ asPeriodMetas agoraSchema)
  case perMb of
    Just (Just pid) -> pure pid
    _               -> throwIO PeriodMetasNotFilledYet

-- | Get all proposals for passed period.
getProposals
  :: AgoraWorkMode m
  => PeriodId
  -> m [T.Proposal]
getProposals periodId = do
  host <- askDiscourseHost
  results <-
    runSelectReturningList' $ select $ do
      prop <- all_ (asProposals agoraSchema)
      guard_ (DB.prPeriod prop ==. val_ (PeriodMetaId periodId))
      voter <- all_ (asVoters agoraSchema)
      guard_ (DB.prProposer prop `references_` voter)
      pure (prop, voter)
  pure $ sortOn (Down . \x -> (_prVotesCasted x, _prHash x)) $ map (convertProposal host) results

-- | Get info about proposal by proposal id.
getProposal
  :: AgoraWorkMode m
  => ProposalId
  -> m T.Proposal
getProposal propId = do
  host <- askDiscourseHost
  resultMb <-
    runSelectReturningOne' $ select $ do
      pr <- all_ (asProposals agoraSchema)
      voter <- all_ (asVoters agoraSchema)
      guard_ $ DB.prId pr ==. val_ (fromIntegral propId)
      guard_ $ DB.prProposer pr `references_` voter
      pure (pr, voter)
  result <- resultMb `whenNothing` throwIO (NotFound "Proposal with given id not exist")
  pure $ convertProposal host result

-- | Fetch proposal votes for a proposal
getSpecificProposalVotes
  :: AgoraWorkMode m
  => ProposalId
  -> Maybe ProposalVoteId
  -> Maybe Limit
  -> m (PaginatedList T.ProposalVote)
getSpecificProposalVotes propId mLastId mLimit = do
  let limit = fromMaybe 20 mLimit
      lastIdGuard
        :: forall s. ProposalVoteT (QExpr Postgres s)
        -> Q Postgres AgoraSchema s ()
      lastIdGuard pv = case mLastId of
        Nothing     -> pure ()
        Just lastId -> guard_ $ DB.pvId pv <. val_ (fromIntegral lastId)

  results <- runSelectReturningList' $ select $
    limit_ (fromIntegral limit) $ orderBy_ (desc_ . DB.pvId . (^. _1)) $ do
      prop <- all_ (asProposals agoraSchema)
      pv <- all_ (asProposalVotes agoraSchema)
      voter <- all_ (asVoters agoraSchema)

      guard_ $ DB.prId prop ==. val_ (fromIntegral propId)
      guard_ $ DB.pvProposal pv `references_` prop
      guard_ $ DB.pvVoter pv `references_` voter
      lastIdGuard pv

      pure (pv, voter, DB.prHash prop, DB.prDiscourseTitle prop)

  let pVotes = map convertProposalVote results
  buildPaginatedList limit (fromIntegral . _pvId) pVotes $ do
    pv <- all_ (asProposalVotes agoraSchema)
    guard_ $ DB.pvProposal pv ==. val_ (DB.ProposalId $ fromIntegral propId)
    lastIdGuard pv
    return pv

-- | Fetch proposal votes for period according to pagination params.
getProposalVotes
  :: AgoraWorkMode m
  => PeriodId
  -> Maybe ProposalVoteId
  -> Maybe Limit
  -> m (PaginatedList T.ProposalVote)
getProposalVotes periodId mLastId mLimit = do
  let limit = fromMaybe 20 mLimit
  -- fetch proposal votes with id less that lastId
  let sqlBody :: Q Postgres AgoraSchema s (ProposalVoteT (QExpr Postgres s))
      sqlBody =
        case mLastId of
          Nothing -> getAllProposalVotesForPeriod periodId
          Just lastId -> do
            x <- getAllProposalVotesForPeriod periodId
            guard_ $ DB.pvId x <. val_ (fromIntegral lastId)
            pure x

  results <- runSelectReturningList' $ select $
    limit_ (fromIntegral limit) $ orderBy_ (desc_ . DB.pvId . (^. _1)) $ do
      pv <- sqlBody
      prop <- all_ (asProposals agoraSchema)
      voter <- all_ (asVoters agoraSchema)
      -- fetch corresponding proposal hash and voter
      guard_ (DB.pvProposal pv `references_` prop)
      guard_ (DB.pvVoter pv `references_` voter)
      pure (pv, voter, DB.prHash prop, DB.prDiscourseTitle prop)

  buildPaginatedList limit (fromIntegral . _pvId) (map convertProposalVote results) sqlBody

-- | Fetch ballots for period according to pagination params.
getBallots
  :: AgoraWorkMode m
  => PeriodId
  -> Maybe BallotId
  -> Maybe Limit
  -> Maybe [Decision]
  -> m (PaginatedList T.Ballot)
getBallots periodId mLastId mLimit mDecs = do
  let limit = fromMaybe 20 mLimit
  let sqlBody :: Q Postgres AgoraSchema s (BallotT (QExpr Postgres s))
      sqlBody = do
        x <- all_ (asBallots agoraSchema)
        guard_ $ DB.bPeriod x ==. val_ (PeriodMetaId periodId) &&.
          case mLastId of
            Nothing     -> val_ True
            Just lastId -> DB.bId x <. val_ (fromIntegral lastId)
          &&.
          case mDecs of
            Nothing   -> val_ True
            Just decs -> in_ (DB.bBallotDecision x)  (map val_ $ S.toList $ S.fromList decs)
        pure x

  results <- runSelectReturningList' $ select $
    limit_ (fromIntegral limit) $ orderBy_ (desc_ . DB.bId . fst) $ do
      ballot <- sqlBody
      voter <- all_ (asVoters agoraSchema)
      guard_ (DB.bVoter ballot `references_` voter)
      pure (ballot, voter)

  buildPaginatedList limit (fromIntegral . _bId) (map convertBallot results) sqlBody

-- | Fetch ballots for period according to pagination params.
getNonVoters :: AgoraWorkMode m => PeriodId -> m [T.Baker]
getNonVoters period = do
  let AgoraSchema{..} = agoraSchema

  let filterVoters =
        VoterHash . DB.voterPbkHash <$>
        filter_ (\voter -> DB.voterPeriod voter ==. val_ (PeriodMetaId period)) (all_ asVoters)
  let filterBallots =
        DB.bVoter <$>
        filter_ (\ballot -> DB.bPeriod ballot ==. val_ (PeriodMetaId period)) (all_ asBallots)
  voters <- runSelectReturningList' $ select $ orderBy_ (desc_ . DB.voterRolls) $ do
    voterIdDiff <- except_ filterVoters filterBallots
    related_ asVoters voterIdDiff
  pure (map convertVoter voters)

-- | Takes limit, list and sql request
-- which selects entries to return and build PaginatedList.
buildPaginatedList
  ::
  ( AgoraWorkMode m
  , Projectible Postgres r
  )
  => Limit
  -> (a -> Word32)
  -> [a]
  -> Q Postgres AgoraSchema (QNested QBaseScope) r
  -> m (PaginatedList a)
buildPaginatedList limit fetchId plResults sqlCounter = do
  let pdLimit = Just limit
  let pdLastId =
        case plResults of
          [] -> Nothing
          _  -> Just $ fetchId $ U.last plResults

  rest <- fmap (fromMaybe 0) $
    runSelectReturningOne' $ select $ aggregate_ (const countAll_) sqlCounter
  let pdRest = fromIntegral $ rest - length plResults
  let plPagination = PaginationData {..}
  pure $ PaginatedList{..}

-- | Fetch all proposal votes in passed period.
getAllProposalVotesForPeriod
  :: PeriodId
  -> Q Postgres AgoraSchema s (ProposalVoteT (QExpr Postgres s))
getAllProposalVotesForPeriod periodId = do
  p <- all_ (asProposals agoraSchema)
  guard_ $ prPeriod p ==. val_ (PeriodMetaId periodId)
  oneToMany_ (asProposalVotes agoraSchema) pvProposal p

askDiscourseHost :: MonadAgoraConfig m => m Text
askDiscourseHost = fmt . build <$> fromAgoraConfig (sub #discourse . option #host)

---------------------------------------------------------------------------
-- Converters from db datatypes to corresponding web ones
---------------------------------------------------------------------------

convertVoter :: DB.Voter -> T.Baker
convertVoter DB.Voter{..} = Baker
  { _bkPkh     = voterPbkHash
  , _bkRolls   = voterRolls
  , _bkName    = fromMaybe "" voterName
  , _bkLogoUrl = voterLogoUrl
  , _bkProfileUrl = voterProfileUrl
  }

convertProposal :: Text -> (DB.Proposal, DB.Voter) -> T.Proposal
convertProposal discourseHost (DB.Proposal{prId=propId,..}, DB.Voter{..}) =
  T.Proposal
  { _prId = fromIntegral propId
  , _prPeriod = unPeriodMetaId prPeriod
  , _prHash = prHash
  , _prTitle = prDiscourseTitle
  , _prShortDescription = prDiscourseShortDesc
  , _prLongDescription = prDiscourseLongDesc
  , _prTimeCreated = prTimeProposed
  , _prProposalFile = prDiscourseFile
  , _prDiscourseLink = liftA2 sl (Just $ discourseHost `sl` "t") (fmt . build <$> prDiscourseTopicId)
  , _prProposer = Baker (unVoterHash prProposer) voterRolls (fromMaybe "" voterName) voterLogoUrl voterProfileUrl
  , _prVotesCasted = prVotesCast
  , _prVotersNum = prVotersNum
  }
  where
    sl a b = a <> "/" <> b

convertBallot :: (DB.Ballot, DB.Voter) -> T.Ballot
convertBallot (DB.Ballot{bId=ballId,..}, DB.Voter{..}) =
  T.Ballot
  { _bId = Id $ fromIntegral ballId
  , _bAuthor = Baker (unVoterHash bVoter)
               bCastedRolls (fromMaybe "" voterName) voterLogoUrl voterProfileUrl
  , _bDecision = bBallotDecision
  , _bOperation = bOperation
  , _bTimestamp = bBallotTime
  }

convertProposalVote :: (DB.ProposalVote, DB.Voter, ProposalHash, Maybe Text) -> T.ProposalVote
convertProposalVote (DB.ProposalVote{pvId=propVoteId,..}, DB.Voter{..}, pHash, mPTitle) =
  T.ProposalVote
  { _pvId = Id $ fromIntegral propVoteId
  , _pvProposal = pHash
  , _pvProposalTitle = mPTitle
  , _pvAuthor = Baker (unVoterHash pvVoter)
                pvCastedRolls (fromMaybe "" voterName) voterLogoUrl voterProfileUrl
  , _pvOperation = pvOperation
  , _pvTimestamp = pvVoteTime
  }
