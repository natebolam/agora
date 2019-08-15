module Agora.Web.HandlersSpec (spec) where

import Data.List (nub)
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Time.Clock (addUTCTime)
import Database.Beam.Query (all_, guard_, select, val_, (==.))
import Monad.Capabilities (CapImpl (..), CapsT)
import Test.Hspec (Expectation, Spec, describe, it, shouldBe)
import Test.QuickCheck (Gen, arbitrary, choose, elements, shuffle, sublistOf, vector,
                        withMaxSuccess)
import Test.QuickCheck.Monadic (monadicIO, pick)

import Agora.Arbitrary ()
import Agora.BlockStack
import qualified Agora.DB as DB
import Agora.Mode
import Agora.Node
import Agora.Types
import Agora.Util
import Agora.Web

import Agora.Node.Blockchain
import Agora.TestMode

spec :: Spec
spec = withDbCapAll $ describe "API handlers" $ do
  it "getPeriodInfo and getProposals" $ \dbCap -> withMaxSuccess 3 $ monadicIO $ do
    let onePeriod = tzOnePeriod testTzConstants
    fbc@FilledBlockChain{..} <- pick genFilledBlockChain
    let chainLen = bcLen fbcChain

    let (_uniqueOps, propsStat, castedProposal, votersNum) = computeProposalResults fbcVoters fbcProposalOps
    let totalVotes = fromIntegral $ sum $ toList fbcVoters
    let ballots = computeExplorationResults fbcVoters fbcBallotOps

    let clientWithVoters :: Monad m => TezosClient m
        clientWithVoters = (inmemoryClientRaw fbcChain)
          { _fetchVoters = \_ _ -> pure $ map (uncurry Voter) $ M.toList fbcVoters
          }
    blockStackImpl <- lift blockStackCapOverDbImplM
    discourseEndpoints <- lift inmemoryDiscourseEndpointsM
    agoraPropertyM dbCap (CapImpl clientWithVoters, discourseEndpoints, blockStackImpl) $ do
      lift bootstrap
      oneCycle <- lift $ tzCycleLength <$> askTzConstants

      -- getPeriodInfo for Proposal period
      let startPropTime = addUTCTime (60 * fromIntegral (onePeriod + 1)) $ blockTimestamp genesisBlock
      let expectedProposalInfo =
            ProposalInfo
            { _iPeriod = Period
              { _pId         = 1
              , _pStartLevel = onePeriod + 1
              , _pEndLevel   = 2 * onePeriod
              , _pStartTime  = startPropTime
              , _pEndTime    = addUTCTime (60 * fromIntegral onePeriod) startPropTime
              , _pCycle      = 8
              }
            , _iTotalPeriods = 3
            , _piVoteStats = VoteStats castedProposal totalVotes votersNum
            , _iDiscourseLink = testDiscourseHostText
            }
      actualProposalInfo <- lift $ getPeriodInfo (Just 1)

      -- getPeriodInfo for Exploration period
      let startExpTime = addUTCTime (60 * fromIntegral (2 * onePeriod + 1)) $ blockTimestamp genesisBlock
      let expectedExplorationInfo =
            ExplorationInfo
            { _iPeriod = Period
              { _pId = 2
              , _pStartLevel = 2 * onePeriod + 1
              , _pEndLevel   = 3 * onePeriod
              , _pStartTime  = startExpTime
              , _pEndTime    = addUTCTime (60 * fromIntegral onePeriod) startExpTime
              , _pCycle      = fromIntegral $ (chainLen - 2 * onePeriod - 1) `div` oneCycle
              }
            , _iTotalPeriods = 3
            , _iDiscourseLink = testDiscourseHostText
            , _eiProposal    = buildProposal fbc (fbcWinner, propsStat M.! fbcWinner)
            , _eiVoteStats   = VoteStats (_bYay ballots + _bNay ballots + _bPass ballots)
                               totalVotes (length fbcBallotOps)
            , _eiBallots     = ballots
            }
      -- pva701: discourse url discarded, will be handled when tests for AG-77/AG-79 is added
      actualExplorationInfo <- discardId (eiProposal . prId) . set (eiProposal . prDiscourseLink) Nothing
        <$> lift (getPeriodInfo Nothing)

      -- getProposals
      let expectedProposals =
            sortOn (Down . \x -> (_prVotesCasted x, _prHash x))
            $ map (buildProposal fbc) $ M.toList propsStat
      -- pva701: discourse url discarded, will be handled when tests for AG-77/AG-79 is added
      actualProposals <- map (discardId prId . set prDiscourseLink Nothing) <$> lift (getProposals 1)

      return $ do
        actualProposalInfo `shouldBe` expectedProposalInfo
        actualProposals `shouldBe` expectedProposals
        actualExplorationInfo `shouldBe` expectedExplorationInfo

  it "getProposalVotes, getSpecificProposalVotes and getBallots" $ \dbCap -> withMaxSuccess 3 $ monadicIO $ do
    fbc@FilledBlockChain{..} <- pick genFilledBlockChain

    let (uniqueOps, _, _, _) = computeProposalResults fbcVoters fbcProposalOps
    let clientWithVoters :: Monad m => TezosClient m
        clientWithVoters = (inmemoryClientRaw fbcChain)
          { _fetchVoters = \_ _ -> pure $ map (uncurry Voter) $ M.toList fbcVoters
          }
    blockStackImpl <- lift blockStackCapOverDbImplM
    discourseEndpoints <- lift inmemoryDiscourseEndpointsM
    agoraPropertyM dbCap (CapImpl clientWithVoters, discourseEndpoints, blockStackImpl) $ do
      lift bootstrap
      let proposalVotes = map (buildProposalVote fbc) (reverse uniqueOps)
          pMapAlter a Nothing   = Just [a]
          pMapAlter a (Just as) = Just (a : as)
          pCollect pv = M.alter (pMapAlter pv) (_pvProposal pv)
          pVotesMap = foldr' pCollect mempty proposalVotes
          ballots = map (buildBallot fbc) (reverse fbcBallotOps)

      testPropHashes <- pick $ sublistOf $ M.keys pVotesMap
      testPropIds <- lift $ forM testPropHashes $ \h ->
        fmap (maybe (error "no proposal in db for given hash") (fromIntegral . DB.prId)) $
        DB.runSelectReturningOne' $ select $ do
          prop <- all_ (DB.asProposals DB.agoraSchema)
          guard_ $ DB.prHash prop ==. val_ h
          pure prop

      let hToIds = M.fromList $ zip testPropHashes testPropIds
          pVotesMap' = M.restrictKeys pVotesMap $ S.fromList testPropHashes
          pVotesMapIds = M.toList $ M.mapKeys (hToIds M.!) pVotesMap'

      ex1 <- lift $ testPaginatedEndpoint 1 pvId proposalVotes getProposalVotes
      ex2 <- lift $ testPaginatedEndpoint 2 bId ballots (\p lst lim -> getBallots p lst lim Nothing)
      ex3 <- lift $ testPaginatedEndpoint 1 bId [] (\p lst lim -> getBallots p lst lim Nothing)
      exes <- lift $ forM pVotesMapIds $ \(propId, pVotes) ->
        testPaginatedEndpoint 1 pvId pVotes (\_period lst lim -> getSpecificProposalVotes propId lst lim)
      pure $ ex1 >> ex2 >> ex3 >> sequence_ exes
  where
    getBakerName bakersInfo addr = maybe "" biBakerName $
      M.lookup addr bakersInfo

    buildProposalVote fbc@FilledBlockChain{..} (ProposalOp op src _period [prop]) =
      ProposalVote
      { _pvId        = 0
      , _pvProposal  = prop
      , _pvAuthor    = Baker src (fbcVoters M.! src) (getBakerName fbcBakersInfo src) Nothing
      , _pvOperation = op
      , _pvTimestamp = getPropTime fbc op
      }
    buildProposalVote _ _ = error "buildProposalVote unexpected input"

    buildBallot fbc@FilledBlockChain{..} (BallotOp op src _period _prop dec) =
      Ballot
      { _bId = 0
      , _bAuthor = Baker src (fbcVoters M.! src) (getBakerName fbcBakersInfo src) Nothing
      , _bDecision = dec
      , _bOperation = op
      , _bTimestamp = getBallotTime fbc op
      }
    buildBallot _ _ = error "buildBallot unexpected input"

    buildProposal fbc@FilledBlockChain{..} (prop, (author, op, castedProp)) =
      Proposal
      { _prId           = 0
      , _prPeriod       = 1
      , _prHash         = prop
      , _prTitle = Just $ shortenHash prop
      , _prShortDescription = Nothing, _prLongDescription = Nothing
      , _prTimeCreated  = getPropTime fbc op
      , _prProposalFile = Nothing
      , _prDiscourseLink = Nothing
      , _prProposer     = Baker author (fbcVoters M.! author) (getBakerName fbcBakersInfo author) Nothing
      , _prVotesCasted  = castedProp
      }

    getPropTime FilledBlockChain{..} op = blockTimestamp $ bcBlocks fbcChain M.! (fbcWherePropOps M.! op)
    getBallotTime FilledBlockChain{..} op = blockTimestamp $ bcBlocks fbcChain M.! (fbcWhereBallotOps M.! op)

testPaginatedEndpoint
  :: (Integral i, Show a, Eq a)
  => PeriodId
  -> Lens' a i
  -> [a]
  -> (PeriodId -> Maybe i -> Maybe Limit -> CapsT AgoraCaps IO (PaginatedList a))
  -> CapsT AgoraCaps IO Expectation
testPaginatedEndpoint period lens expected endpoint = do
  let limit1 = length expected `div` 2
  let limit2 = length expected - limit1
  (lastId, ex1) <- callEndpoint Nothing limit1 limit2 (take limit1 expected)

  (_, ex2) <- callEndpoint lastId limit2 0 (drop limit1 expected)
  pure $ ex1 >> ex2
  where
    callEndpoint lastId limit rest exPart = do
      actualPgList <-
        discardId (plResultsL . traverse . lens) <$> endpoint period lastId (Just $ fromIntegral limit)
      let lastIdRet = fromIntegral <$> pdLastId (plPagination actualPgList)
      let expPgList =
            PaginatedList
            { plPagination = PaginationData
              { pdRest  = fromIntegral rest
              , pdLimit = Just $ fromIntegral limit
              , pdLastId = fromIntegral <$> lastIdRet -- stolen from response, because it's hard to estimate id
              }
            , plResults = exPart
            }
      pure (lastIdRet, actualPgList `shouldBe` expPgList)

data FilledBlockChain = FilledBlockChain
  { fbcChain          :: BlockChain
  , fbcVoters         :: Map PublicKeyHash Rolls
  , fbcBakersInfo     :: Map PublicKeyHash BakerInfo
  , fbcProposalOps    :: [Operation]
  , fbcBallotOps      :: [Operation]
  , fbcWherePropOps   :: Map OperationHash BlockHash
  , fbcWhereBallotOps :: Map OperationHash BlockHash
  , fbcWinner         :: ProposalHash
  } deriving Show

genFilledBlockChain :: Gen FilledBlockChain
genFilledBlockChain = do
    let onePeriod = tzOnePeriod testTzConstants
    rest <- choose (2, onePeriod - 1)
    let chainLen = 2 * onePeriod + rest
    emptyBc <- genBlockChainSkeleton [Proposing, Proposing, Exploration] (fromIntegral chainLen)
    (propsNum, votersNum) <- (,) <$> choose (1, 5) <*> choose (4, 20)
    (proposals, votersPk, proposalOpsNum) <-
      (,,)
        <$> vector propsNum
        <*> ((map biDelegationCode (bilBakers testBakers) <>) <$> vector votersNum)
        <*> choose (1, propsNum * votersNum)
    fbcVoters <- M.fromList . zip votersPk <$> vector (length votersPk)
    fbcProposalOps <- genProposalOpsWithWinner proposals fbcVoters 1 proposalOpsNum
    (newBc', bkhProps) <- distributeOperations fbcProposalOps (onePeriod + 1, 2 * onePeriod) emptyBc
    let fbcWherePropOps = M.fromList $ zip (map opHash fbcProposalOps) bkhProps
    let (_uniqueOps, propsStat, _castedProposal, _vNum) = computeProposalResults fbcVoters fbcProposalOps
    let fbcWinner = fromMaybe (error "winner not found") $ chooseWinner propsStat
    let fbcBakersInfo = M.fromList $ map (\bi -> (biDelegationCode bi, bi)) $ bilBakers testBakers

    ballotOpsNum <- choose (1, votersNum)
    fbcBallotOps <- genBallotOps fbcWinner votersPk 2 ballotOpsNum
    (fbcChain, bkhBallots) <- distributeOperations fbcBallotOps (2 * onePeriod + 1, chainLen - 1) newBc'
    let fbcWhereBallotOps = M.fromList $ zip (map opHash fbcBallotOps) bkhBallots
    pure $ FilledBlockChain {..}

-- Exploration
genBallotOps
  :: ProposalHash
  -> [PublicKeyHash]
  -> PeriodId
  -> Int
  -> Gen [Operation]
genBallotOps prop allVoters period amount = do
  voters <- take amount <$> shuffle allVoters
  forM voters $ \pkh -> do
    dec <- arbitrary
    op <- arbitrary
    pure $ BallotOp op pkh period prop dec

computeExplorationResults
  :: Map PublicKeyHash Rolls
  -> [Operation]
  -> Ballots
computeExplorationResults voters =
  foldl (\b (BallotOp _ pk _ _ dec) -> case dec of
                Yay  -> b & bYay %~ (+ getRolls pk)
                Nay  -> b & bNay %~ (+ getRolls pk)
                Pass -> b & bPass %~ (+ getRolls pk))
    (Ballots 0 0 0 80.0 80.0)
  where
    getRolls v = fromIntegral $ voters M.! v

-- Proposal
genProposalOpsWithWinner
  :: [ProposalHash]
  -> Map PublicKeyHash Rolls
  -> PeriodId
  -> Int
  -> Gen [Operation]
genProposalOpsWithWinner props voters periodId opsNum = do
  let votersPk = M.keys voters
  ops <- genProposalOps props votersPk periodId opsNum
  case chooseWinner (computeProposalResults voters ops ^. _2) of
    Just _  -> pure ops
    Nothing -> genProposalOpsWithWinner props voters periodId opsNum

chooseWinner
  :: Map ProposalHash (PublicKeyHash, OperationHash, Votes)
  -> Maybe ProposalHash
chooseWinner props =
  case sortOn (Down . view _3 . snd) $ M.toList props of
    []  -> Nothing
    [x] -> Just $ fst x
    (x : y : _)
      | snd x ^. _3 == snd y ^. _3 -> Nothing
      | otherwise -> Just (fst x)

computeProposalResults
  :: Map PublicKeyHash Rolls
  -> [Operation]
  -> ( [Operation] -- operations which have to get into db
     , Map ProposalHash (PublicKeyHash, OperationHash, Votes) -- map from proposal to (author, sum of rolls for the proposal)
     , Votes -- casted votes
     , Int   -- number of voters which casted some vote
     )
computeProposalResults voters proposalOps = do
  let getRolls v = fromIntegral $ voters M.! v
  let uniques =
        reverse $ fst $
          foldl (\(ops, diff) oper@(ProposalOp _ pkh _ [prop]) ->
                if S.member (pkh, prop) diff then (ops, diff)
                else (oper : ops, S.insert (pkh, prop) diff))
            ([], mempty)
            proposalOps
  let votesForProps =
        foldl (\vts (ProposalOp op pkh _ [prop]) ->
                 M.alter (\case
                             Nothing -> Just (pkh, op, getRolls pkh)
                             Just (auth, h, tot) -> Just (auth, h, getRolls pkh + tot))
                 prop
                 vts)
          mempty
          uniques
  let casted = sum $ map getRolls $ nub $ map source proposalOps
  let votersNum = S.size $ S.fromList $
        map (\(ProposalOp _ pkh _ _) -> pkh) proposalOps
  (uniques, votesForProps, casted, votersNum)

genProposalOps
  :: [ProposalHash]
  -> [PublicKeyHash]
  -> PeriodId
  -> Int
  -> Gen [Operation]
genProposalOps props voters periodId opsNum = replicateM opsNum $ do
  voter <- elements voters
  prop <- elements props
  op <- arbitrary
  pure $ ProposalOp op voter periodId [prop]
