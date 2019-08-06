module Agora.DB.QuerySpec (spec) where

import Database.Beam.Query (all_, insert, insertValues, runInsert, runSelectReturningList, select)
import Test.Hspec (Spec, describe, it, shouldBe)
import Test.QuickCheck (vector, once)
import Test.QuickCheck.Monadic (monadicIO, pick)

import Agora.Arbitrary ()
import Agora.DB
import Agora.TestMode
import Agora.BlockStack

spec :: Spec
spec = withDbCapAll $
  describe "DB queries work" $
    it "can read the value which has just been inserted" $ \dbCap -> once $ monadicIO $ do
      blockStackImpl <- lift blockStackCapOverDbImplM
      discourseEndpoints <- lift inmemoryDiscourseEndpointsM
      agoraPropertyM dbCap (emptyTezosClient, discourseEndpoints, blockStackImpl) $ do
        voterHashesRolls <- pick $ vector 10
        let voters = map (\(h, r) -> Voter h Nothing Nothing r) voterHashesRolls
        lift $ runPg $ runInsert $ insert (asVoters agoraSchema) $ insertValues voters
        voters' <- lift $ runPg $ runSelectReturningList $ select (all_ $ asVoters agoraSchema)
        return $ voters' `shouldBe` voters
