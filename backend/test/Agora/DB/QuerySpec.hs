module Agora.DB.QuerySpec (spec) where

import Database.Beam.Query (all_, insert, insertValues, runInsert, runSelectReturningList, select)
import Test.Hspec (Spec, describe, it, shouldBe)
import Test.QuickCheck (vector)
import Test.QuickCheck.Monadic (pick)

import Agora.Arbitrary ()
import Agora.DB
import Agora.TestMode

spec :: Spec
spec = withTestCaps $ do
  describe "DB queries work" $ do
    it "can read the value which has just been inserted" $ agoraPropertyM $ do
      voterHashesRolls <- pick $ vector 10
      let voters = map (\(h, r) -> Voter h Nothing Nothing r) voterHashesRolls
      lift $ runPg $ runInsert $ insert (asVoters agoraSchema) $ insertValues voters
      voters' <- lift $ runPg $ runSelectReturningList $ select (all_ $ asVoters agoraSchema)
      return $ voters' `shouldBe` voters
