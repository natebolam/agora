module Agora.DB.QuerySpec (spec) where

import Database.Beam.Query (all_, insert, insertValues, runInsert, runSelectReturningList, select)
import Test.Hspec (Spec, describe, it, shouldBe)
import Test.QuickCheck (vector)
import Test.QuickCheck (once)
import Test.QuickCheck.Monadic (monadicIO, pick)
import qualified UnliftIO as UIO

import Agora.Arbitrary ()
import Agora.DB
import Agora.TestMode
import Agora.BlockStack

spec :: Spec
spec = withDbCapAll $ do
  describe "DB queries work" $ do
    it "can read the value which has just been inserted" $ \dbCap -> once $ monadicIO $ do
      tvar <- lift $ UIO.newTVarIO Nothing
      agoraPropertyM dbCap (emptyTezosClient, blockStackCapOverDbImpl tvar) $ do
        voterHashesRolls <- pick $ vector 10
        let voters = map (\(h, r) -> Voter h Nothing Nothing r) voterHashesRolls
        lift $ runPg $ runInsert $ insert (asVoters agoraSchema) $ insertValues voters
        voters' <- lift $ runPg $ runSelectReturningList $ select (all_ $ asVoters agoraSchema)
        return $ voters' `shouldBe` voters
