module Agora.DB.Runner
       ( withPostgresConn
       ) where

import Control.Monad.Reader (withReaderT)
import qualified Database.Beam.Postgres.Full as Pg
import Database.Beam.Query (insertValues, runInsert)
import Database.Beam.Schema (primaryKey)
import Monad.Capabilities (CapsT, HasNoCap, addCap)
import qualified UnliftIO as UIO

import Agora.Config
import Agora.DB.Connection
import Agora.DB.Instances ()
import Agora.DB.Schema
import Agora.Node.Types hiding (Voter)
import Agora.Types

-- | Adds a @PostgresConn@ capability into @CapsT@.
-- TODO: seems like it's time to think about proper resource
-- allocation/cleanup with `componentm`.
withPostgresConn
  :: (HasNoCap PostgresConn caps, HasAgoraConfig caps, UIO.MonadUnliftIO m)
  => CapsT (PostgresConn ': caps) m a
  -> CapsT caps m a
withPostgresConn action = do
  connString <- fromAgoraConfig $ sub #db . option #conn_string
  maxConnsNum <- fromAgoraConfig $ sub #db . option #max_connections
  predefinedBakers <- fromAgoraConfig $ option #predefined_bakers

  let toVoter BakerInfo {..} = Voter biDelegationCode (Just biBakerName) Nothing (Rolls 0)
      predefinedVoters = map toVoter predefinedBakers
      action' pool =
        withReaderT (addCap $ postgresConnPooled pool) $ do
          transact $ runPg $ do
            ensureSchemaIsSetUp
            runInsert $ Pg.insert (asVoters agoraSchema) (insertValues predefinedVoters) $
              Pg.onConflict (Pg.conflictingFields primaryKey) $
              Pg.onConflictUpdateInstead voterName
          action
  UIO.bracket
    (createConnPool connString maxConnsNum)
    destroyConnPool
    action'
