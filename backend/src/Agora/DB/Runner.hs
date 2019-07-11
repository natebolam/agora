module Agora.DB.Runner
       ( withPostgresConn
       ) where

import Control.Monad.Reader (withReaderT)
import Monad.Capabilities (CapsT, HasNoCap, addCap)
import qualified UnliftIO as UIO

import Agora.Config
import Agora.DB.Connection
import Agora.DB.Schema

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
  let action' pool =
        withReaderT (addCap $ postgresConnPooled pool) $
        transact (runPg ensureSchemaIsSetUp) >> action
  UIO.bracket
    (createConnPool connString maxConnsNum)
    destroyConnPool
    action'
