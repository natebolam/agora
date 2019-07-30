{-|
A module which defines functions and datatypes to handle the connections
to PostgreSQL database.
-}
module Agora.DB.Connection
       ( ConnPool
       , ConnString (..)
       , createConnPool
       , destroyConnPool
       , PostgresConn (..)
       , MonadPostgresConn (..)
       , postgresConnPooled
       , postgresConnSingle
       -- * Pg runners
       , runPg
       , runPgDebug
       , runInsert'
       , runUpdate'
       , runDelete'
       , runSelectReturningOne'
       , runSelectReturningList'
       ) where

import Control.Monad.Reader (withReaderT)
import Data.Pool (Pool, createPool, destroyAllResources, tryWithResource, withResource)
import Data.Time.Clock (nominalDay)
import Database.Beam.Backend (FromBackendRow)
import Database.Beam.Postgres (Connection, Pg, Postgres, close, connectPostgreSQL, runBeamPostgres,
                               runBeamPostgresDebug)
import Database.Beam.Query (SqlDelete, SqlInsert, SqlSelect, SqlUpdate, runDelete, runInsert,
                            runSelectReturningList, runSelectReturningOne, runUpdate)
import Database.PostgreSQL.Simple.Transaction (withSavepoint, withTransactionSerializable)
import Loot.Log (MonadLogging, logDebug)
import Monad.Capabilities (CapImpl (..), Capabilities, HasCap, overrideCap, withCap)
import UnliftIO (MonadUnliftIO, withRunInIO)

import Agora.Util

-- | Database connection pool. One @Connection@ can not be used simultaneously
-- by multiple threads, so we need a pool of connections to allow for
-- simultaneous DB queries.
newtype ConnPool = ConnPool
  { unConnPool :: Pool Connection
  } deriving (Show)

-- | Creates a @ConnPool@ with PostgreSQL connections which use given
-- connection string.
createConnPool
  :: MonadIO m
  => ConnString
  -> Int
  -> m ConnPool
createConnPool (ConnString connStr) maxConnsNum = liftIO $ ConnPool <$>
  createPool
  (connectPostgreSQL connStr)   -- connection creation action
  close                         -- connection destroy action
  1                             -- number of individual pools (just one is fine)
  nominalDay                    -- maximum time the connection should remain open while unused
  maxConnsNum                   -- maximum number of DB connections.

-- | Destroys a @ConnPool@.
destroyConnPool :: MonadIO m => ConnPool -> m ()
destroyConnPool (ConnPool pool) = liftIO $ destroyAllResources pool

-- | Capability which provides methods for working with connections
-- to Postgres database.
data PostgresConn m = PostgresConn
  { _withConnection    :: forall a. (Connection -> m a) -> m a
  , _tryWithConnection :: forall a. (Connection -> m a) -> m (Maybe a)
  , _withTransaction   :: forall a. Connection -> m a -> m a
  }

-- | Implementation of @PostgresConn@ capability which uses a
-- @ConnPool@ to support multithreading. Methods have the following
-- properties:
--
-- * `withConnection` blocks if no idle connections are available
--   and the maximum number of DB connections has been reached
-- * `tryWithConnection` returns `Nothing` immediately in the same situation.
-- * `withTransaction` uses @withTransactionSerializable@ as implementation.
postgresConnPooled
  :: MonadUnliftIO m
  => ConnPool
  -> CapImpl PostgresConn '[] m
postgresConnPooled (ConnPool pool) = CapImpl $ PostgresConn
  { _withConnection = \action -> withRunInIO $ \unlift ->
      withResource pool (unlift . action)
  , _tryWithConnection = \action -> withRunInIO $ \unlift ->
      tryWithResource pool (unlift . action)
  , _withTransaction = \conn action -> withRunInIO $ \unlift ->
      withTransactionSerializable conn (unlift action)
  }

-- | Implementation of @PostgresConn@ which simply uses the single
-- connection. It's only intended to be constructed inside @transact@
-- in order to make sure that all database actions wrapped with @transact@
-- are performed with a single connection.
--
-- `tryWithConnection` always returns `Just` in this case,
-- and `withTransaction` implementation is @withSavepoint@ to support
-- "nested" transactions (https://www.postgresql.org/docs/9.5/sql-savepoint.html).
postgresConnSingle
  :: MonadUnliftIO m
  => Connection
  -> CapImpl PostgresConn '[] m
postgresConnSingle conn = CapImpl $ PostgresConn
  { _withConnection = ($ conn)
  , _tryWithConnection = fmap Just . ($ conn)
  , _withTransaction = \conn' action -> withRunInIO $ \unlift ->
      withSavepoint conn' (unlift action)
  }

-- | Monadic typeclass which corresponds to @PostgresConn@ capability.
-- We don't use @makeCap@ here because instead of providing @withTransaction@
-- method which directly corresponds to the capability field we provide a
-- @transact@ method which overrides the capability implementation inside
-- the wrapped action.
class Monad m => MonadPostgresConn m where
  withConnection :: (Connection -> m a) -> m a
  tryWithConnection :: (Connection -> m a) -> m (Maybe a)
  transact :: m a -> m a

instance (HasCap PostgresConn caps, r ~ Capabilities caps m, MonadUnliftIO m) =>
         MonadPostgresConn (ReaderT r m) where
  withConnection action = withCap $ \cap -> _withConnection cap action
  tryWithConnection action = withCap $ \cap -> _tryWithConnection cap action
  transact action = withCap $ \cap -> _withConnection cap $ \conn ->
    _withTransaction cap conn $
    withReaderT (overrideCap $ postgresConnSingle conn) action

-- | Helper method which runs a @Pg@ action inside @MonadPostgresConn@.
runPg :: (MonadIO m, MonadPostgresConn m) => Pg a -> m a
runPg pg = withConnection $ \conn -> liftIO $ runBeamPostgres conn pg

runPgDebug
  :: (MonadUnliftIO m, MonadPostgresConn m, MonadLogging m)
  => Pg a -> m a
runPgDebug pg =
  withConnection $
    \conn ->
      withRunInIO $
        \runner -> runBeamPostgresDebug (runner . logDebug . fromString) conn pg

runInsert'
  :: (MonadIO m, MonadPostgresConn m)
  => SqlInsert Postgres table -> m ()
runInsert' = runPg . runInsert

runUpdate'
  :: (MonadIO m, MonadPostgresConn m)
  => SqlUpdate Postgres tbl -> m ()
runUpdate' = runPg . runUpdate

runDelete'
  :: (MonadIO m, MonadPostgresConn m)
  => SqlDelete Postgres tbl -> m ()
runDelete' = runPg . runDelete

runSelectReturningOne'
  :: ( FromBackendRow Postgres a, MonadIO m, MonadPostgresConn m)
  => SqlSelect Postgres a -> m (Maybe a)
runSelectReturningOne' = runPg . runSelectReturningOne

runSelectReturningList'
  :: ( FromBackendRow Postgres a, MonadIO m, MonadPostgresConn m)
  => SqlSelect Postgres a -> m [a]
runSelectReturningList' = runPg . runSelectReturningList
