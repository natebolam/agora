{-# LANGUAGE TypeOperators #-}

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
       , withPostgresConn
       ) where

import Control.Monad.Reader (withReaderT)
import Data.Aeson (FromJSON (..), ToJSON (..), Value (..), withText)
import Data.Pool (Pool, createPool, destroyAllResources, tryWithResource, withResource)
import Data.Time.Clock (nominalDay)
import Database.Beam.Postgres (Connection, close, connectPostgreSQL)
import Database.PostgreSQL.Simple.Transaction (withSavepoint, withTransactionSerializable)
import Fmt (Buildable (..), (+|), (|+))
import Monad.Capabilities (CapImpl (..), Capabilities, CapsT, HasCap, HasNoCap, addCap, overrideCap,
                           withCap)
import UnliftIO (MonadUnliftIO, withRunInIO)

-- | Database connection pool. One @Connection@ can not be used simultaneously
-- by multiple threads, so we need a pool of connections to allow for
-- simultaneous DB queries.
newtype ConnPool = ConnPool
  { unConnPool :: Pool Connection
  } deriving (Show)

-- | Newtype which denotes LIBPQ connection string.
-- Syntax: https://www.postgresql.org/docs/9.5/libpq-connect.html#LIBPQ-CONNSTRING
newtype ConnString = ConnString
  { unConnString :: ByteString
  } deriving (Show, Eq, Ord)

instance FromJSON ConnString where
  parseJSON = withText "ConnString" $ pure . ConnString . encodeUtf8

instance ToJSON ConnString where
  toJSON = String . decodeUtf8 . unConnString

instance Buildable ConnString where
  build (ConnString s) = ""+|decodeUtf8 @Text s|+""

-- | Creates a @ConnPool@ with PostgreSQL connections which use given
-- connection string.
createConnPool
  :: MonadIO m
  => ConnString
  -> m ConnPool
createConnPool (ConnString connStr) = liftIO $ ConnPool <$>
  createPool
  (connectPostgreSQL connStr)   -- connection creation action
  close                         -- connection destroy action
  1                             -- number of individual pools (just one is fine)
  nominalDay                    -- maximum time the connection should remain open while unused
  200                           -- maximum number of DB connections. TODO: should it be configured?

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
class MonadPostgresConn m where
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

-- | Adds a @PostgresConn@ capability into @CapsT@.
-- TODO: seems like it's time to think about proper resource
-- allocation/cleanup with `componentm`.
withPostgresConn
  :: (HasNoCap PostgresConn caps, MonadUnliftIO m)
  => ConnString
  -> CapsT (PostgresConn ': caps) m a
  -> CapsT caps m a
withPostgresConn connString action = do
  pool <- createConnPool connString
  withReaderT (addCap $ postgresConnPooled pool) action
