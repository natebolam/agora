{-|
Instances of @MonadBeam@ and related classes for @MonadPostgresConn@,
also related type constraints.
-}
module Agora.DB.Class
       ( MonadBeamExtended
       , MonadPostgresDB
       ) where

import Database.Beam.Backend (MonadBeam (..))
import Database.Beam.Backend.SQL.BeamExtensions (MonadBeamDeleteReturning (..),
                                                 MonadBeamInsertReturning (..),
                                                 MonadBeamUpdateReturning (..))
import Database.Beam.Postgres (Postgres, runBeamPostgres)
import Monad.Capabilities (CapsT, HasCap)
import UnliftIO (MonadUnliftIO, withRunInIO)

import Agora.DB.Connection

{-
Instances which provide each @CapsT@ with @PostgresConn@ capability
the @MonadBeam@ functionality with extensions.
TODO: probably, it's better to provide those not through @Pg@ monad,
but through alternative Conduit interface.
-}

instance {-# OVERLAPPING #-} (MonadUnliftIO m, HasCap PostgresConn caps) =>
         MonadBeam Postgres (CapsT caps m) where
  runReturningMany q callback = withConnection $ \conn ->
    withRunInIO $ \unlift ->
    let callbackPg =
          liftIO . unlift . callback .
          liftIO . runBeamPostgres conn
    in runBeamPostgres conn $ runReturningMany q callbackPg

instance {-# OVERLAPPING #-} (MonadUnliftIO m, HasCap PostgresConn caps) =>
         MonadBeamInsertReturning Postgres (CapsT caps m) where
  runInsertReturningList insert = withConnection $ \conn ->
    liftIO $ runBeamPostgres conn $ runInsertReturningList insert

instance {-# OVERLAPPING #-} (MonadUnliftIO m, HasCap PostgresConn caps) =>
         MonadBeamUpdateReturning Postgres (CapsT caps m) where
  runUpdateReturningList update = withConnection $ \conn ->
    liftIO $ runBeamPostgres conn $ runUpdateReturningList update

instance {-# OVERLAPPING #-} (MonadUnliftIO m, HasCap PostgresConn caps) =>
         MonadBeamDeleteReturning Postgres (CapsT caps m) where
  runDeleteReturningList insert = withConnection $ \conn ->
    liftIO $ runBeamPostgres conn $ runDeleteReturningList insert

-- | Constraint which combines @MonadBeam@ class with its extensions.
type MonadBeamExtended be m =
  ( MonadBeam be m
  , MonadBeamInsertReturning be m
  , MonadBeamUpdateReturning be m
  , MonadBeamDeleteReturning be m
  )

-- | Constraint which defines full functionality of Postgres database
-- (including transactions).
type MonadPostgresDB m =
  ( MonadBeamExtended Postgres m
  , MonadPostgresConn m
  )
