{-# LANGUAGE CPP #-}
module Agora.DB.Instances () where

import Database.Beam.Backend (BackendFromField, BeamBackend, BeamSqlBackend, FromBackendRow (..))
import Database.Beam.Backend.SQL.SQL92 (HasSqlValueSyntax (..), Sql92ExpressionValueSyntax)
import Database.Beam.Postgres.Syntax (PgValueSyntax)
import Database.Beam.Query (HasSqlEqualityCheck (..))
import Database.PostgreSQL.Simple.FromField (FromField (..))

import Agora.Types

{- Instance templates

CPP does not allow multi-line output, so writing one macros per instance.
TH would play better, but supposedly would also work slightly slower.
-}

#define IsPgValue HasSqlValueSyntax PgValueSyntax

#define EnumInstanceEnc(TYPE) \
instance IsPgValue (TYPE) where \
    sqlValueSyntax = sqlValueSyntax . fromEnum

#define EnumInstanceDec(TYPE) \
instance FromField (TYPE) where \
    fromField field ty = toEnum <$> fromField field ty

#define GenFromBackendRow(TYPE) \
instance (BeamBackend be, BackendFromField be (TYPE)) => FromBackendRow be (TYPE)

#define GenHasSqlEqualityCheck(TYPE) \
instance (HasSqlValueSyntax (Sql92ExpressionValueSyntax syntax) (TYPE), \
          BeamSqlBackend syntax) => \
         HasSqlEqualityCheck syntax (TYPE)

{- Newtype instances -}

deriving instance IsPgValue (Hash a)
deriving instance FromField (Hash a)

deriving instance IsPgValue (Id a)
deriving instance FromField (Id a)

deriving instance IsPgValue Cycle
deriving instance FromField Cycle

deriving instance IsPgValue Level
deriving instance FromField Level

deriving instance IsPgValue Votes
deriving instance FromField Votes

deriving instance IsPgValue Voters
deriving instance FromField Voters

deriving instance IsPgValue Rolls
deriving instance FromField Rolls

deriving instance IsPgValue Quorum
deriving instance FromField Quorum

deriving instance IsPgValue Stage
deriving instance FromField Stage

deriving instance IsPgValue Epoch
deriving instance FromField Epoch

{- Enum instances -}

EnumInstanceEnc(Decision)
EnumInstanceDec(Decision)

EnumInstanceEnc(StageType)
EnumInstanceDec(StageType)

EnumInstanceEnc(VoteType)
EnumInstanceDec(VoteType)

{- `FromBackendRow` -}

deriving instance (BeamBackend be, FromBackendRow be ByteString) =>
  FromBackendRow be (Hash a)

deriving instance (BeamBackend be, FromBackendRow be Int32) =>
  FromBackendRow be (Id a)

GenFromBackendRow(Cycle)
GenFromBackendRow(Level)
GenFromBackendRow(Votes)
GenFromBackendRow(Voters)
GenFromBackendRow(Rolls)
GenFromBackendRow(Quorum)
GenFromBackendRow(Decision)
GenFromBackendRow(StageType)
GenFromBackendRow(VoteType)
GenFromBackendRow(Stage)
GenFromBackendRow(Epoch)

{- `HasSqlEqualityCheck` -}

deriving instance ( HasSqlEqualityCheck syntax ByteString
                  , BeamSqlBackend syntax) =>
  HasSqlEqualityCheck syntax (Hash a)

deriving instance ( HasSqlEqualityCheck syntax Int32
                  , BeamSqlBackend syntax) =>
  HasSqlEqualityCheck syntax (Id a)

GenHasSqlEqualityCheck(Cycle)
GenHasSqlEqualityCheck(Votes)
GenHasSqlEqualityCheck(Voters)
GenHasSqlEqualityCheck(Rolls)
GenHasSqlEqualityCheck(Quorum)
GenHasSqlEqualityCheck(StageType)

instance ( HasSqlEqualityCheck syntax VoteType
                  , BeamSqlBackend syntax) =>
  HasSqlEqualityCheck syntax VoteType

instance ( HasSqlEqualityCheck syntax VoteType
                  , BeamSqlBackend syntax) =>
  HasSqlEqualityCheck syntax Decision

instance ( HasSqlEqualityCheck syntax Stage
                  , BeamSqlBackend syntax) =>
  HasSqlEqualityCheck syntax Stage

instance ( HasSqlEqualityCheck syntax Epoch
                  , BeamSqlBackend syntax) =>
  HasSqlEqualityCheck syntax Epoch

instance ( HasSqlEqualityCheck syntax Level
                  , BeamSqlBackend syntax) =>
  HasSqlEqualityCheck syntax Level
