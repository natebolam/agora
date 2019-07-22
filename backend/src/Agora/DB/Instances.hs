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

deriving instance IsPgValue Rolls
deriving instance FromField Rolls

deriving instance IsPgValue Quorum
deriving instance FromField Quorum

{- Enum instances -}

EnumInstanceEnc(Decision)
EnumInstanceDec(Decision)

EnumInstanceEnc(PeriodType)
EnumInstanceDec(PeriodType)

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
GenFromBackendRow(Rolls)
GenFromBackendRow(Quorum)
GenFromBackendRow(Decision)
GenFromBackendRow(PeriodType)
GenFromBackendRow(VoteType)

{- `HasSqlEqualityCheck` -}

deriving instance ( HasSqlEqualityCheck syntax ByteString
                  , BeamSqlBackend syntax) =>
  HasSqlEqualityCheck syntax (Hash a)

deriving instance ( HasSqlEqualityCheck syntax Int32
                  , BeamSqlBackend syntax) =>
  HasSqlEqualityCheck syntax (Id a)

GenHasSqlEqualityCheck(Cycle)
GenHasSqlEqualityCheck(Level)
GenHasSqlEqualityCheck(Votes)
GenHasSqlEqualityCheck(Rolls)
GenHasSqlEqualityCheck(Quorum)
GenHasSqlEqualityCheck(Decision)
GenHasSqlEqualityCheck(PeriodType)

instance ( HasSqlEqualityCheck syntax VoteType
                  , BeamSqlBackend syntax) =>
  HasSqlEqualityCheck syntax VoteType
