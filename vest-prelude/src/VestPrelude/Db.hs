module VestPrelude.Db
  ( module VestPrelude.Db
  , module Reexports
  ) where

import Data.Time.Clock (UTCTime)
import Data.Time.Clock.System (SystemTime(..), systemToUTCTime, utcToSystemTime)
import Database.Beam as Reexports hiding (insert)
import Database.Beam.Backend.SQL
import Database.Beam.Postgres as Reexports
import Database.Beam.Postgres.Full as Reexports
import Database.PostgreSQL.Simple.FromField
import Database.PostgreSQL.Simple.Types as Reexports (PGArray)
import qualified Money
import VestPrelude

-- Postgres serializations for VestPrelude types.
-- In theory you could implement this directly on Timestamp without having to create a UTCTime
-- but that's a bunch of work for what's only a smallish win.
-- No type is provided for Time / interval yet because postgresql-simple doesn't support DiffTime or
-- NominalDiffTime and implementing it would be complicated
instance HasSqlValueSyntax be UTCTime => HasSqlValueSyntax be Timestamp where
  sqlValueSyntax = sqlValueSyntax . utcTimeFromTimestamp

instance FromField Timestamp where
  fromField f bs = fromField f bs >>- timestampFromUTCTime

instance FromBackendRow Postgres Timestamp

-- Beam doesn't support rationals so Money.Dense values can't be persisted. This is probably ok tho.
-- instance HasSqlValueSyntax be Rational =>
--          HasSqlValueSyntax be (Money.Dense a) where
--   sqlValueSyntax = sqlValueSyntax . toRational
-- instance FromField (Money.Dense a) where
--   fromField f bs = fromField f bs >>- Money.dense'
-- instance FromBackendRow Postgres (Money.Dense a)
instance (HasSqlValueSyntax be Integer, KnownSymbol a, Money.GoodScale scale) =>
         HasSqlValueSyntax be (Money.Discrete' a scale) where
  sqlValueSyntax =
    sqlValueSyntax . Money.someDiscreteAmount . Money.toSomeDiscrete

instance (Money.GoodScale scale) => FromField (Money.Discrete' a scale) where
  fromField f bs = fromField f bs >>- Money.discrete @scale

instance (Money.GoodScale scale) =>
         FromBackendRow Postgres (Money.Discrete' a scale)
