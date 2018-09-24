module VestPrelude.Db
  ( module VestPrelude.Db
  , module Reexports
  ) where

import Data.Time.Clock (UTCTime)
import Data.Time.Clock.System (SystemTime(..), systemToUTCTime, utcToSystemTime)
import Data.Time.Format (defaultTimeLocale, formatTime, parseTimeM)
import Database.Beam as Reexports hiding (insert)
import Database.Beam.Backend.SQL
import Database.Beam.Backend.Types
import Database.Beam.Postgres as Reexports
import Database.PostgreSQL.Simple.FromField
import qualified Money
import VestPrelude

-- Postgres serializations for VestPrelude types
instance HasSqlValueSyntax be Text => HasSqlValueSyntax be (Id a) where
  sqlValueSyntax (Id text) = sqlValueSyntax text

instance FromField (Id a) where
  fromField f bs = fromField f bs >>- Id

instance FromBackendRow Postgres (Id a)

-- UTCTime <--> Timestamp
-- TODO: keep nanos
utcTimeFromTimestamp :: Timestamp -> UTCTime
utcTimeFromTimestamp (Timestamp seconds) =
  systemToUTCTime $
  MkSystemTime {systemSeconds = truncate seconds, systemNanoseconds = 0}

timestampFromUTCTime :: UTCTime -> Timestamp
timestampFromUTCTime utcTime =
  let MkSystemTime {systemSeconds} = utcToSystemTime utcTime
   in fromUnixTime systemSeconds

-- In theory you could implement this directly on Timestamp without having to create a UTCTime
-- but that's a bunch of work for what's only a smallish win.
instance HasSqlValueSyntax be UTCTime => HasSqlValueSyntax be Timestamp where
  sqlValueSyntax = sqlValueSyntax . utcTimeFromTimestamp

instance FromField Timestamp where
  fromField f bs = fromField f bs >>- timestampFromUTCTime

instance FromBackendRow Postgres Timestamp

-- No type is provided for Time / interval because for contracts and such we want a nominal
-- duration instead of a fixed one.
instance HasSqlValueSyntax be Rational =>
         HasSqlValueSyntax be (Money.Dense a) where
  sqlValueSyntax = sqlValueSyntax . toRational

instance FromField (Money.Dense a) where
  fromField f bs = fromField f bs >>- Money.dense'

instance FromBackendRow Postgres (Money.Dense a)
-- instance HasSqlValueSyntax be Integer =>
--          HasSqlValueSyntax be (Money.Discrete' a scale) where
--   sqlValueSyntax =
--     sqlValueSyntax . Money.someDiscreteAmount . Money.toSomeDiscrete
-- instance FromField (Money.Discrete' a scale) where
--   fromField f bs = fromField f bs >>- Money.discrete @scale
-- instance FromBackendRow Postgres (Money.Discrete' a scale)
-- instance Bounded (Money.Dense a)
-- instance Enum (Money.Dense a)
-- instance (KnownSymbol a) => SqlType (Money.Dense a)
-- instance (KnownSymbol a) => SqlOrd (Money.Dense a)
-- instance Money.GoodScale b => Bounded (Money.Discrete' a b)
-- instance (KnownSymbol a, Money.GoodScale b, Typeable b) =>
--          SqlType (Money.Discrete' a b)
-- instance (KnownSymbol a, Money.GoodScale b, Typeable b) =>
--          SqlOrd (Money.Discrete' a b)
