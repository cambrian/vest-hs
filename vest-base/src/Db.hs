module Db
  ( module Reexports
  , Config(..)
  ) where

import Database.Beam as Reexports hiding (insert)
import Database.Beam.Backend.SQL
import Database.Beam.Postgres as Reexports
import qualified Database.Beam.Postgres as Postgres
import Database.Beam.Postgres.Full as Reexports
import Database.PostgreSQL.Simple.FromField
import Database.PostgreSQL.Simple.Types as Reexports (PGArray)
import GHC.Base (String)
import qualified Money
import Vest

-- Postgres serializations for Vest types.
-- In theory you could implement this directly on Timestamp without having to create a UTCTime but
-- that's a bunch of work for what's only a smallish win.
-- No type is provided for Time/interval yet because postgresql-simple doesn't support DiffTime or
-- NominalDiffTime and implementing it would be complicated.
instance HasSqlValueSyntax be UTCTime => HasSqlValueSyntax be Timestamp where
  sqlValueSyntax = sqlValueSyntax . utcTimeFromTimestamp

instance FromField Timestamp where
  fromField f bs = fromField f bs >>- timestampFromUTCTime

instance FromBackendRow Postgres Timestamp

instance (HasSqlValueSyntax be Integer, KnownSymbol a, Money.GoodScale scale) =>
         HasSqlValueSyntax be (Money.Discrete' a scale) where
  sqlValueSyntax =
    sqlValueSyntax . Money.someDiscreteAmount . Money.toSomeDiscrete

instance (Money.GoodScale scale) => FromField (Money.Discrete' a scale) where
  fromField f bs = fromField f bs >>- Money.discrete @scale

instance (Money.GoodScale scale) =>
         FromBackendRow Postgres (Money.Discrete' a scale)

data Config = Config
  { host :: String
  , port :: Word16
  , user :: String
  , password :: String
  , database :: String
  } deriving (Generic, FromJSON, Show)

pgConnectInfo :: Config -> Postgres.ConnectInfo
pgConnectInfo Config {host, port, user, password, database} =
  Postgres.ConnectInfo host port user password database

instance Resource Connection where
  type ResourceConfig Connection = Config
  make = connect . pgConnectInfo
  cleanup = close
