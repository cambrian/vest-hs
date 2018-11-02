module Db
  ( module Reexports
  , PostgresConfig(..)
  ) where

import Database.Beam as Reexports hiding (insert)
import Database.Beam.Backend.SQL
import Database.Beam.Postgres as Reexports
import qualified Database.Beam.Postgres as Postgres
import Database.Beam.Postgres.Full as Reexports
import Database.PostgreSQL.Simple.FromField
import Database.PostgreSQL.Simple.Types as Reexports (PGArray)
import qualified GHC.Base
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

data PostgresConfig = PostgresConfig
  { connectHost :: GHC.Base.String
  , connectPort :: Word16
  , connectUser :: GHC.Base.String
  , connectPassword :: GHC.Base.String
  , connectDatabase :: GHC.Base.String
  } deriving (Generic, FromJSON, Show)

toConnectInfo :: PostgresConfig -> Postgres.ConnectInfo
toConnectInfo PostgresConfig { connectHost
                             , connectPort
                             , connectUser
                             , connectPassword
                             , connectDatabase
                             } =
  Postgres.ConnectInfo
    {connectHost, connectPort, connectUser, connectPassword, connectDatabase}

instance Resource Connection where
  type ResourceConfig Connection = PostgresConfig
  make = connect . toConnectInfo
  cleanup = close
