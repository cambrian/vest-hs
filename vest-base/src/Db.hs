module Db
  ( module Reexports
  , Config(..)
  , HasConnection(..)
  , InvalidStateException(..)
  , runLogged
  , runLoggedTransaction
  ) where

import Database.Beam as Reexports hiding (insert)
import Database.Beam.Backend.SQL as Reexports
import Database.Beam.Postgres as Reexports
import qualified Database.Beam.Postgres as Postgres
import Database.Beam.Postgres.Full as Reexports
import Database.PostgreSQL.Simple.FromField as Reexports
import Database.PostgreSQL.Simple.Transaction (withTransactionSerializable)
import Database.PostgreSQL.Simple.Types as Reexports (PGArray)
import GHC.Base (String)
import qualified Money
import Vest

data InvalidStateException =
  InvalidStateException
  deriving (Eq, Ord, Show, Read, Generic, Exception, Hashable, FromJSON, ToJSON)

deriving instance Read a => Read (PgJSON a)

-- Postgres serializations for Vest types.
-- Tagged types have HasSqlValueSyntax defined in Beam but no FromField instances
instance FromField a => FromField (Tagged t a) where
  fromField f bs = Tagged <$> fromField f bs

instance FromField a => FromBackendRow Postgres (Tagged t a)

instance (HasSqlValueSyntax be Integer, KnownSymbol a, Money.GoodScale scale) =>
         HasSqlValueSyntax be (Money.Discrete' a scale) where
  sqlValueSyntax =
    sqlValueSyntax . Money.someDiscreteAmount . Money.toSomeDiscrete

instance Money.GoodScale scale => FromField (Money.Discrete' a scale) where
  fromField f bs = Money.discrete @scale <$> fromField f bs

instance Money.GoodScale scale =>
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

-- | Slightly different pattern from the normal HasX classes to encourage connection pooling.
class HasConnection t where
  withConnection :: t -> (Connection -> IO a) -> IO a

instance HasConnection Connection where
  withConnection conn f = f conn

instance HasConnection (Pool Connection) where
  withConnection = withResource

runLogged :: HasConnection t => t -> Pg a -> IO a
runLogged t f =
  withConnection t $ \c -> runBeamPostgresDebug (log Debug "SQL query") c f

runLoggedTransaction :: HasConnection t => t -> Pg a -> IO a
-- ^ TODO: Also log transaction open/close.
runLoggedTransaction t f =
  withConnection t $ \c ->
    withTransactionSerializable c $
    runBeamPostgresDebug (log Debug "SQL transaction") c f
