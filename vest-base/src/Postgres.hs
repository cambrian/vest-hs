-- Even though this module represents a connection to a postgres database, it is not directly
-- loadable itself (unlike redis, amqp for example). This is because services generally connect
-- to their own specific databases instead of a shared database.
-- This means that when defining services that use databases, you need to newtype
-- Postgres.Connection so that you can provide a specific loadable instance.
module Postgres
  ( module Reexports
  , Config(..)
  , HasConnection(..)
  , runLogged
  , runLoggedTransaction
  , ensureSchema
  ) where

import Database.Beam as Reexports hiding (insert)
import Database.Beam.Backend.SQL as Reexports
import Database.Beam.Migrate as Reexports hiding (time)
import Database.Beam.Migrate.Simple
import Database.Beam.Postgres as Reexports
import qualified Database.Beam.Postgres as Postgres
import Database.Beam.Postgres.Full as Reexports
import Database.Beam.Postgres.Migrate as Postgres (migrationBackend)
import Database.PostgreSQL.Simple.FromField as Reexports
import Database.PostgreSQL.Simple.Transaction (withTransactionSerializable)
import Database.PostgreSQL.Simple.Types as Reexports (PGArray)
import GHC.Base (String)
import qualified Money
import Vest

deriving instance Read a => Read (PgJSON a)

-- Postgres serializations for Vest types.
-- Tagged types have HasSqlValueSyntax defined in Beam but no FromField instances
instance FromField a => FromField (Tagged t a) where
  fromField f bs = Tagged <$> fromField f bs

instance Money.GoodScale scale => FromField (Money.Discrete' a scale) where
  fromField f bs = Money.discrete @scale <$> fromField f bs

instance FromField a => FromBackendRow Postgres (Tagged t a)

instance Money.GoodScale scale =>
         FromBackendRow Postgres (Money.Discrete' a scale)

instance (HasSqlValueSyntax be Integer, KnownSymbol a, Money.GoodScale scale) =>
         HasSqlValueSyntax be (Money.Discrete' a scale) where
  sqlValueSyntax =
    sqlValueSyntax . Money.someDiscreteAmount . Money.toSomeDiscrete

instance HasDefaultSqlDataType syntax a =>
         HasDefaultSqlDataType syntax (Tagged t a) where
  defaultSqlDataType _ = defaultSqlDataType (Proxy :: Proxy a)

instance HasDefaultSqlDataType syntax Int =>
         HasDefaultSqlDataType syntax (Money.Discrete' a scale) where
  defaultSqlDataType _ = defaultSqlDataType (Proxy :: Proxy Int)

instance (IsSql92DataTypeSyntax syntax, HasDefaultSqlDataType syntax Text) =>
         HasDefaultSqlDataType syntax Time where
  defaultSqlDataType _ = defaultSqlDataType (Proxy :: Proxy Text)

instance HasDefaultSqlDataTypeConstraints syntax a =>
         HasDefaultSqlDataTypeConstraints syntax (Tagged t a) where
  defaultSqlDataTypeConstraints _ _ =
    defaultSqlDataTypeConstraints (Proxy :: Proxy a) (Proxy :: Proxy syntax)

instance HasDefaultSqlDataTypeConstraints syntax Int =>
         HasDefaultSqlDataTypeConstraints syntax (Money.Discrete' a scale) where
  defaultSqlDataTypeConstraints _ _ =
    defaultSqlDataTypeConstraints (Proxy :: Proxy Int) (Proxy :: Proxy syntax)

instance ( IsSql92ColumnSchemaSyntax syntax
         , HasDefaultSqlDataTypeConstraints syntax Text
         ) =>
         HasDefaultSqlDataTypeConstraints syntax Time where
  defaultSqlDataTypeConstraints _ _ =
    defaultSqlDataTypeConstraints (Proxy :: Proxy Text) (Proxy :: Proxy syntax)

data Config = Config
  { host :: String
  , port :: Word16
  , user :: String
  , password :: String
  , database :: String
  } deriving (Eq, Read, Show, Generic, FromJSON, ToJSON)

pgConnectInfo :: Config -> Postgres.ConnectInfo
pgConnectInfo Config {host, port, user, password, database} =
  Postgres.ConnectInfo host port user password database

instance Resource Connection where
  type ResourceConfig Connection = Config
  make = connect . pgConnectInfo
  cleanup = close

instance Loadable Connection where
  configFile = [relfile|postgres.yaml|]

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
-- ^ If parsing interleaved transaction logs is annoying, get a uuid and include it in the log
-- context.
runLoggedTransaction t f =
  withConnection t $ \c -> do
    log_ Debug "Begin SQL transaction."
    a <-
      withTransactionSerializable c $
      runBeamPostgresDebug (log Debug "SQL transaction") c f
    log_ Debug "End SQL transaction."
    return a

ensureSchema ::
     Database Postgres db => CheckedDatabaseSettings Postgres db -> Pg ()
ensureSchema schema = do
  verifyExists <- verifySchema Postgres.migrationBackend schema
  case verifyExists of
    VerificationSucceeded -> return ()
    VerificationFailed _ -> createSchema Postgres.migrationBackend schema
