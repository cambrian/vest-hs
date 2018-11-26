-- |
-- In general you want to use service-specific connection types. The functions @runLogged and
-- @runLoggedTransaction only accept specific connection pools to encourage this pattern.
--
-- e.g.
-- data T = T
-- { dbPool :: Pool (Specific T Postgres.Connection)
-- ...
-- }
--
-- You load the dbPool normally; the only thing to be aware of is that Specific loadables prepend
-- a namespace to the configFile. So your config dir should look like this:
--
-- config/
--   <env>/
--     <service>/
--       postgres.yaml
--       postgres.pool.yaml
module Postgres
  ( module Reexports
  , Config(..)
  , runLogged
  , runLoggedTransaction
  , ensureSchema
  , IndexableTable(..)
  ) where

import Database.Beam as Reexports hiding (insert)
import Database.Beam.Backend.SQL as Reexports
import Database.Beam.Migrate as Reexports hiding (time)
import Database.Beam.Migrate.Simple
import Database.Beam.Postgres as Reexports
import qualified Database.Beam.Postgres as Postgres
import Database.Beam.Postgres.Full as Reexports
import qualified Database.Beam.Postgres.Migrate as Postgres (migrationBackend)
import Database.Beam.Postgres.Syntax as Reexports
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

instance HasDefaultSqlDataType syntax Int64 =>
         HasDefaultSqlDataType syntax (Money.Discrete' a scale) where
  defaultSqlDataType _ = defaultSqlDataType (Proxy :: Proxy Int64)

instance HasDefaultSqlDataType syntax a =>
         HasDefaultSqlDataType syntax (Tagged t a) where
  defaultSqlDataType _ = defaultSqlDataType (Proxy :: Proxy a)

instance HasDefaultSqlDataType PgDataTypeSyntax Time where
  defaultSqlDataType _ _ = timestampType Nothing True

instance HasDefaultSqlDataTypeConstraints PgColumnSchemaSyntax Time

instance HasDefaultSqlDataTypeConstraints syntax Int64 =>
         HasDefaultSqlDataTypeConstraints syntax (Money.Discrete' a scale) where
  defaultSqlDataTypeConstraints _ _ =
    defaultSqlDataTypeConstraints (Proxy :: Proxy Int64) (Proxy :: Proxy syntax)

instance HasDefaultSqlDataTypeConstraints syntax a =>
         HasDefaultSqlDataTypeConstraints syntax (Tagged t a) where
  defaultSqlDataTypeConstraints _ _ =
    defaultSqlDataTypeConstraints (Proxy :: Proxy a) (Proxy :: Proxy syntax)

-- -- | This would be for Enums... but we probably want to think of an alternate solution
-- instance {-# OVERLAPPABLE #-} ( IsSql92DataTypeSyntax syntax
--                               , HasDefaultSqlDataType syntax Int
--                               ) =>
--                               HasDefaultSqlDataType syntax a where
--   defaultSqlDataType _ = defaultSqlDataType (Proxy :: Proxy Int)
-- instance {-# OVERLAPPABLE #-} ( IsSql92ColumnSchemaSyntax syntax
--                               , HasDefaultSqlDataTypeConstraints syntax Int
--                               ) =>
--                               HasDefaultSqlDataTypeConstraints syntax a where
--   defaultSqlDataTypeConstraints _ _ = defaultSqlDataTypeConstraints (Proxy :: Proxy Int) (Proxy :: Proxy syntax)
-- instance Enum a => HasSqlEqualityCheck PgExpressionSyntax a where
--   a ==. b =
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

runLogged :: Pool (Specific s Connection) -> Pg a -> IO a
-- ^ Requires the connection to be service-specific.
runLogged pool f =
  withResource pool $ \c ->
    runBeamPostgresDebug (log Debug "SQL query") (base c) f

runLoggedTransaction :: Pool (Specific s Connection) -> Pg a -> IO a
-- ^ If parsing interleaved transaction logs is annoying, get a uuid and include it in the log
-- context.
runLoggedTransaction pool f =
  withResource pool $ \c -> do
    log_ Debug "begin SQL transaction"
    a <-
      withTransactionSerializable (base c) $
      runBeamPostgresDebug (log Debug "SQL transaction") (base c) f
    log_ Debug "end SQL transaction"
    return a

ensureSchema ::
     Database Postgres db => CheckedDatabaseSettings Postgres db -> Pg ()
-- ^ Will fail if data loss might occur.
ensureSchema schema = do
  verifyExists <- verifySchema Postgres.migrationBackend schema
  case verifyExists of
    VerificationSucceeded -> return ()
    VerificationFailed _ -> createSchema Postgres.migrationBackend schema

-- | I don't love the overloading of "Index", but not sure what might be better
--
-- Note: For Indexables, it's not advisable to use Postgres' serial type because serials produce
-- gaps in the case of serialization failures. Instead, you should manually read the next unseen
-- index and insert with that index in a transaction.
class (Table a) =>
      IndexableTable a
  where
  indexColumn :: a f -> C f Word64
  wasHandled ::
       (Database Postgres db, FromBackendRow Postgres (a Identity))
    => DatabaseEntity Postgres db (TableEntity a)
    -> Word64
    -> Pg Bool
  -- ^ This assumes that the table follows the convention of rows representing handled events.
  wasHandled table idx =
    not . null <$>
    runSelectReturningList
      (select $ filter_ (\row -> indexColumn row ==. val_ idx) $ all_ table)
  nextUnseenIndex ::
       Database Postgres db
    => DatabaseEntity Postgres db (TableEntity a)
    -> Pg Word64
  nextUnseenIndex table = do
    m <-
      runSelectReturningOne $
      select $ aggregate_ max_ $ indexColumn @a <$> all_ table
    return $
      case m of
        Nothing -> 0
        Just Nothing -> 0
        Just (Just num) -> num + 1
