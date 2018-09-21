module VestPrelude.Db
  ( module VestPrelude.Db
  , module Reexports
  ) where

import Data.Time.Clock (UTCTime)
import Data.Time.Clock.System (SystemTime(..), systemToUTCTime, utcToSystemTime)
import Data.Time.Format (defaultTimeLocale, formatTime, parseTimeM)
import qualified Money
import Database.Beam as Reexports hiding (insert)
import qualified Database.Beam.Postgres as Reexports
import Database.Beam.Backend.SQL
import VestPrelude
-- -- Provide a bunch of instances to let types be used in SqlRow and SqlType
-- -- instance Bounded (Time rat)
-- -- instance (Typeable rat, KnownUnitName rat) => SqlType (Time rat)
-- -- instance (Typeable rat, KnownUnitName rat) => SqlOrd (Time rat)
-- instance Bounded Timestamp
-- instance Generic Timestamp
-- instance Enum Timestamp
-- In theory you could implement this directly on Timestamp without having to create a UTCTime
-- but that's a bunch of work for what's only a smallish win.
-- Rounds to the nearest second. TODO: get remainder as nanos
instance HasSqlValueSyntax be Text => HasSqlValueSyntax be (Id a) where
  sqlValueSyntax (Id text) = sqlValueSyntax text
-- instance SqlType Timestamp where
--   mkLit x =
--     let Timestamp seconds = x
--         utcTime =
--           systemToUTCTime $
--           MkSystemTime {systemSeconds = truncate seconds, systemNanoseconds = 0}
--         lit =
--           LCustom .
--           LDateTime . pack . formatTime defaultTimeLocale sqlDateTimeFormat
--      in lit utcTime
--   sqlType _ = TDateTime
--   fromSql (SqlString s) =
--     let utcTime =
--           case parseTimeM True defaultTimeLocale sqlDateTimeFormat (unpack s) of
--             Just t -> t
--             _ -> panic $ "fromSql: bad datetime string: " <> s
--         MkSystemTime {systemSeconds} = utcToSystemTime utcTime
--      in fromUnixTime systemSeconds
--   fromSql v =
--     panic $ "fromSql: datetime column with non-datetime value: " <> show v
--   defaultValue = LCustom $ LDateTime "1970-01-01 00:00:00"
-- instance SqlOrd Timestamp
-- instance Bounded (Money.Dense a)
-- instance Enum (Money.Dense a)
-- instance (KnownSymbol a) => SqlType (Money.Dense a)
-- instance (KnownSymbol a) => SqlOrd (Money.Dense a)
-- instance Money.GoodScale b => Bounded (Money.Discrete' a b)
-- instance (KnownSymbol a, Money.GoodScale b, Typeable b) =>
--          SqlType (Money.Discrete' a b)
-- instance (KnownSymbol a, Money.GoodScale b, Typeable b) =>
--          SqlOrd (Money.Discrete' a b)
