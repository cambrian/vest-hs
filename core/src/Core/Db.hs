module Core.Db
  ( module Reexports
  , Config (..)
  , Schema
  , tryCreateTables
  , tryInsertUser
  , userBalance
  , insertStakingContract
  , insertCollection
  , insertStakingReward
  ) where

import VestPrelude hiding (from)
import VestPrelude.Db as Reexports

-- The db user == currency. This is because squeal does not provide a way to specify the
-- (postgres) schema per query, so we rely on the fact that a user's schema search path is
-- ["#user", public].
-- Number of stripes is left at 1.
data Config (currency :: Symbol) = Config
  { host :: Host
  , port :: Port
  , database :: Id
  , currency :: Proxy currency
  , password :: Id
  , idleTime :: Time Second
  , connectionPoolSize :: Int
  }

connectionString :: (KnownSymbol a) => Config a -> Text
connectionString Config {host, port, database, currency, password} =
  let Host h = host
      Port p = port
      Id db = database
      user = proxyText currency
      Id pwd = password
   in "host="<>h<>" port="<>show p<>" dbname="<>db<>" user="<>user<>" password="<>pwd

-- TODO: God awful formatting in this file, probably thanks to hindent's inability to recognize
-- type-level operators. Disable formatting somehow and just do it manually if possible.
--
-- Each of these tables are per-currency. We have one postgres schema per currency, as defined in
-- Stakeable
--
-- We are overloading the prefix 'try' to indicate that a manipulation does nothing if some
-- condition is not met.


-- | Schema uses NO default values, such that no information flows from db -> server during inserts
type Schema =
  '[ "users" ::: 'Table (
        '[ "pk_users" ::: 'PrimaryKey '[ "hash"]] :=>
        '[ "hash" ::: 'NoDef :=> 'NotNull 'PGbytea
         , "balance" ::: 'NoDef :=> 'NotNull 'PGnumeric
        ])
    , "staking_contracts" ::: 'Table (
        '[ "pk_staking_contracts" ::: 'PrimaryKey '["id"]
         , "fk_owner" ::: 'ForeignKey '["owner"] "users" '["hash"]
         ] :=>
        '[ "id" ::: 'NoDef :=> 'NotNull 'PGbytea
         , "owner" ::: 'NoDef :=> 'NotNull 'PGbytea
         , "size" ::: 'NoDef :=> 'NotNull 'PGnumeric
         , "start_time" ::: 'NoDef :=> 'NotNull 'PGtimestamp
         , "duration" ::: 'NoDef :=> 'NotNull 'PGinterval
         , "price" ::: 'NoDef :=> 'NotNull 'PGnumeric
         ])
    , "collections" ::: 'Table (
        '[ "pk_collections" ::: 'PrimaryKey '["tx_hash"]
         , "fk_owner" ::: 'ForeignKey '["owner"] "users" '["hash"]
         ] :=>
        '[ "tx_hash" ::: 'NoDef :=> 'NotNull 'PGbytea
         , "owner" ::: 'NoDef :=> 'NotNull 'PGbytea
         , "recipient" ::: 'NoDef :=> 'NotNull 'PGbytea
         , "size" ::: 'NoDef :=> 'NotNull 'PGnumeric
         , "time" ::: 'NoDef :=> 'NotNull 'PGtimestamp
         ])
    , "staking_rewards" ::: 'Table (
        '[ "pk_staking_rewards" ::: 'PrimaryKey '["id"]
        , "fk_staking_contract" ::: 'ForeignKey '["staking_contract"] "staking_contracts" '["id"]
         ] :=>
        '[ "id" ::: 'NoDef :=> 'NotNull 'PGbytea
         , "tx_hash" ::: 'NoDef :=> 'NotNull 'PGbytea
         , "staking_contract" ::: 'NoDef :=> 'NotNull 'PGbytea
         , "size" ::: 'NoDef :=> 'NotNull 'PGnumeric
         , "time" ::: 'NoDef :=> 'NotNull 'PGtimestamp
         ])
    ]

tryCreateTables :: Definition Schema Schema
tryCreateTables =
  createTableIfNotExists
    #users
    ((bytea & notNullable) `as` #hash :*
    (numeric & notNullable) `as` #balance)
    (primaryKey #hash `as` #pk_users) >>>
  createTableIfNotExists
    #staking_contracts
    ((bytea & notNullable) `as` #id :*
     (bytea & notNullable) `as` #owner :*
     (numeric & notNullable) `as`
     #size :*
     (timestamp & notNullable) `as`
     #start_time :*
     (interval & notNullable) `as`
     #duration :*
     (numeric & notNullable) `as`
     #price)
    (primaryKey #id `as` #pk_staking_contracts :*
     foreignKey #owner #users #hash OnDeleteCascade OnUpdateRestrict `as`
     #fk_owner) >>>
  createTableIfNotExists
    #collections
    ((bytea & notNullable) `as` #tx_hash :* (bytea & notNullable) `as` #owner :*
     (bytea & notNullable) `as`
     #recipient :*
     (numeric & notNullable) `as`
     #size :*
     (timestamp & notNullable) `as`
     #time)
    (primaryKey #tx_hash `as` #pk_collections :*
     foreignKey #owner #users #hash OnDeleteCascade OnUpdateRestrict `as`
     #fk_owner) >>>
  createTableIfNotExists
    #staking_rewards
    ((bytea & notNullable) `as` #id :* (bytea & notNullable) `as` #tx_hash :*
     (bytea & notNullable) `as`
     #staking_contract :*
     (numeric & notNullable) `as`
     #size :*
     (timestamp & notNullable) `as`
     #time)
    (primaryKey #id `as` #pk_staking_rewards :*
     foreignKey
       #staking_contract
       #staking_contracts
       #id
       OnDeleteCascade
       OnUpdateRestrict `as`
     #fk_staking_contract)

tryInsertUser ::
     Manipulation Schema '[ 'NotNull 'PGbytea, 'NotNull 'PGnumeric] '[]
tryInsertUser =
  insertRow
    #users
    (Set (param @1) `as` #hash :* Set (param @2) `as` #balance)
    OnConflictDoNothing
    (Returning Nil)

userBalance :: Query Schema '[ 'NotNull 'PGbytea] '[ "balance" ::: 'NotNull 'PGnumeric]
userBalance =
  select #balance $ from (table #users) & where_ (#hash .== param @1)

insertStakingContract ::
     Manipulation Schema '[ 'NotNull 'PGbytea, 'NotNull 'PGbytea, 'NotNull 'PGnumeric, 'NotNull 'PGtimestamp, 'NotNull 'PGinterval, 'NotNull 'PGnumeric] '[]
insertStakingContract =
  insertRow_
    #staking_contracts
    (Set (param @1) `as` #id :* Set (param @2) `as` #owner :* Set (param @3) `as`
     #size :*
     Set (param @4) `as`
     #start_time :*
     Set (param @5) `as`
     #duration :*
     Set (param @6) `as`
     #price)

insertCollection ::
     Manipulation Schema '[ 'NotNull 'PGbytea, 'NotNull 'PGbytea, 'NotNull 'PGbytea, 'NotNull 'PGnumeric, 'NotNull 'PGtimestamp] '[]
insertCollection =
  insertRow_
    #collections
    (Set (param @1) `as` #tx_hash :* Set (param @2) `as` #owner :*
     Set (param @3) `as`
     #recipient :*
     Set (param @4) `as`
     #size :*
     Set (param @5) `as`
     #time)

insertStakingReward ::
     Manipulation Schema '[ 'NotNull 'PGbytea, 'NotNull 'PGbytea, 'NotNull 'PGbytea, 'NotNull 'PGnumeric, 'NotNull 'PGtimestamp] '[]
insertStakingReward =
  insertRow_
    #staking_rewards
    (Set (param @1) `as` #id :* Set (param @2) `as` #tx_hash :* Set (param @3) `as`
     #staking_contract :*
     Set (param @4) `as`
     #size :*
     Set (param @5) `as`
     #time)
