-- Currently intended as an executable for mocked testing data.
module TezosPlatformStats
  ( module TezosPlatformStats
  ) where

import qualified Postgres as Pg
import qualified Tezos
import qualified TezosChainWatcher.Db
import qualified TezosPlatformCore.Db
import TezosPlatformStats.Api as TezosPlatformStats
import TezosPlatformStats.Internal as TezosPlatformStats
import Vest

originationsByImplicitAddress ::
     T -> Tezos.ImplicitAddress -> IO [Tezos.OriginatedAddress]
originationsByImplicitAddress T {chainWatcherDb} addr = do
  Pg.runLogged chainWatcherDb $
    Pg.runSelectReturningList $
    Pg.select $
    TezosChainWatcher.Db.originated <$>
    Pg.filter_
      (\TezosChainWatcher.Db.Origination {originator} ->
         originator Pg.==. Pg.val_ addr)
      (Pg.all_ $ (TezosChainWatcher.Db.originations TezosChainWatcher.Db.schema))
    -- SELECT originated from originations where originator == addr

bakerCount :: T -> () -> IO Int
bakerCount T {coreDb} () = do
  x <-
    Pg.runLogged coreDb $
    Pg.runSelectReturningOne $
    Pg.select $
    Pg.aggregate_
      (const Pg.countAll_)
      (Pg.all_ (TezosPlatformCore.Db.delegates TezosPlatformCore.Db.schema))
  fromJustUnsafe BugException x
  -- SELECT COUNT(*) FROM delegates

totalRewards :: T -> () -> IO (FixedQty XTZ)
totalRewards T {coreDb} () = do
  x <-
    Pg.runLogged coreDb $
    Pg.runSelectReturningOne $
    Pg.select $
    Pg.aggregate_
      (\TezosPlatformCore.Db.Reward {size} -> Pg.sum_ size)
      (Pg.all_ (TezosPlatformCore.Db.rewards TezosPlatformCore.Db.schema))
  fromMaybe 0 <$> fromJustUnsafe BugException x
  -- SELECT SUM(size) FROM rewards
  -- Pg.select $
  -- TezosPlatformCore.Db.rewards $
  -- (\TezosPlatformCore.Db.Reward {cycle, staking_balance, delegated_balance} ->
  --    (cycle, (staking_balance - delegate_balance)))
  --   (Pg.all_ (TezosPlatformCore.Db.rewards TezosPlatformCore.Db.schema))

-- bondsOverTime :: T -> () -> IO [TimeBond]
-- bondsOverTime T {coreDb} () = do
--   Pg.runLogged coreDb $
--     Pg.runSelectReturningList $
--     Pg.select $
--     records <- Pg.all_ (TezosPlatformCore.Db.rewards TezosPlatformCore.Db.schema)
--     pure (CycleT cycles, FixedQty XTZ staking_balance, FixedQty XTZ delegated_balance)
-- SELECT cycle, SUM (staking_balance - delegated_balance) FROM rewards GROUPBY cycle
-- delegateDistribution :: T -> () -> IO ([])
instance Service T where
  type RpcSpec T = OriginationsByImplicitAddressEndpoint
  type ValueSpec T = ()
  type EventsProduced T = ()
  type EventsConsumed T = ()
  summary = "Tezos Platform Stats v0.1.0"
  description = "Front-end stats server for Tezos."
  init configPaths f = do
    withLoadable configPaths $ \(webSocket :<|> amqp :<|> coreDb :<|> chainWatcherDb :<|> redis) ->
      f $ T {webSocket, amqp, coreDb, chainWatcherDb, redis}
  rpcHandlers t = originationsByImplicitAddress t
  valuesPublished _ = ()
  eventProducers _ = ()
  eventConsumers _ = ()

type PublicApi = ()

type AuxiliaryTypes = ()
