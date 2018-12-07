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

-- bondsOverTime :: T -> () -> IO [TimeBond]
-- bondsOverTime T {coreDb} ()
--   --table <- Pg.all_ (TezosPlatformCore.Db.rewards TezosPlatformCore.Db.schema)
--  = do
--   records <-
--     Pg.runLogged coreDb $
--     Pg.select $ Pg.all_ (delegates TezosPlatformCore.Db.Schema)
--     -- pure (cycles table, staking_balance table, delegated_balance table)
--     --pure (CycleT cycles, FixedQty XTZ staking_balance, FixedQty XTZ delegated_balance)
--   pure records
-- SELECT cycle, SUM (staking_balance - delegated_balance) FROM rewards GROUPBY cycle
-- delegateBonds :: T -> () -> IO [DelegateBond]
-- delegateBonds T {coreDb} () = do
--   x <-
--     Pg.runLogged coreDb $
--     Pg.runSelectReturningList $
--     Pg.select $
--     Pg.all_ (TezosPlatformCore.Db.delegates TezosPlatformCore.Db.schema)
instance Service T where
  type RpcSpec T = OriginationsByImplicitAddressEndpoint
                   :<|> BakerCountEndpoint
  type ValueSpec T = ()
  type EventsProduced T = ()
  type EventsConsumed T = ()
  summary = "Tezos Platform Stats v0.1.0"
  description = "Front-end stats server for Tezos."
  init configPaths f = do
    withLoadable configPaths $ \(webSocket :<|> amqp :<|> coreDb :<|> chainWatcherDb :<|> redis) -> do
      let t = T {webSocket, amqp, coreDb, chainWatcherDb, redis}
      originations <-
        originationsByImplicitAddress t "tz1Wit2PqodvPeuRRhdQXmkrtU8e8bRYZecd"
      -- numBakers <- bakerCount t ()
      -- cashMoney <- totalRewards t ()
      log Debug "originations by implicit address" originations
      -- log Debug "number of bakers" numBakers
      -- log Debug "total rewards" cashMoney
      f t
       -- init configPaths f = do
    -- withLoadable configPaths $ \(webSocket :<|> amqp :<|> coreDb :<|> chainWatcherDb :<|> redis) ->
    --   let s = T {webSocket, amqp, coreDb, chainWatcherDb, redis} in
    --       do
    --         originations <- originationsByImplicitAddress s "0xdummyaddr"
    --   f $ T {webSocket, amqp, coreDb, chainWatcherDb, redis}
  rpcHandlers t = originationsByImplicitAddress t :<|> bakerCount t
  valuesPublished _ = ()
  eventProducers _ = ()
  eventConsumers _ = ()

type PublicApi = ()

type AuxiliaryTypes = ()
