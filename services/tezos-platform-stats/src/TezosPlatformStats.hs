-- Currently intended as an executable for mocked testing data.
module TezosPlatformStats
  ( module TezosPlatformStats
  ) where

import qualified Postgres as Pg
import qualified Tezos
import qualified TezosChainWatcher.Db
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
