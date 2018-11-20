module TezosHotWallet
  ( module TezosHotWallet
  ) where

import qualified AccessControl.Client
import qualified Postgres as Pg
import TezosChainWatcher.Api as TezosChainWatcher
import TezosHotWallet.Api as TezosHotWallet
import TezosHotWallet.Db
import TezosHotWallet.Internal as TezosHotWallet
import Vest

-- blockConsumer :: Consumers BlockEvents
-- blockConsumer =
--   let ixMaterializer = Pg.selectFirstMissing $ blocks schema
--       f BlockEvent {number, transactions}
instance Service T where
  type ValueSpec T = ()
  type EventsProduced T = PaymentEvents
  type EventsConsumed T = TezosChainWatcher.BlockEvents
  type RpcSpec T = PayoutEndpoint
  summary = "Tezos Hot Wallet v0.1.0"
  description = "The Vest hot wallet manager for Tezos."
  init configPaths f = do
    (accessControlPublicKey :<|> seed :<|> tezosCli) <- load configPaths
    withLoadable configPaths $ \(dbPool :<|> amqp :<|> redis) -> do
      accessControlClient <-
        AccessControl.Client.make amqp accessControlPublicKey seed
      Pg.runLogged dbPool $ Pg.ensureSchema checkedSchema
      f $ T {dbPool, amqp, redis, tezosCli, accessControlClient}
  rpcHandlers _ = panic "unimplemented"
  valuesPublished _ = ()
  eventProducers _ = panic "unimplemented"
  eventConsumers _ = panic "unimplemented"
