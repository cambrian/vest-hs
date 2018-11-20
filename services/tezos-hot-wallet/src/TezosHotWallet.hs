module TezosHotWallet
  ( module TezosHotWallet
  ) where

import qualified AccessControl.Client
import TezosChainWatcher.Api as TezosChainWatcher
import TezosHotWallet.Api as TezosHotWallet
import TezosHotWallet.Internal as TezosHotWallet
import Vest

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
      f $ T {dbPool, amqp, redis, tezosCli, accessControlClient}
  rpcHandlers _ = panic "unimplemented"
  valuesPublished _ = ()
  eventProducers _ = panic "unimplemented"
  eventConsumers _ = panic "unimplemented"
