module TezosHotWallet
  ( module TezosHotWallet
  ) where

import qualified AccessControl.Client
import TezosHotWallet.Api as TezosHotWallet
import TezosHotWallet.Internal as TezosHotWallet
import Vest

type Api = ()

handlers :: Handlers Api
handlers = ()

instance Service T where
  type ValueSpec T = ()
  type EventsProduced T = PaymentEvents
  type EventsConsumed T = ()
  type RpcSpec T = Api
  summary = "Tezos Hot Wallet v0.1.0"
  description = "Vest's Hot Wallet for Tezos"
  init configPaths f = do
    seed <- load configPaths
    accessControlPublicKey <- load configPaths
    withLoadable configPaths $ \(dbPool :<|> amqp :<|> redis :<|> tezos) -> do
      accessControlClient <-
        AccessControl.Client.make amqp accessControlPublicKey seed
      f $ T {dbPool, amqp, redis, tezos, accessControlClient}
  rpcHandlers _ = ()
  valuesPublished _ = ()
  eventProducers _ = panic "unimplemented"
  eventConsumers _ = ()
