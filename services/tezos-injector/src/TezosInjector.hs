module TezosInjector
  ( module TezosInjector
  ) where

import qualified AccessControl.Auth
import qualified AccessControl.Client
import qualified Tezos
import TezosInjector.Api as TezosInjector
import TezosInjector.Internal as TezosInjector
import Vest

-- TODO: Un-stub.
inject :: T -> Tezos.SignedOperation -> IO ()
inject _ _ = panic "unimplemented"

-- TODO: Un-stub.
payout ::
     T
  -> AccessControl.Auth.Claims
  -> HashMap Tezos.Address (FixedQty XTZ)
  -> IO ()
payout _ _ _ = panic "unimplemented"

instance Service T where
  type RpcSpec T = InjectEndpoint
                   :<|> PayoutEndpoint
  type ValueSpec T = ()
  type EventsProduced T = ()
  type EventsConsumed T = ()
  summary = "tezos-injector v0.1.0"
  description = "Injects Tezos operations and makes sure they go through."
  init configPaths f = do
    (accessControlPublicKey :<|> seed) <- load configPaths
    withLoadable configPaths $ \(dbPool :<|> amqp :<|> redis :<|> webSocket) -> do
      accessControlClient <-
        AccessControl.Client.make amqp accessControlPublicKey seed
      f $ T {dbPool, amqp, redis, webSocket, accessControlClient}
  rpcHandlers t = inject t :<|> payout t
  valuesPublished _ = ()
  eventProducers _ = ()
  eventConsumers _ = ()

type PublicApi = InjectEndpoint
