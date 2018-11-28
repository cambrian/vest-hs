module TezosOperationQueue
  ( module TezosOperationQueue
  ) where

import qualified AccessControl.Client
import qualified Tezos
import TezosOperationQueue.Api as TezosOperationQueue
import TezosOperationQueue.Internal as TezosOperationQueue
import Vest

type Api = InjectEndpoint

-- TODO: Un-stub; add Vest inject.
inject :: T -> Tezos.SignedOperation -> IO ()
inject _ _ = return ()

instance Service T where
  type RpcSpec T = Api
  type ValueSpec T = ()
  type EventsProduced T = ()
  type EventsConsumed T = ()
  summary = "tezos-operation-queue v0.1.0"
  description = "RPC queue for Tezos operations signed on the front-end."
  init configPaths f = do
    (accessControlPublicKey :<|> seed) <- load configPaths
    withLoadable configPaths $ \(dbPool :<|> amqp :<|> redis :<|> webSocket) -> do
      accessControlClient <-
        AccessControl.Client.make amqp accessControlPublicKey seed
      f $ T {dbPool, amqp, redis, webSocket, accessControlClient}
  rpcHandlers = inject
  valuesPublished _ = ()
  eventProducers _ = ()
  eventConsumers _ = ()
