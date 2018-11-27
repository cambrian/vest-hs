module TezosOperationQueue
  ( module TezosOperationQueue
  ) where

import qualified AccessControl.Client
import qualified Postgres as Pg
import qualified Tezos
import TezosChainWatcher.Api (BlockEvents)
import TezosOperationQueue.Api as TezosOperationQueue
import TezosOperationQueue.Db
import TezosOperationQueue.Internal as TezosOperationQueue
import Vest

type Api = InjectEndpoint

-- TODO: Un-stub.
inject :: T -> Tezos.SignedOperation -> IO ()
inject _ _ = return ()

blockEventConsumer :: T -> Consumers BlockEvents
blockEventConsumer T {dbPool} =
  let getInitialBlockNumber = Pg.runLogged dbPool selectNextBlockNumber
   in (getInitialBlockNumber, panic "unimplemented")

instance Service T where
  type ValueSpec T = ()
  type EventsProduced T = ()
  type EventsConsumed T = BlockEvents
  type RpcSpec T = Api
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
  eventConsumers = blockEventConsumer
