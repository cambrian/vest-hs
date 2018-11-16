-- Currently intended as an executable for mocked testing data.
module TezosOperationQueue
  ( module TezosOperationQueue
  ) where

-- import qualified Db
import qualified Tezos
import TezosOperationQueue.Api as TezosOperationQueue
import TezosOperationQueue.Internal as TezosOperationQueue
import Vest

type Api = InjectEndpoint

-- TODO: Un-stub.
inject :: T -> Tezos.SignedOperationContents -> IO ()
inject _ _ = return ()

instance Service T where
  type ValueSpec T = ()
  type EventsProduced T = ()
  type EventsConsumed T = ()
  type RpcSpec T = Api
  summary = "tezos-operation-queue v0.1.0"
  description = "RPC queue for Tezos operations signed on the front-end."
  init configPaths f =
    withLoadable configPaths $ \webSocket -> f $ T {webSocket}
  rpcHandlers = inject
  valuesPublished _ = ()
  eventProducers _ = ()
  eventConsumers _ = ()
