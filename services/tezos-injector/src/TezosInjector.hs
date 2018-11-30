module TezosInjector
  ( module TezosInjector
  ) where

import qualified AccessControl.Auth
import qualified AccessControl.Client
import qualified Postgres as Pg
import qualified Tezos
import qualified Tezos.Rpc
import TezosInjector.Api as TezosInjector
import TezosInjector.Db
import TezosInjector.Internal as TezosInjector
import Vest

-- TODO: Replace with an actual forge service.
mockForge ::
     BatchPayout -> Word64 -> IO (Tezos.SignedOperation, Tezos.OperationObject)
mockForge = panic "unimplemented"

-- TODO: Also depends on the forge service.
mockAddress :: IO Tezos.Address
mockAddress = panic "unimplemented"

inject :: T -> (Tezos.SignedOperation, Tezos.OperationObject) -> IO ()
inject T {dbPool} (signedBytes, object) = do
  createdAt <- now
  Pg.runLoggedTransaction dbPool $
    insertOperationTx createdAt signedBytes object []

payout :: T -> AccessControl.Auth.Claims -> BatchPayout -> IO ()
payout T {dbPool} _ batch = do
  complete <-
    Pg.runLoggedTransaction dbPool $
    withVestCounterTx $ \counter -> do
      (signedBytes, object) <- liftIO $ mockForge batch counter
      -- ^ Forge while we hold the lock on the counter.
      createdAt <- liftIO now
      insertOperationTx createdAt signedBytes object $
        (\TezosInjector.Payout {id} -> id) <$> TezosInjector.payouts batch
      -- ^ Insert the forged transaction and payout references to it.
      let updatedAt = createdAt
          newCounter =
            counter + fromIntegral (length (TezosInjector.payouts batch))
      -- Update the counter and release the lock.
      return (updatedAt, newCounter)
  unless complete $ throw BugException

instance Service T where
  type RpcSpec T = InjectEndpoint
                   :<|> PayoutEndpoint
  type ValueSpec T = ()
  type EventsProduced T = ()
  type EventsConsumed T = ()
  summary = "tezos-injector v0.1.0"
  description = "Injects Tezos operations and makes sure they go through."
  init configPaths f = do
    (accessControlPublicKey :<|> seed :<|> tezosCli) <- load configPaths
    withLoadable configPaths $ \(dbPool :<|> amqp :<|> webSocket :<|> redis :<|> tezosRpc) -> do
      accessControlClient <-
        AccessControl.Client.make amqp accessControlPublicKey seed
      Pg.runLogged dbPool $ Pg.ensureSchema checkedSchema
      createdAt <- now
      counter <- Tezos.Rpc.getCounter tezosRpc =<< mockAddress
      Pg.runLogged dbPool $ tryInsertVestCounter createdAt counter
      -- TODO: Spawn locked processor thread for retrying.
      f $
        T
          { dbPool
          , amqp
          , webSocket
          , redis
          , tezosRpc
          , tezosCli
          , accessControlClient
          }
  rpcHandlers t = inject t :<|> payout t
  valuesPublished _ = ()
  eventProducers _ = ()
  eventConsumers _ = ()

type PublicApi = InjectEndpoint
