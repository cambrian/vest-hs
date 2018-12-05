module TezosInjector
  ( module TezosInjector
  ) where

import qualified AccessControl.Auth
import qualified AccessControl.Client
import qualified Amqp
import qualified Postgres as Pg
import qualified TMap
import qualified Tezos
import qualified Tezos.Cli
import qualified Tezos.Rpc
import qualified TezosChainWatcher.Api as TezosChainWatcher
import TezosChainWatcher.Api (withMonitor)
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

inject :: T -> [Injection] -> IO ()
inject T {dbPool} ops = do
  createdAt <- now
  Pg.runLoggedTransaction dbPool $
    mapM_
      (\Injection {counterAddress, signedOpBytes, opObject} ->
         insertOperationTx
           createdAt
           counterAddress
           signedOpBytes
           opObject
           False
           [])
      ops

payout :: T -> AccessControl.Auth.Claims -> BatchPayout -> IO ()
payout T {dbPool} _ batch = do
  complete <-
    Pg.runLoggedTransaction dbPool $
    withVestCounterTx $ \counter -> do
      (signedBytes, object) <- liftIO $ mockForge batch counter
      -- ^ Forge while we hold the lock on the counter.
      createdAt <- liftIO now
      vestAddress <- liftIO mockAddress
      insertOperationTx createdAt vestAddress signedBytes object True $
        (\TezosInjector.Payout {id} -> id) <$> TezosInjector.payouts batch
      -- ^ Insert the forged transaction and payout references to it.
      let updatedAt = createdAt
          newCounter =
            counter + fromIntegral (length (TezosInjector.payouts batch))
      -- Update the counter and release the lock.
      return (updatedAt, newCounter)
  unless complete $ throw BugException

processLocked ::
     RedisConnection
  -> Amqp.T
  -> Pool (Specific T Pg.Connection)
  -> Tezos.Cli.T
  -> IO ()
processLocked redis amqp dbPool tezosCli = do
  let lockId = Tagged $ namespace @T <> "/processor"
  -- Manually lock so processing is limited to one injector replica.
  void $
    async $
    withDistributedLock redis lockId $ do
      eventHashStream <-
        subscribe
          amqp
          (Proxy :: Proxy TezosChainWatcher.ProvisionalBlockHashValue)
      monitorThreads <- TMap.newIO
      consumeStream
        (\_ -> do
           newNextOperations <-
             Pg.runLoggedTransaction dbPool $ do
               nextOperations <- selectNextOperations
               filterM
                 (\Operation {id} ->
                    liftIO $
                    isNothing <$> atomically (TMap.lookup id monitorThreads))
                 nextOperations
            -- ^ Every block time, query the DB for all operations that are still pending and
            -- collect the first added operation for each account. For each operation, inject and
            -- start monitoring it (if a monitor thread is not already running).
           mapConcurrently_
             (\Operation {id, signed_bytes, object, is_internal} -> do
                monitorThread <-
                  async $ do
                    opHashMaybe <-
                      Tezos.Cli.injectOperation tezosCli signed_bytes object
                    let reject =
                          if is_internal
                            then throw BugException
                            else do
                              updatedAt <- now
                              complete <-
                                Pg.runLoggedTransaction dbPool $
                                rejectOperationTx updatedAt id
                              unless complete $ throw BugException
                              log Debug "rejected public op" ()
                    let confirm opHash = do
                          updatedAt <- now
                          complete <-
                            Pg.runLoggedTransaction dbPool $
                            confirmOperationTx updatedAt id opHash
                          unless complete $ throw BugException
                    -- ^ Panic on rejected/invalid Vest ops and mark bad public ops as rejected
                    -- in the database.
                    case opHashMaybe of
                      Nothing -> reject
                      Just opHash ->
                        withMonitor
                          amqp
                          opHash
                          (\case
                             Tezos.Confirmed _ -> confirm opHash
                             Tezos.Rejected -> reject
                             _ -> return ())
                -- Outer thread waits for the monitor thread to finish and then deletes it
                -- from the TMap. This way, we avoid a (not dangerous, but still undesirable)
                -- race condition where the monitor thread tries to delete itself from the TMap
                -- before it is added.
                atomically $ TMap.insert monitorThread id monitorThreads
                wait monitorThread >> atomically (TMap.delete id monitorThreads))
             newNextOperations)
        eventHashStream

instance Service T where
  type RpcSpec T = InjectEndpoint
                   :<|> PayoutEndpoint
  type ValueSpec T = ()
  type EventsProduced T = ()
  type EventsConsumed T = ()
  summary = "Tezos Injector v0.1.0"
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
      processLocked redis amqp dbPool tezosCli
      -- ^ Locked async processor thread.
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
