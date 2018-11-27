module TezosHotWallet
  ( module TezosHotWallet
  ) where

import qualified AccessControl.Client
import qualified Postgres as Pg
import qualified Tezos
import qualified Tezos.Cli
import TezosChainWatcher.Api as TezosChainWatcher
import TezosHotWallet.Api as TezosHotWallet
import TezosHotWallet.Db
import TezosHotWallet.Internal as TezosHotWallet
import Vest

vestAddress :: Tezos.Address
vestAddress = panic "change this"

blockConsumer :: T -> Consumers BlockEvents
blockConsumer T {dbPool, paymentWriter} =
  let nextUnseen = Pg.runLogged dbPool $ Pg.nextUnseenIndex $ blocks schema
      f Tezos.BlockEvent {number = blockNum, transactions} = do
        Pg.runLogged dbPool (Pg.wasHandled (blocks schema) blockNum) >>=
          (`when` return ())
        time <- now
        forM_ transactions $ \Tezos.Transaction {hash, from, to, size} -> do
          unless (to == vestAddress) $ return ()
          Pg.runLoggedTransaction dbPool $ do
            idx <- Pg.nextUnseenIndex $ payments schema
            Pg.runInsert $
              Pg.insert
                (payments schema)
                (Pg.insertValues [Payment idx hash from size time time])
                Pg.onConflictDefault
            liftIO $
              writeStream paymentWriter $ PaymentEvent idx hash from size time
        Pg.runLogged dbPool $
          Pg.runInsert $
          Pg.insert
            (blocks schema)
            (Pg.insertValues [Block blockNum time])
            Pg.onConflictDefault
   in (nextUnseen, f)

payoutHandler :: T -> PayoutRequest -> IO ()
payoutHandler t@T {dbPool} PayoutRequest {id, to, size} = do
  time <- now
  Pg.runLogged dbPool $
    Pg.runInsert $
    Pg.insert
      (payouts schema)
      (Pg.insertValues [Payout id to size Nothing "NotIssued" time])
      Pg.onConflictDefault
  void . atomically $ tryPutTMVar (newPayoutVar t) ()

paymentMaterializer :: T -> Word64 -> IO PaymentEvent
paymentMaterializer T {dbPool} idx_ = do
  m <-
    Pg.runLogged dbPool $
    Pg.runSelectReturningOne $ Pg.lookup_ (payments schema) $ PaymentIdx idx_
  Payment {idx, hash, from, size, time} <- fromJustUnsafe BugException m
  return $ PaymentEvent {idx, hash, from, size, time}

instance Service T where
  type ValueSpec T = ()
  type EventsProduced T = PaymentEvents
  type EventsConsumed T = TezosChainWatcher.BlockEvents
  type RpcSpec T = PayoutEndpoint
  summary = "Tezos Hot Wallet v0.1.0"
  description = "The Vest hot wallet manager for Tezos."
  init configPaths f = do
    (accessControlPublicKey :<|> seed :<|> tezosCli) <- load configPaths
    (paymentWriter, paymentStream) <- newStream
    newPayoutVar <- newTMVarIO () -- init this mvar as full so we check payouts on service startup
    withLoadable configPaths $ \(dbPool :<|> amqp :<|> redis) -> do
      accessControlClient <-
        AccessControl.Client.make amqp accessControlPublicKey seed
      Pg.runLogged dbPool $ Pg.ensureSchema checkedSchema
      let monitorOp =
            makeClient
              amqp
              (Proxy :: Proxy TezosChainWatcher.MonitorOperationEndpoint)
      -- Payout handler thread
      -- we don't need to cancel this do we?
      -- TODO: what if multiple instances are running this?
      void . async . forever $ do
        atomically $ takeTMVar newPayoutVar
        newPayouts <- Pg.runLogged dbPool $ selectPayoutsByStatus "NotIssued"
        forM_ newPayouts $ \Payout {id = payoutId, to, size} -> do
          let fee = 0 -- TODO:
          hash_ <- Tezos.Cli.makePayout tezosCli to size fee
          -- Update transaction as pending, set tx hash
          Pg.runLogged dbPool $
            Pg.runUpdate $
            Pg.update
              (payouts schema)
              (\Payout {hash, status} ->
                 [ hash Pg.<-. Pg.val_ (Just hash_)
                 , status Pg.<-. Pg.val_ "Pending"
                 ])
              (\Payout {id} -> id Pg.==. Pg.val_ payoutId)
          -- watch pending transactions
          pendingPayouts <-
            Pg.runLogged dbPool $ selectPayoutsByStatus "Pending"
          forM_ pendingPayouts $ \Payout {id = payoutId, hash} ->
            case hash of
              Nothing -> throw BugException -- Log error and set to pending?
              Just h ->
                monitorOp h $
                tapStream_
                  (\case
                     Left exn -> throw exn
                     Right (Tezos.NotIncluded _) -> do
                       return ()
                     Right (Tezos.Included _) -> return ()
                     Right (Tezos.Confirmed _) ->
                       Pg.runLogged dbPool $
                       Pg.runUpdate $
                       Pg.update
                         (payouts schema)
                         (\Payout {status} ->
                            [status Pg.<-. Pg.val_ "Confirmed"])
                         (\Payout {id} -> id Pg.==. Pg.val_ payoutId))
      f $
        T
          { dbPool
          , amqp
          , redis
          , tezosCli
          , accessControlClient
          , paymentWriter
          , paymentStream
          , newPayoutVar
          }
  rpcHandlers _ = panic "unimplemented"
  valuesPublished _ = ()
  eventProducers t = (return $ paymentStream t, paymentMaterializer t)
  eventConsumers = blockConsumer
