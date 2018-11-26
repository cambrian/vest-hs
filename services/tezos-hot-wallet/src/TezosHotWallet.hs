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
  void . atomically $ tryPutTMVar (newPaymentVar t) ()

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
    newPaymentVar <- newEmptyTMVarIO
    withLoadable configPaths $ \(dbPool :<|> amqp :<|> redis) -> do
      accessControlClient <-
        AccessControl.Client.make amqp accessControlPublicKey seed
      Pg.runLogged dbPool $ Pg.ensureSchema checkedSchema
      _pendingPayouts <- Pg.runLogged dbPool $ selectPayoutsByStatus "Pending"
      -- TODO: watch pending payouts
      -- Payout handler thread
      void . async . forever $ do
        atomically $ takeTMVar newPaymentVar
        newPayouts <- Pg.runLogged dbPool $ selectPayoutsByStatus "NotIssued"
        forM_ newPayouts $ \Payout {to, size} -> do
          let fee = 0 -- TODO:
          let tryPayoutUntilSuccess = do
                m <- Tezos.Cli.makePayout tezosCli to size fee
                case m of
                  Nothing -> tryPayoutUntilSuccess
                  Just hash -> return hash
          _hash <- tryPayoutUntilSuccess
          -- TODO: set state and watch
          return ()
      f $
        T
          { dbPool
          , amqp
          , redis
          , tezosCli
          , accessControlClient
          , paymentWriter
          , paymentStream
          , newPaymentVar
          }
  rpcHandlers _ = panic "unimplemented"
  valuesPublished _ = ()
  eventProducers _ = panic "unimplemented"
  eventConsumers _ = panic "unimplemented"
