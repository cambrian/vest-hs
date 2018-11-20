module TezosHotWallet
  ( module TezosHotWallet
  ) where

import qualified AccessControl.Client
import qualified Postgres as Pg
import qualified Tezos
import TezosChainWatcher.Api as TezosChainWatcher
import TezosHotWallet.Api as TezosHotWallet
import TezosHotWallet.Db
import TezosHotWallet.Internal as TezosHotWallet
import Vest

vestAddress :: Tezos.Address
vestAddress = panic "change this"

blockConsumer :: T -> Consumers BlockEvents
blockConsumer t@T {paymentWriter} =
  let nextUnseen = Pg.runLogged t $ Pg.nextUnseenIndex $ blocks schema
      f Tezos.BlockEvent {number = blockNum, transactions} = do
        Pg.runLogged t (Pg.wasHandled (blocks schema) blockNum) >>=
          (`when` return ())
        time <- now
        forM_ transactions $ \Tezos.Transaction {hash, from, to, size} -> do
          unless (to == vestAddress) $ return ()
          Pg.runLoggedTransaction t $ do
            idx <- Pg.nextUnseenIndex $ payments schema
            Pg.runInsert $
              Pg.insert
                (payments schema)
                (Pg.insertValues [Payment idx hash from size time time])
                Pg.onConflictDefault
            liftIO $
              writeStream paymentWriter $ PaymentEvent idx hash from size time
        Pg.runLogged t $
          Pg.runInsert $
          Pg.insert
            (blocks schema)
            (Pg.insertValues [Block blockNum time])
            Pg.onConflictDefault
   in (nextUnseen, f)

payoutHandler :: T -> PayoutRequest -> IO ()
payoutHandler t PayoutRequest {id, to, size} = do
  time <- now
  Pg.runLogged t $
    Pg.runInsert $
    Pg.insert
      (payouts schema)
      (Pg.insertValues [Payout id to size Nothing time])
      Pg.onConflictDefault

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
    paymentPendingVar <- newEmptyTMVarIO
    withLoadable configPaths $ \(dbPool :<|> amqp :<|> redis) -> do
      accessControlClient <-
        AccessControl.Client.make amqp accessControlPublicKey seed
      Pg.runLogged dbPool $ Pg.ensureSchema checkedSchema
      void . async . forever $ do atomically $ takeTMVar paymentPendingVar
      f $
        T
          { dbPool
          , amqp
          , redis
          , tezosCli
          , accessControlClient
          , paymentWriter
          , paymentStream
          , payoutPendingVar
          }
  rpcHandlers _ = panic "unimplemented"
  valuesPublished _ = ()
  eventProducers _ = panic "unimplemented"
  eventConsumers _ = panic "unimplemented"
