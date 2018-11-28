module TezosHotWallet
  ( module TezosHotWallet
  ) where

import qualified AccessControl.Auth
import qualified AccessControl.Client
import qualified Postgres as Pg
import qualified Tezos
import qualified Tezos.Cli
import TezosChainWatcher.Api as TezosChainWatcher
import TezosHotWallet.Api as TezosHotWallet
import TezosHotWallet.Db
import TezosHotWallet.Internal as TezosHotWallet
import TezosOperationQueue.Api as TezosOperationQueue
import Vest

blockConsumer :: T -> Consumers FinalizedBlockEvents
blockConsumer T {dbPool, paymentWriter, vestAddress} =
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

payoutHandler :: T -> AccessControl.Auth.Claims -> PayoutRequest -> IO ()
payoutHandler t@T {dbPool, tezosCli} AccessControl.Auth.Claims {name} req@PayoutRequest { id
                                                                                        , to
                                                                                        , size
                                                                                        , fee
                                                                                        } = do
  log Debug "payout requested" (name, req)
  signedTransaction <- Tezos.Cli.forgeTransaction tezosCli to (size - fee) fee
  let inject =
        makeClient t (Proxy :: Proxy TezosOperationQueue.InjectVestEndpoint)
  hash <- inject signedTransaction
  time <- now
  Pg.runLogged dbPool $
    Pg.runInsert $
    Pg.insert
      (payouts schema)
      (Pg.insertValues [Payout id hash to size time])
      Pg.onConflictDefault

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
  type EventsConsumed T = TezosChainWatcher.FinalizedBlockEvents
  type RpcSpec T = PayoutEndpoint
  summary = "Tezos Hot Wallet v0.1.0"
  description = "The Vest hot wallet manager for Tezos."
  init configPaths f = do
    (accessControlPublicKey :<|> seed :<|> tezosCli) <- load configPaths
    (paymentWriter, paymentStream) <- newStream
    withLoadable configPaths $ \(dbPool :<|> amqp :<|> redis) -> do
      accessControlClient <-
        AccessControl.Client.make amqp accessControlPublicKey seed
      Pg.runLogged dbPool $ Pg.ensureSchema checkedSchema
      vestAddress <- Tezos.Cli.extractAddress tezosCli
      f $
        T
          { dbPool
          , amqp
          , redis
          , tezosCli
          , accessControlClient
          , paymentWriter
          , paymentStream
          , vestAddress
          }
  rpcHandlers = payoutHandler
  valuesPublished _ = ()
  eventProducers t = (return $ paymentStream t, paymentMaterializer t)
  eventConsumers = blockConsumer
