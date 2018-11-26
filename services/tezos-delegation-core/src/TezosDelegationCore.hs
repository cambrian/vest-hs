module TezosDelegationCore
  ( module TezosDelegationCore
  ) where

import qualified AccessControl.Client
import qualified Postgres as Pg
import qualified Tezos
import TezosChainWatcher.Api
import TezosDelegationCore.Db
import TezosDelegationCore.Internal as TezosDelegationCore
import TezosHotWallet.Api
import Vest

platformFee :: Rational
platformFee = 0.5

blockConsumer :: T -> Consumers BlockEvents
-- ^ On each cycle, bill each delegate and update dividends for each delegator.
-- Does not issue dividends; dividends are paid out when the delegator pays its bill. This happens
-- in the payment consumer.
blockConsumer t@T {dbPool} =
  let getRewardInfo = makeClient t (Proxy :: Proxy RewardInfoEndpoint)
      handleCycle blockNum cycleNum cycleTime = do
        Pg.runLogged dbPool (Pg.wasHandled (cycles schema) cycleNum) >>=
          (`when` return ())
        delegates <- Pg.runLogged dbPool $ delegatesTrackedAtCycle cycleNum
        rewardInfos <- getRewardInfo $ RewardInfoRequest cycleNum delegates
        time <- now
        forM_ rewardInfos $ \Tezos.RewardInfo { delegate
                                              , reward
                                              , stakingBalance
                                              , delegatedBalance
                                              , delegations
                                              } -> do
          Pg.runLogged dbPool (wasRewardAlreadyProcessed cycleNum delegate) >>=
            (`when` return ())
          let delegateReward size =
                fixedOf Round $
                rationalOf reward ^* 0.1 ^* size ^/^ stakingBalance
              dividends_ =
                map
                  (\Tezos.DelegationInfo {delegator, size} ->
                     (delegator, delegateReward size))
                  delegations
              totalDividends =
                foldr
                  (\(_, size) tot -> tot + size)
                  (fixedQty @Mutez 0)
                  dividends_
              grossDelegateReward = reward - totalDividends
              vestCut =
                fixedOf Ceiling $ rationalOf grossDelegateReward ^* platformFee
              paymentOwed = totalDividends + vestCut
          billId <- nextUUID
          -- Insert entries into Pg.
          Pg.runLoggedTransaction dbPool $ do
            Pg.runInsert $
              Pg.insert
                (bills schema)
                (Pg.insertValues
                   [ Bill
                       billId
                       (DelegateAddress delegate)
                       paymentOwed
                       time
                       Nothing
                   ])
                Pg.onConflictDefault
            Pg.runInsert $
              Pg.insert
                (rewards schema)
                (Pg.insertValues
                   [ Reward
                       (CycleNumber cycleNum)
                       (DelegateAddress delegate)
                       reward
                       stakingBalance
                       delegatedBalance
                       (BillId billId)
                       time
                   ])
                Pg.onConflictDefault
            forM_ dividends_ $ \(delegator, size) ->
              Pg.runInsert $
              Pg.insert
                (dividends schema)
                (Pg.insertValues
                   [ Dividend
                       (CycleNumber cycleNum)
                       (DelegateAddress delegate)
                       delegator
                       size
                       (BillId billId)
                       (PayoutId Nothing)
                       time
                   ])
                Pg.onConflictDefault
        Pg.runLogged dbPool $
          Pg.runInsert $
          Pg.insert
            (cycles schema)
            (Pg.insertValues [Cycle cycleNum cycleTime blockNum blockNum time])
            Pg.onConflictDefault
      handleBlock Tezos.BlockEvent {number = blockNumber, cycleNumber, time} = do
        handleCycle blockNumber cycleNumber time
        Pg.runLogged dbPool $
          Pg.runUpdate $
          Pg.update
            (cycles schema)
            (\Cycle {latestBlock} -> [latestBlock Pg.<-. Pg.val_ blockNumber])
            (\Cycle {number} -> number Pg.==. Pg.val_ cycleNumber)
   in (Pg.runLogged dbPool selectNextBlock, handleBlock)

paymentConsumer :: T -> Consumers PaymentEvents
-- TODO: transaction fees
paymentConsumer t@T {dbPool} =
  let nextPayment = Pg.runLogged dbPool $ Pg.nextUnseenIndex $ payments schema
      issuePayout = makeClient t (Proxy :: Proxy PayoutEndpoint)
      f PaymentEvent {idx, from, size, time = paymentTime} = do
        Pg.runLogged dbPool (Pg.wasHandled (payments schema) idx) >>=
          (`when` return ())
        isPlatformDelegate_ <- Pg.runLogged dbPool $ isPlatformDelegate from
        time <- now
        when isPlatformDelegate_ $ do
          billsOutstanding_ <-
            Pg.runLogged dbPool $ billsOutstanding $ Tagged from
          let (billIdsPaid, refund) =
                foldr
                  (\Bill {id, size} (paid, rem) ->
                     if rem >= size
                       then (id : paid, rem - size)
                       else (paid, rem))
                  ([], size)
                  billsOutstanding_
          dividendsPaid <-
            foldr (<>) [] <$>
            mapM (Pg.runLogged dbPool . dividendsForBill) billIdsPaid
          -- Issue dividends and record issuance.
          -- TODO: Could group dividends by delegator.
          forM_ dividendsPaid $ \dividend@Dividend {delegator, size, payout} -> do
            when (isJust $ payoutId payout) $ return ()
            id <- nextUUID
            -- TODO: Make sure this can't fail.
            issuePayout $ PayoutRequest id (untag delegator) size
            Pg.runLoggedTransaction dbPool $ do
              Pg.runInsert $
                Pg.insert
                  (payouts schema)
                  (Pg.insertValues [Payout id time])
                  Pg.onConflictDefault
              Pg.runUpdate $
                Pg.save
                  (dividends schema)
                  (dividend {payout = PayoutId $ Just id} :: Dividend)
          -- Mark bills paidm
          Pg.runLogged dbPool $
            Pg.runUpdate $
            Pg.update
              (bills schema)
              (\Bill {paidAt} -> [paidAt Pg.<-. Pg.val_ (Just paymentTime)])
              (\Bill {id} -> id `Pg.in_` (Pg.val_ <$> billIdsPaid))
          -- Issue refund if necessarym
          when (refund > fixedQty @Mutez 0) $ do
            refundId <- nextUUID
            issuePayout $ PayoutRequest refundId from refund
            -- ^ TODO: Make sure this can't fail.
            Pg.runLoggedTransaction dbPool $ do
              Pg.runInsert $
                Pg.insert
                  (payouts schema)
                  (Pg.insertValues [Payout refundId time])
                  Pg.onConflictDefault
              Pg.runInsert $
                Pg.insert
                  (refunds schema)
                  (Pg.insertValues
                     [Refund (PaymentIndex idx) refund (PayoutId refundId) time])
                  Pg.onConflictDefault
        Pg.runLogged dbPool $
          Pg.runInsert $
          Pg.insert
            (payments schema)
            (Pg.insertValues [Payment idx time])
            Pg.onConflictDefault
   in (nextPayment, f)

instance Service T where
  type ValueSpec T = ()
  type EventsProduced T = ()
  type EventsConsumed T = BlockEvents
                          :<|> PaymentEvents
  type RpcSpec T = ()
  summary = "Tezos Delegation Core v0.1.0"
  description =
    "Tracks how much delegates owe their delegators for each reward event, and issues payouts on behalf of delegates."
  init configPaths f = do
    (accessControlPublicKey :<|> seed) <- load configPaths
    withLoadable configPaths $ \(dbPool :<|> amqp :<|> redis) -> do
      accessControlClient <-
        AccessControl.Client.make amqp accessControlPublicKey seed
      Pg.runLogged dbPool $ Pg.ensureSchema checkedSchema
      f $ T {dbPool, amqp, redis, accessControlClient}
  rpcHandlers _ = ()
  valuesPublished _ = ()
  eventProducers _ = ()
  eventConsumers t = blockConsumer t :<|> paymentConsumer t
