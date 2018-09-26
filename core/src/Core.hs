module Core
  (
  ) where

import Bridge
import qualified Bridge.Transports.Amqp as Amqp
import Core.Api
import qualified Core.Db as Db
import qualified PriceServer
import VestPrelude
import qualified VestPrelude.Money as Money

data Config = Config
  {
  } deriving (Eq, Show, Read, Generic)

data T currency unit = T
  { priceVirtualStake :: PriceServer.PriceVirtualStakeRequest currency unit -> IO (Money.Discrete "USD" "cent")
  , dbPool :: Pool Db.T
  }

class (PriceServer.Priceable currency) =>
      Stakeable currency unit
  where
  make :: Amqp.T -> Pool Db.T -> Config -> IO (T currency unit)
  make amqp dbPool _config =
    let priceVirtualStake =
          makeClient
            (Proxy :: Proxy (PriceServer.PriceVirtualStakeEndpoint currency unit))
            amqp $
          (sec 1)
     in return T {priceVirtualStake, dbPool}
  stake ::
       T currency unit
    -> VirtualStakeRequest currency unit
    -> IO (VirtualStakeResponse currency unit)
  stake T {priceVirtualStake, dbPool} VirtualStakeRequest { user
                                                          , size
                                                          , duration
                                                          , payment
                                                          } = do
    price <-
      priceVirtualStake $ PriceServer.PriceVirtualStakeRequest {size, duration}
    -- TODO: process payment
    id <- newUuid @"VirtualStake"
    withResource
      dbPool
      (\(Db.T conn) ->
         runBeamPostgres conn $
         runInsert $
         insert
           (_coreUsers coreDb)
           (insertValues [user])
           (onConflict anyConflict onConflictDoNothing))
    -- save to db
    return $ VirtualStakeResponse {}
-- make :: Config -> Amqp.T -> IO T
-- make Config {} bridge = do
--   return $ T {}
-- start :: Db.PGConnectInfo -> Bridge.Config -> Config -> IO ()
-- start dbConfig bridgeConfig config =
--   Db.withForever
--     dbConfig
--     (\db -> do
--        Bridge.withForever
--          bridgeConfig
--          (\bridge -> do
--             t <- make config bridge
--             return ()))
