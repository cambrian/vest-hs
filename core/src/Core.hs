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

data T c u = T
  { priceVirtualStake :: PriceServer.PriceVirtualStakeRequest c u -> IO (Money.Discrete "USD" "cent")
  , dbPool :: Pool Db.T
  }

class (PriceServer.Priceable c, Money.Unit c u) =>
      Stakeable c u
  where
  make :: Amqp.T -> Pool Db.T -> Config -> IO (T c u)
  make amqp dbPool _config =
    let priceVirtualStake =
          (makeClient
             (Proxy :: Proxy (PriceServer.PriceVirtualStakeEndpoint c u))
             amqp)
            (sec 1)
            defaultHeaders
     in return T {priceVirtualStake, dbPool}
  stake :: T c u -> VirtualStakeRequest c u -> IO (VirtualStakeResponse c u)
  stake T {priceVirtualStake, dbPool} VirtualStakeRequest { user
                                                          , size
                                                          , duration
                                                          , payment
                                                          } = do
    time <- now
    price <-
      priceVirtualStake $ PriceServer.PriceVirtualStakeRequest {size, duration}
    -- TODO: process payment
    id <- newUuid @"VirtualStake"
    withResource
      dbPool
      (Db.storeVirtualStake @c @u id user size time duration price)
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
