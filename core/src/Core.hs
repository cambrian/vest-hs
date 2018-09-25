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

data T = T
  {
  }

class (PriceServer.Priceable a) =>
      Stakeable a
  -- storeNewContract ::
  --      (MonadSelda m)
  --   => Proxy a
  --   -> SeldaConnection
  --   -> Hash
  --   -> Money.Dense a
  --   -> Timestamp
  --   -> Days
  --   -> Money.Discrete "USD" "cent"
  --   -> m Id
  -- storeNewContract _ db owner size startTime duration price = do
  --   id <- liftIO newUuid
  --   insert_ users (Db.Contract {id, owner, size, startTime})
  --   return id
  where
  handleStake :: Amqp.T -> VirtualStakeRequest a -> IO (VirtualStakeResponse a)
  handleStake priceServerRpcTransport VirtualStakeRequest { user
                                                          , size
                                                          , duration
                                                          , payment
                                                          } = do
    let priceContract =
          makeClient
            (Proxy :: Proxy (PriceServer.PriceContractEndpoint a, Amqp.T)) -- TODO: export with transport from priceServer
            priceServerRpcTransport
    price <-
      priceContract $ PriceServer.PriceContractRequest {size, duration, payment}
    id <- newUuid @"VirtualStake"
    -- save to db
    return $ VirtualStakeResponse {}

make :: Config -> Amqp.T -> IO T
make Config {} bridge = do
  return $ T {}
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
