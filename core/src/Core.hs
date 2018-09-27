module Core
  ( module Core
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

class ( PriceServer.Priceable c
      , Money.Unit c u
      , KnownSymbol (VirtualStakeRoute c)
      ) =>
      Stakeable c u
  where
  make :: Config -> Amqp.T -> Pool Db.T -> IO (T c u)
  make _config amqp dbPool =
    let priceVirtualStake =
          (makeClient
             (Proxy :: Proxy (PriceServer.PriceVirtualStakeEndpoint c u))
             amqp)
            (sec 1)
            defaultHeaders
     in return T {priceVirtualStake, dbPool}
  handleStake ::
       T c u -> VirtualStakeRequest c u -> IO (VirtualStakeResponse c u)
  handleStake T {priceVirtualStake, dbPool} VirtualStakeRequest { user
                                                                , size
                                                                , duration
                                                                , payment
                                                                } = do
    time_ <- now
    price <-
      priceVirtualStake $ PriceServer.PriceVirtualStakeRequest {size, duration}
    -- TODO: process payment
    id <- newUuid @"VirtualStake"
    withResource
      dbPool
      (Db.storeVirtualStake @c @u id user size duration time_ price)
    return $ VirtualStakeResponse id user size duration time_ price
  start :: Amqp.T -> T c u -> IO ()
  start rpcTransport t =
    serve
      (Proxy :: Proxy (VirtualStakeEndpoint c u))
      (handleStake t)
      rpcTransport

instance Stakeable "XTZ" "mutez"
