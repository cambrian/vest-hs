module PriceServer
  ( Config(..)
  , PriceContractRequest(..)
  , Priceable(..)
  , start
  ) where

import qualified Bridge
import Streamly.Prelude as Streamly
import VestPrelude
import qualified VestPrelude.Money as Money

data PriceContractRequest (a :: Symbol) = PriceContractRequest
  { size :: Money.Dense a
  , duration :: Time Day
  } deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, Hashable)

class (KnownSymbol a) =>
      Priceable (a :: Symbol)
  where
  priceContract :: T -> Money.Dense a -> Time Day -> IO (Money.Dense "USD")
  priceContract' :: T -> PriceContractRequest a -> IO (Money.Dense "USD")
  priceContract' t PriceContractRequest {size, duration} =
    priceContract t size duration
  priceContractEndpoint :: Proxy a -> Route
  priceContractEndpoint = Route . ("priceContract" <>) . proxyText
  servePriceContract :: Proxy a -> Bridge.T -> T -> IO ()
  servePriceContract proxy bridge t =
    Bridge.serveRPC bridge (priceContractEndpoint proxy) (priceContract' @a t)

instance Priceable "XTZ" where
  priceContract T {tezosPrice} size duration = do
    xtzUsd <- readTVarIO tezosPrice
    let mutez = toRational size
        xtzUsdRaw = Money.exchangeRateToRational xtzUsd
        price = mutez * xtzUsdRaw
    return $ Money.dense' price

data Config = Config
  {
  } deriving (Eq, Show, Read, Generic)

data T = T
  { tezosPrice :: TVar (Money.ExchangeRate "XTZ" "USD")
  }

make :: Config -> Bridge.T -> IO T
make Config {} bridge = do
  dummyTezosExchangeRate <- fromJustUnsafe $ Money.exchangeRate 1
  tezosPrice <-
    Bridge.subscribe' bridge (Route "tezosPrice") >>=
    (makeStreamVar' dummyTezosExchangeRate . snd)
  return $ T {tezosPrice}

start :: Bridge.Config -> Config -> IO ()
start bridgeConfig config =
  Bridge.withForever
    bridgeConfig
    (\bridge -> do
       t <- make config bridge
       servePriceContract (Proxy :: Proxy "XTZ") bridge t)
