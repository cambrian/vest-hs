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

data Config = Config
  {
  } deriving (Eq, Show, Read, Generic)

-- T should contain any data used in pricing functions as TVars
data T = T
  { tezosPrice :: TVar (Money.ExchangeRate "XTZ" "USD")
  }

data (Priceable a) =>
     PriceContractRequest a = PriceContractRequest
  { size :: Money.Dense a
  , duration :: Time Day
  } deriving (Eq, Ord, Show, Read)

class (KnownSymbol a) =>
      Priceable a
  where
  priceContract :: T -> Money.Dense a -> Time Day -> IO (Money.Dense "USD")
  priceContract' :: T -> PriceContractRequest a -> IO (Money.Dense "USD")
  priceContract' t PriceContractRequest {size, duration} =
    priceContract t size duration

instance Priceable "XTZ" where
  priceContract T {tezosPrice} size duration = do
    xtzUsd <- readTVarIO tezosPrice
    let mutez = toRational size
        xtzUsdRaw = Money.exchangeRateToRational xtzUsd
        price = mutez * xtzUsdRaw
    return $ Money.dense' price

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
       Bridge.serveRPC
         bridge
         (Route "priceTezosContract")
         (priceContract' @"XTZ" t))
