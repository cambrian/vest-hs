module PriceServer
  ( Config(..)
  , PriceTezosContractRequest(..)
  , Priceable(..)
  , start
  ) where

import qualified Bridge
import Streamly.Prelude as Streamly
import VestPrelude
import qualified VestPrelude.Money as Money

data Config = Config
  { bridgeConfig :: Bridge.Config
  } deriving (Eq, Show, Read, Generic)

class (KnownSymbol a) =>
      Priceable a
  where
  price ::
       Money.ExchangeRate a "USD"
    -> Money.Dense a
    -> Time Day
    -> Money.Dense "USD"

data PriceTezosContractRequest = PriceTezosContractRequest
  { size :: Money.Dense "XTZ"
  , duration :: Time Day
  } deriving (Eq, Ord, Show, Read, Generic, Hashable, ToJSON, FromJSON)

instance Priceable "XTZ" where
  price xtzUsd size duration =
    let mutez = toRational size
        xtzUsdRaw = Money.exchangeRateToRational xtzUsd
        price = mutez * xtzUsdRaw
     in Money.dense' price

start :: Config -> IO ()
start Config {bridgeConfig} = do
  dummyTezosExchangeRate <- fromJustUnsafe $ Money.exchangeRate 1
  Bridge.withForever
    bridgeConfig
    (\bridge -> do
       latestTezosPrice <-
         Bridge.subscribe' bridge (Route "tezosPrice") >>=
         (makeStreamVar' dummyTezosExchangeRate . snd)
       Bridge.serveRPC
         bridge
         (Route "priceTezosContract")
         (\PriceTezosContractRequest {size, duration} -> do
            xtzUsd <- readTVarIO latestTezosPrice
            return $ price xtzUsd size duration))
