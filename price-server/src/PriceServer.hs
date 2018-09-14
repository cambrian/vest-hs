module PriceServer
  ( Config(..)
  , start
  ) where

import qualified Bridge
import Streamly.Prelude as Streamly
import VestPrelude
import qualified VestPrelude.Money as Money

data Config = Config
  { bridgeConfig :: Bridge.Config
  } deriving (Eq, Show, Read, Generic)

data PriceTezosContractRequest a = PriceTezosContractRequest
  { size :: Money.Dense "XTZ"
  , duration :: Time Day
  } deriving (Eq, Show, Read, Generic, ToJSON, FromJSON)

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
         (Route "priceContract")
         (\PriceTezosContractRequest {size, duration} -> do
            xtzUsd <- readTVarIO latestTezosPrice
            return $ tezosContractPrice xtzUsd size duration))

tezosContractPrice ::
     Money.ExchangeRate "XTZ" "USD"
  -> Money.Dense "XTZ"
  -> Time Day
  -> Money.Dense "USD"
tezosContractPrice xtzUsd size duration =
  let mutez = toRational size
      xtzUsdRaw = Money.exchangeRateToRational xtzUsd
      price = mutez * xtzUsdRaw
   in Money.dense' price
