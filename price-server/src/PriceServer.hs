module PriceServer
  ( Config(..)
  , start
  ) where

import qualified Bridge
import qualified Control.Concurrent.STM.TVar as TVar
import qualified Control.Monad.STM as STM
import Streamly.Prelude as Streamly
import VestPrelude
import qualified VestPrelude.Money as Money

data Config = Config
  { bridgeConfig :: Bridge.Config
  } deriving (Eq, Show, Read, Generic)

data PriceTezosContractRequest = PriceTezosContractRequest
  { size :: Money.Dense "XTZ"
  , duration :: Time Day
  } deriving (Eq, Show, Read, Generic)

instance FromJSON PriceTezosContractRequest

instance ToJSON PriceTezosContractRequest

start :: Config -> IO ()
start Config {bridgeConfig} = do
  bridge <- Bridge.make bridgeConfig
  let Just dummyTezosExchangeRate = Money.exchangeRate 1
  let dummyTezosPrice = Streamly.fromList [dummyTezosExchangeRate]
  latestTezosPrice <- makeStreamVar' dummyTezosPrice dummyTezosExchangeRate
  Bridge.serveRPC
    bridge
    (Route "priceTezosContract")
    (\PriceTezosContractRequest {size, duration} -> do
       xtzUsd <- TVar.readTVarIO latestTezosPrice
       return $ tezosContractPrice xtzUsd size duration)
  return ()

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
