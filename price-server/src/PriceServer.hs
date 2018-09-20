module PriceServer
  ( Config(..)
  , T
  , PriceContractRequest(..)
  , Priceable()
  , start
  , make
  ) where

import Bridge
import qualified Bridge.Transports.Amqp as Amqp
import GHC.TypeLits
import PriceServer.Prelude
import qualified Streamly
import qualified Streamly.Prelude as Streamly
import VestPrelude
import qualified VestPrelude.Money as Money

data Config = Config
  {
  } deriving (Eq, Show, Read, Generic)

data T = T
  { tezosPrice :: TVar (Money.ExchangeRate "XTZ" "USD")
  }

instance Priceable "XTZ" T where
  priceContract T {tezosPrice} size duration = do
    xtzUsd <- readTVarIO tezosPrice
    let mutez = toRational size
        xtzUsdRaw = Money.exchangeRateToRational xtzUsd
        price = mutez * xtzUsdRaw
    return $ Money.dense' price

-- Eventually this will live inside the exchanger
type TezosPriceTopic = Topic "price/XTZ" (Money.ExchangeRate "XTZ" "USD")

make :: Config -> Amqp.T -> IO T
make Config {} amqp = do
  dummyTezosExchangeRate <- fromJustUnsafe $ Money.exchangeRate 1
  (_, tezosPriceStream) <-
    subscribe (Proxy :: Proxy (TezosPriceTopic, amqp)) amqp
  tezosPrice <- makeStreamVar' dummyTezosExchangeRate tezosPriceStream
  return $ T {tezosPrice}
