module PriceServer
  ( Config(..)
  , T
  , PriceVirtualStakeRequest(..)
  , PriceVirtualStakeEndpoint
  , PriceFunctionTopic
  , Priceable()
  , PriceableCurrencies
  , start
  , make
  ) where

import Bridge
import qualified Bridge.Transports.Amqp as Amqp
import PriceServer.Prelude
import qualified Streamly
import qualified Streamly.Prelude as Streamly
import VestPrelude
import qualified VestPrelude.Money as Money

-- Eventually this will live inside the exchanger
type TezosPriceTopic = Topic "price/XTZ" (Money.ExchangeRate "XTZ" "USD")

make :: Config -> Amqp.T -> IO T
make Config {} amqp = do
  dummyTezosExchangeRate <- fromJustUnsafe $ Money.exchangeRate 1
  (_, tezosPriceStream) <-
    subscribe (Proxy :: Proxy (TezosPriceTopic, amqp)) amqp
  tezosPrice <- makeStreamVar' dummyTezosExchangeRate tezosPriceStream
  return $ T {tezosPrice}

instance Priceable "XTZ" where
  priceVirtualStake T {tezosPrice} size duration = do
    xtzUsd <- readTVarIO tezosPrice
    let price = Money.exchange xtzUsd size
    return price

type PriceableCurrencies = (Proxy "XTZ", Proxy "mutez") -- extend with `:<|> (Proxy "ETH", Proxy "wei")
