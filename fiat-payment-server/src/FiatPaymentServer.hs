module FiatPaymentServer
  ( Config(..)
  , T
  , PriceVirtualStakeRequest(..)
  , PriceVirtualStakeEndpoint
  , PriceFunctionTopic
  , Priceable()
  , PriceableCurrencies
  , start
  ) where

import Bridge
import qualified Bridge.Transports.Amqp as Amqp
import FiatPaymentServer.Prelude
import qualified Streamly
import qualified Streamly.Prelude as Streamly
import VestPrelude
import qualified VestPrelude.Money as Money

-- Eventually this will live inside the exchanger
type TezosPriceTopic
   = Topic 'Haskell "price/XTZ" (Money.ExchangeRate "XTZ" "USD")

type instance ResourceConfig T = (Config, Amqp.T)

instance Resource T where
  make :: (Config, Amqp.T) -> IO T
  make (Config {}, amqp) = do
    dummyTezosExchangeRate <- fromJustUnsafe BugException (Money.exchangeRate 1)
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
