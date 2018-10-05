module FiatPaymentServer.Prelude
  ( PriceVirtualStakeRequest(..)
  , PriceVirtualStakeEndpoint
  , PriceFunctionTopic
  , Priceable(..)
  , Config(..)
  , T(..)
  , start
  ) where

import Bridge
import qualified Bridge.Transports.Amqp as Amqp
import qualified Streamly
import VestPrelude
import qualified VestPrelude.Money as Money

data PriceVirtualStakeRequest (currency :: Symbol) (unit :: Symbol) = PriceVirtualStakeRequest
  { size :: Money.Discrete currency unit
  , duration :: Time Day
  } deriving (Generic)

deriving instance
         Money.Unit c u => Eq (PriceVirtualStakeRequest c u)

deriving instance
         Money.Unit c u => Read (PriceVirtualStakeRequest c u)

deriving instance
         Money.Unit c u => Show (PriceVirtualStakeRequest c u)

instance Money.Unit c u => ToJSON (PriceVirtualStakeRequest c u)

instance Money.Unit c u => FromJSON (PriceVirtualStakeRequest c u)

data Config = Config
  {
  } deriving (Eq, Show, Read, Generic)

data T = T
  { tezosPrice :: TVar (Money.ExchangeRate "XTZ" "USD")
  }

type PriceVirtualStakeRoute currency
   = AppendSymbol "priceVirtualStake/" currency

type PriceVirtualStakeEndpoint currency unit
   = ( Endpoint 'NoAuth (PriceVirtualStakeRoute currency) (PriceVirtualStakeRequest currency unit) ('Direct (Money.Discrete "USD" "cent"))
     , Amqp.T)

type PriceFunctionRoute currency = AppendSymbol "priceFunctions/" currency

type PriceFunctionTopic currency
   = (Topic 'Haskell (PriceFunctionRoute currency) Text, Amqp.T)

class ( KnownSymbol currency
      , KnownSymbol (PriceVirtualStakeRoute currency)
      , KnownSymbol (PriceFunctionRoute currency)
      ) =>
      Priceable currency
  where
  priceVirtualStake ::
       T -> Money.Dense currency -> Time Day -> IO (Money.Dense "USD")
  priceFunctionsInJavascript :: T -> Proxy currency -> Streamly.Serial Text
  priceVirtualStake' ::
       (Money.Unit currency unit)
    => T
    -> PriceVirtualStakeRequest currency unit
    -> IO (Money.Discrete "USD" "cent")
  priceVirtualStake' t PriceVirtualStakeRequest {size, duration} = do
    let denseSize = Money.denseFromDiscrete size
    densePrice <- priceVirtualStake t denseSize duration
    return $ fst $ Money.discreteFromDense Money.Truncate densePrice

class PriceServer currencies where
  start ::
       Amqp.T -- ^ rpcTransport
    -> Amqp.T -- ^ pubSubTransport
    -> Proxy currencies
    -> T
    -> IO ()
  -- ^ Takes separate rpcTransport and pubSubTransports even though they're both Amqp.T for now

instance ( HasUniqueSymbols (a
                             :<|> b)
         , PriceServer a
         , PriceServer b
         ) =>
         PriceServer (a
                      :<|> b) where
  start ::
       Amqp.T
    -> Amqp.T
    -> Proxy (a
              :<|> b)
    -> T
    -> IO ()
  start rpcTransport amqpTransport _ t = do
    start rpcTransport amqpTransport (Proxy :: Proxy a) t
    start rpcTransport amqpTransport (Proxy :: Proxy b) t

instance (Priceable currency, Money.Unit currency unit) =>
         PriceServer (Proxy currency, Proxy unit) where
  start :: Amqp.T -> Amqp.T -> Proxy (Proxy currency, Proxy unit) -> T -> IO ()
  start rpcTransport pubSubTransport _ t = do
    serve
      (priceVirtualStake' t)
      (Proxy :: Proxy (PriceVirtualStakeEndpoint currency unit))
      rpcTransport
    publish
      (priceFunctionsInJavascript t (Proxy :: Proxy currency))
      (Proxy :: Proxy (PriceFunctionTopic currency))
      pubSubTransport
