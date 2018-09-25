module PriceServer.Prelude
  ( PriceContractRequest(..)
  , PriceContractEndpoint
  , PriceFunctionTopic
  , Priceable(..)
  , Config(..)
  , T(..)
  , start
  ) where

import Bridge
import qualified Bridge.Transports.Amqp as Amqp
import GHC.TypeLits
import qualified Streamly
import VestPrelude
import qualified VestPrelude.Money as Money

data PriceContractRequest (a :: Symbol) = PriceContractRequest
  { size :: Money.Dense a
  , duration :: Time Day
  } deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, Hashable)

data Config = Config
  {
  } deriving (Eq, Show, Read, Generic)

data T = T
  { tezosPrice :: TVar (Money.ExchangeRate "XTZ" "USD")
  }

type PriceContractRoute currency = AppendSymbol "priceContract/" currency

type PriceContractEndpoint currency
   = DirectEndpoint (PriceContractRoute currency) (PriceContractRequest currency) (Money.Dense "USD")

type PriceFunctionRoute currency = AppendSymbol "priceFunctions/" currency

type PriceFunctionTopic currency = Topic (PriceFunctionRoute currency) Text

class ( KnownSymbol a
      , KnownSymbol (PriceContractRoute a)
      , KnownSymbol (PriceFunctionRoute a)
      ) =>
      Priceable a
  where
  priceContract :: T -> Money.Dense a -> Time Day -> IO (Money.Dense "USD")
  priceFunctionsInJavascript :: T -> Proxy a -> Streamly.Serial Text
  priceContract' :: T -> PriceContractRequest a -> IO (Money.Dense "USD")
  priceContract' t PriceContractRequest {size, duration} =
    priceContract t size duration

type family Currencies currencies where
  Currencies (Proxy c) = '[ c]
  Currencies (a
              :<|> b) = Currencies a :++ Currencies b

type family NubCurrencies currencies where
  NubCurrencies (Proxy c) = '[ c]
  NubCurrencies (a
                 :<|> b) = Nub (NubCurrencies a :++ NubCurrencies b)

type HasUniqueCurrencies currencies
   = Currencies currencies ~ NubCurrencies currencies

class PriceServer currencies where
  start ::
       Amqp.T -- ^ rpcTransport
    -> Amqp.T -- ^ pubSubTransport
    -> Proxy currencies
    -> T
    -> IO ()
  -- ^ Takes separate rpcTransport and pubSubTransports even though they're both Amqp.T for now

instance ( HasUniqueCurrencies (a
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

instance (Priceable currency) => PriceServer (Proxy currency) where
  start :: Amqp.T -> Amqp.T -> Proxy (Proxy currency) -> T -> IO ()
  start rpcTransport pubSubTransport _ t = do
    serve
      (Proxy :: Proxy (PriceContractEndpoint currency, Amqp.T))
      rpcTransport
      (priceContract' t)
    publish
      (Proxy :: Proxy (PriceFunctionTopic currency, Amqp.T))
      pubSubTransport
      (priceFunctionsInJavascript t (Proxy :: Proxy currency))
