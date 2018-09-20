module PriceServer.Prelude
  ( PriceContractRequest(..)
  , Priceable(..)
  , start
  ) where

import Bridge
import qualified Bridge.Transports.Amqp as Amqp
import GHC.TypeLits
import qualified Streamly
import qualified Streamly.Prelude as Streamly
import VestPrelude
import qualified VestPrelude.Money as Money

data PriceContractRequest (a :: Symbol) = PriceContractRequest
  { size :: Money.Dense a
  , duration :: Time Day
  } deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, Hashable)

type PriceContractRoute currency = AppendSymbol "priceContract/" currency

type PriceContractEndpoint currency
   = DirectEndpoint (PriceContractRoute currency) (PriceContractRequest currency) (Money.Dense "USD")

type PriceFunctionRoute currency = AppendSymbol "priceFunctions/" currency

type PriceFunctionTopic currency = Topic (PriceFunctionRoute currency) Text

-- t should contain the necessary state to price a
class ( KnownSymbol a
      , KnownSymbol (PriceContractRoute a)
      , KnownSymbol (PriceFunctionRoute a)
      ) =>
      Priceable a t
  where
  priceContract :: t -> Money.Dense a -> Time Day -> IO (Money.Dense "USD")
  priceFunctionsInJavascript :: Proxy a -> t -> Streamly.Serial (Text)
  -- ^ Get a stream of serialized Javascript price functions for this currency
  priceContract' :: t -> PriceContractRequest a -> IO (Money.Dense "USD")
  priceContract' t PriceContractRequest {size, duration} =
    priceContract t size duration

type family Currencies currencies where
  Currencies (SymbolT c) = '[ c]
  Currencies (a
              :<|> b) = Currencies a :++ Currencies b

type family NubCurrencies currencies where
  NubCurrencies (SymbolT c) = '[ c]
  NubCurrencies (a
                 :<|> b) = Nub (NubCurrencies a :++ NubCurrencies b)

type HasUniqueCurrencies currencies
   = Currencies currencies ~ NubCurrencies currencies

-- Takes separate rpcTransport and pubSubTransports even though they're both Amqp.T for now
class PriceServer currencies t where
  start :: Proxy currencies -> t -> Amqp.T -> Amqp.T -> IO ()

instance ( HasUniqueCurrencies (a
                                :<|> b)
         , PriceServer a t
         , PriceServer b t
         ) =>
         PriceServer (a
                      :<|> b) t where
  start ::
       Proxy (a
              :<|> b)
    -> t
    -> Amqp.T
    -> Amqp.T
    -> IO ()
  start _ t rpcTransport pubSubTransport = do
    start (Proxy :: Proxy a) t rpcTransport pubSubTransport
    start (Proxy :: Proxy b) t rpcTransport pubSubTransport

instance Priceable currency t => PriceServer (SymbolT currency) t where
  start :: Proxy (SymbolT currency) -> t -> Amqp.T -> Amqp.T -> IO ()
  start _ t rpcTransport pubSubTransport = do
    serve
      (Proxy :: Proxy (PriceContractEndpoint currency, Amqp.T))
      rpcTransport
      (priceContract' @currency t)
    publish
      (Proxy :: Proxy (PriceFunctionTopic currency, Amqp.T))
      pubSubTransport
      (priceFunctionsInJavascript (Proxy :: Proxy currency) t)
