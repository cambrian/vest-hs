import Bridge
import VestPrelude

-- Used to iterate over the nested API structure, run a (possibly monadic) function on the types of
-- each endpoint, and optionally collect the results in a list.
class Collector spec where
  collect ::
       (Bool -> (TypeRep, TypeRep, TypeRep, TypeRep) -> c) -> Proxy spec -> [c]
  -- ^ Function takes isStream -> (format, path, request type, response type).
  collectM ::
       (Monad m)
    => (Bool -> (TypeRep, TypeRep, TypeRep, TypeRep) -> m c)
    -> Proxy spec
    -> m [c]
  collectM fn = sequence . collect fn
  collectM_ ::
       (Monad m)
    => (Bool -> (TypeRep, TypeRep, TypeRep, TypeRep) -> m c)
    -> Proxy spec
    -> m ()
  collectM_ fn = void . collectM fn

instance (Collector a, Collector b) =>
         Collector (a
                    :<|> b) where
  collect ::
       (Bool -> (TypeRep, TypeRep, TypeRep, TypeRep) -> c)
    -> Proxy (a
              :<|> b)
    -> [c]
  collect fn _ = collect fn (Proxy :: Proxy a) ++ collect fn (Proxy :: Proxy b)

instance (Typeable f, Typeable s, Typeable a, Typeable b) =>
         Collector (DirectEndpointAs (f :: Format) (s :: Symbol) a b) where
  collect ::
       (Bool -> (TypeRep, TypeRep, TypeRep, TypeRep) -> c)
    -> Proxy (DirectEndpointAs f s a b)
    -> [c]
  collect fn _ =
    [ fn
        False
        ( typeRep (Proxy :: Proxy f)
        , typeRep (Proxy :: Proxy s)
        , typeRep (Proxy :: Proxy a)
        , typeRep (Proxy :: Proxy b))
    ]

instance (Typeable f, Typeable s, Typeable a, Typeable b) =>
         Collector (StreamingEndpointAs (f :: Format) (s :: Symbol) a b) where
  collect ::
       (Bool -> (TypeRep, TypeRep, TypeRep, TypeRep) -> c)
    -> Proxy (StreamingEndpointAs f s a b)
    -> [c]
  collect fn _ =
    [ fn
        True
        ( typeRep (Proxy :: Proxy f)
        , typeRep (Proxy :: Proxy s)
        , typeRep (Proxy :: Proxy a)
        , typeRep (Proxy :: Proxy b))
    ]

-- Just a sample API ripped from the Bridge tests.
type EchoEndpoint = DirectEndpoint "echo" Text Text

type EchoIntsEndpoint = DirectEndpointJSON "echoInts" [Int] [Int]

type EchoInts'Endpoint = StreamingEndpoint "echoInts'" [Int] Int

type EchoCharsEndpoint = DirectEndpoint "echoChars" [Char] [Char]

type EchoChars'Endpoint = StreamingEndpoint "echoChars'" [Char] Char

type EchoTimeoutEndpoint = DirectEndpoint "echoTimeout" [Int] [Int]

type EchoTimeout'Endpoint = StreamingEndpoint "echoTimeout'" [Int] Int

type EchoDelay'Endpoint = StreamingEndpoint "echoDelay'" Int Int

type RpcApi
   = EchoEndpoint
     :<|> EchoIntsEndpoint
     :<|> EchoInts'Endpoint
     :<|> EchoCharsEndpoint
     :<|> EchoChars'Endpoint
     :<|> EchoTimeoutEndpoint
     :<|> EchoTimeout'Endpoint
     :<|> EchoDelay'Endpoint

main :: IO ()
main =
  collectM_ (\m (f, s, a, b) -> print (m, f, s, a, b)) (Proxy :: Proxy RpcApi)
