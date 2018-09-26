import Bridge
import VestPrelude

data SpecTypes = SpecTypes
  { directOrStreaming :: TypeRep
  , auth :: TypeRep
  , route :: TypeRep
  , req :: TypeRep
  , res :: TypeRep
  } deriving (Show)

-- Used to iterate over the nested API structure, run a (possibly monadic) function on the types of
-- each endpoint, and optionally collect the results in a list.
class Collector spec where
  collect :: (SpecTypes -> c) -> Proxy spec -> [c]
  collectM :: (Monad m) => (SpecTypes -> m c) -> Proxy spec -> m [c]
  collectM fn = sequence . collect fn
  collectM_ :: (Monad m) => (SpecTypes -> m c) -> Proxy spec -> m ()
  collectM_ fn = void . collectM fn

instance (Collector a, Collector b) =>
         Collector (a
                    :<|> b) where
  collect ::
       (SpecTypes -> c)
    -> Proxy (a
              :<|> b)
    -> [c]
  collect fn _ = collect fn (Proxy :: Proxy a) ++ collect fn (Proxy :: Proxy b)

instance (Typeable t, Typeable auth, Typeable route, Typeable req, Typeable res) =>
         Collector (Endpoint (t :: DirectOrStreaming) (auth :: Auth *) (route :: k) req res) where
  collect :: (SpecTypes -> c) -> Proxy (Endpoint t auth route req res) -> [c]
  collect fn _ =
    [ fn
        SpecTypes
          { directOrStreaming = typeRep (Proxy :: Proxy t)
          , auth = typeRep (Proxy :: Proxy auth)
          , route = typeRep (Proxy :: Proxy route)
          , req = typeRep (Proxy :: Proxy req)
          , res = typeRep (Proxy :: Proxy res)
          }
    ]

-- Just a sample API ripped from the Bridge tests.
type EchoIntsDirectEndpoint
   = Endpoint 'Direct 'NoAuth "echoIntsDirect" [Int] [Int]

type EchoTextsDirectEndpoint
   = Endpoint 'Direct 'NoAuth "echoTextDirect" [Text] [Text]

type EchoIntsStreamingEndpoint
   = Endpoint 'Streaming ('Auth TokenAuth) "echoIntsStreaming" [Int] Int

type EchoTextsStreamingEndpoint
   = Endpoint 'Streaming ('Auth TokenAuth) "echoTextsStreaming" [Text] Text

type RpcApi
   = EchoIntsDirectEndpoint
     :<|> EchoTextsDirectEndpoint
     :<|> EchoIntsStreamingEndpoint
     :<|> EchoTextsStreamingEndpoint

main :: IO ()
main = collectM_ print (Proxy :: Proxy RpcApi)
