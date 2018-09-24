import Bridge
import VestPrelude

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
