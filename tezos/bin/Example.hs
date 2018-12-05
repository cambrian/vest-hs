import qualified Http

-- import Test (ignoreIO)
import Tezos.Rpc
import Vest

publicTezosConfig :: ResourceConfig Http.Client
publicTezosConfig =
  Http.Config
    {scheme = Http.Https, host = "rpc.tezrpc.me", port = 443, path = ""}

main :: IO ()
main =
  with publicTezosConfig $ \connection -> do
    getRewardInfo connection 26 [Tagged "tz1RCFbB9GpALpsZtu6J58sb74dm8qe6XBzv"] >>=
      print
      -- Compare with https://api1.tzscan.io/v2/rewards_split
      -- tz1RCFbB9GpALpsZtu6J58sb74dm8qe6XBzv?cycle=20&p=0
    (blockEventStream, _, _) <- streamBlockEventsDurable connection 0 2
    -- cycleEventStream <- toCycleEventStream blockEventStream 42
    tapStream_ print blockEventStream
    -- tapStream_ print cycleEventStream
    waitStream blockEventStream
    -- waitStream cycleEventStream
