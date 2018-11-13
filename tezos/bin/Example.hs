import qualified Http
import Test (ignoreIO)
import Tezos
import Vest

publicTezosConfig :: ResourceConfig Http.T
publicTezosConfig =
  Http.Config
    {schemeType = Http.HttpsType, host = "rpc.tezrpc.me", port = 443, path = ""}

main :: IO ()
main = do
  connection <- make publicTezosConfig
  -- Compare with https://api1.tzscan.io/v2/rewards_split
  -- tz1RCFbB9GpALpsZtu6J58sb74dm8qe6XBzv?cycle=20&p=0
  ignoreIO $
    getRewardInfo connection 26 [Tagged "tz1RCFbB9GpALpsZtu6J58sb74dm8qe6XBzv"] >>=
    print
  blockEventStream <- streamNewBlockEventsDurable connection
  cycleEventStream <- toCycleEventStream blockEventStream 42
  async $ tapStream_ print blockEventStream
  async $ tapStream_ print cycleEventStream
  waitStream blockEventStream
  waitStream cycleEventStream
