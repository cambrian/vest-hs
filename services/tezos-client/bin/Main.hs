import TezosClient
import Vest

main :: IO Void
main = start @T handlers (const $ return ()) (const $ return ())
