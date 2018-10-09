import TezosDelegationCore
import TezosDelegationCore.Api
import TezosDispatcher.Api
import Vest

main :: IO Void
main = start @T (const $ return ()) ()
