import TezosDelegationCore
import TezosDispatcher
import Vest

main :: IO Void
main = start @T (const $ return ()) ()
