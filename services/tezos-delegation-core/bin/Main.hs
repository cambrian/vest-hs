import TezosDelegationCore
import Vest

main :: IO Void
main =
  start
    @T
    handlers
    (const $ return ())
    (const $ return ())
    (panic "unimplemented")
