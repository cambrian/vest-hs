import AccessControl (T, handlers, makeStreams)
import Vest

main :: IO Void
main = start @T makeStreams handlers
