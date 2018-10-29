import AccessControl (T, handlers, makeValues)
import Vest

main :: IO Void
main = start @T handlers makeValues (const $ return ())
