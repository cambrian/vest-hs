import AccessControl (T, handlers, makeVariables)
import Vest

main :: IO Void
main = start @T handlers makeVariables (const $ return ())
