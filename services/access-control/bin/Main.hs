import AccessControl
import Vest

main :: IO Void
main = start @T (const $ return ()) ()
