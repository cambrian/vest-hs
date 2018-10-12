import AccessControl (T)
import qualified AccessControl.Handlers
import Vest

main :: IO Void
main = start @T (const $ return ()) AccessControl.Handlers.t
