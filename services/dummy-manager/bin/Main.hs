import DummyManager
import Vest

main :: IO Void
main = start @T (const . return $ ()) handlers
