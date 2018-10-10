import AccessControl
import Vest

handlePubKey :: () -> T -> IO (Text' "PublicKey")
handlePubKey () T {} =

main :: IO Void
main = start @T (const $ return ()) ()
