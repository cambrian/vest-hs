import AccessControl
import qualified ECDSA
import Vest

handlePubKey :: T -> () -> IO ECDSA.PublicKey
handlePubKey T {keys} () = return $ ECDSA.toPublicKey keys

main :: IO Void
main = start @T (const $ return ()) handlePubKey
