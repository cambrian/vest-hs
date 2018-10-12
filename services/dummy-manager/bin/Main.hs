import DummyManager
import qualified Streamly
import qualified Streamly.Prelude as Streamly
import Vest

addInts :: T -> AddIntsRequest -> IO Int
addInts _ AddIntsRequest {a, b} = do
  threadDelay (sec 0.25)
  return (a + b)

echoThrice :: T -> Int -> IO (Streamly.Serial Int)
echoThrice _ x = do
  threadDelay (sec 0.25)
  return . Streamly.fromList . replicate 3 $ x

concatTextAuth ::
     T
  -> VerifierClaims DummyAuth
  -> ConcatTextAuthRequest
  -> IO ConcatTextAuthResponse
concatTextAuth _ _ ConcatTextAuthRequest {a, b} =
  return $ ConcatTextAuthResponse {result = a <> b}

echoThriceAuth ::
     T -> VerifierClaims DummyAuth -> Text -> IO (Streamly.Serial Text)
echoThriceAuth _ _ = return . Streamly.fromList . replicate 3

handlers :: Handlers Api
handlers = addInts :<|> echoThrice :<|> concatTextAuth :<|> echoThriceAuth

main :: IO Void
main = start @T (const . return $ ()) handlers
