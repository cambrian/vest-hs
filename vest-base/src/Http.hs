module Http
  ( module Http
  , module Reexports
  ) where

import Network.HTTP.Client hiding (Proxy)
import Network.HTTP.Client.TLS
import Servant.API as Reexports hiding (Stream)
import Servant.Client hiding (Client)
import Servant.Client as Reexports (Scheme(Http, Https), ServantError(..))
import qualified Servant.Client
import Vest.Prelude.Core
import Vest.Prelude.Resource
import Vest.Prelude.Time

defaultRequestTimeout :: Duration
defaultRequestTimeout = sec 5

deriving instance Read Scheme

instance ToJSON Scheme

instance FromJSON Scheme

data Config = Config
  { scheme :: Scheme
  , host :: Text' "Host"
  , port :: Word16' "Port"
  , path :: Text' "Path"
  } deriving (Eq, Show, Read, Generic, FromJSON, ToJSON)

newtype Client = Client
  { request :: forall a. ClientM a -> IO a
  }

type Request = ClientM

instance Resource Client where
  type ResourceConfig Client = Config
  make :: Config -> IO Client
  make Config {scheme, host, port, path} = do
    let managerSettings =
          case scheme of
            Http ->
              defaultManagerSettings
                {managerResponseTimeout = responseTimeoutNone}
            Https ->
              tlsManagerSettings {managerResponseTimeout = responseTimeoutNone}
    -- ^ Override default timeout to none. Timeouts for HTTP requests should be explicitly
    -- specified in the code (and ideally at the handler level).
    manager <- newManager managerSettings
    let clientEnv =
          mkClientEnv manager $
          BaseUrl
            scheme
            (unpack $ untag host)
            (fromIntegral port)
            (unpack $ untag path)
        call :: ClientM a -> IO a
        call req = runClientM req clientEnv >>= fromRightOrThrowLeft
    return $ Client call
  -- closeManager is apparently deprecated, since managers close on their own when they are no
  -- longer needed. Unclear how that works but okay.
  cleanup :: Client -> IO ()
  cleanup _ = return ()

-- Renaming to make things less opaque for the user.
buildRequest ::
     HasClient ClientM api => Proxy api -> Servant.Client.Client ClientM api
buildRequest = client
