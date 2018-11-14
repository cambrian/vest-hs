module Http
  ( module Http
  , module Reexports
  ) where

import qualified GHC.Base
import Network.HTTP.Client hiding (Proxy)
import Network.HTTP.Client.TLS
import Servant.API as Reexports hiding (Stream)
import Servant.Client hiding (ServantError(..))
import Servant.Client as Reexports (Scheme(Http, Https), ServantError(..))
import Vest.Prelude.Core
import Vest.Prelude.Resource
import Vest.Prelude.Stream
import Vest.Prelude.Time

data HttpClientException =
  HttpClientException Text
  deriving (Eq, Ord, Show, Read, Generic, Exception, Hashable, FromJSON, ToJSON)

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

data T = T
  { scheme :: Scheme
  , host :: Text' "Host"
  , port :: Word16' "Port"
  , path :: Text' "Path"
  , manager :: Manager
  }

instance Resource T where
  type ResourceConfig T = Config
  make :: Config -> IO T
  make Config {scheme, host, port, path} = do
    manager <-
      newManager
        (case scheme of
           Http ->
             defaultManagerSettings
               {managerResponseTimeout = responseTimeoutNone}
           Https ->
             tlsManagerSettings {managerResponseTimeout = responseTimeoutNone})
    -- ^ Override default timeout to none. Timeouts for HTTP requests should be explicitly
    -- specified in the code (and ideally at the handler level).
    return T {scheme, host, port, path, manager}
  -- closeManager is apparently deprecated, since managers close on their own when they are no
  -- longer needed. Unclear how that works but okay.
  cleanup :: T -> IO ()
  cleanup _ = return ()

call :: ClientM res -> T -> IO (Either ServantError res)
call requester T { scheme
                 , host = Tagged host
                 , port = Tagged port
                 , path = Tagged path
                 , manager
                 } =
  runClientM
    requester
    (mkClientEnv
       manager
       (BaseUrl scheme (unpack host) (fromIntegral port) (unpack path)))

direct :: ClientM result -> T -> IO result
direct requester t = do
  errorOrResult <- call requester t
  case errorOrResult of
    Left error -> throw error
    Right result -> return result

resultLoop ::
     ThreadId
  -> (result -> IO ())
  -> IO ()
  -> IO (Maybe (Either GHC.Base.String result))
  -> IO ()
resultLoop caller push close pull = do
  resultMaybe <- pull
  case resultMaybe of
    Nothing -> close
    Just result ->
      case result of
        Left error ->
          close >> evilThrowTo caller (HttpClientException (pack error))
        Right value -> push value >> resultLoop caller push close pull

-- Only returns a stream when the first result has been received.
-- TODO: Figure out how to pass an exception thrower fn to resultLoop.
-- TODO: Consider encoding streaming/direct with the API route definitions.
streaming ::
     ClientM (ResultStream result) -> T -> IO (Stream QueueBuffer result)
streaming requester t = do
  (writer, stream) <- newStream
  receivedFirst <- newEmptyTMVarIO
  caller <- myThreadId
  let pushNotify x =
        void $ do
          writeStream writer x
          atomically $ tryPutTMVar receivedFirst ()
  async $ do
    errorOrResult <- call requester t
    case errorOrResult of
      Left error -> closeStream writer >> evilThrowTo caller error
      Right (ResultStream results) ->
        results $ resultLoop caller pushNotify $ closeStream writer
  atomically $ takeTMVar receivedFirst
  return stream

-- Renaming to make things less opaque for the user.
request :: HasClient ClientM api => Proxy api -> Client ClientM api
request = client
