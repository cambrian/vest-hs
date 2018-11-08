module Http
  ( module Http
  , module Reexports
  ) where

import qualified GHC.Base
import Network.HTTP.Client hiding (Proxy)
import Network.HTTP.Client.TLS
import Servant.API as Reexports hiding (Stream)
import Servant.Client hiding (ServantError(..))
import Servant.Client as Reexports (ServantError(..))
import Vest.Prelude.Core
import Vest.Prelude.Resource
import Vest.Prelude.Stream
import Vest.Prelude.Time

data HttpClientException =
  HttpClientException Text
  deriving (Eq, Ord, Show, Read, Generic, Exception, Hashable, FromJSON, ToJSON)

defaultRequestTimeout :: Time Millisecond
defaultRequestTimeout = ms 5000

-- Added because Scheme does not derive certain useful instances.
data SchemeType
  = HttpType
  | HttpsType
  deriving (Eq, Ord, Show, Read, Generic, Hashable, FromJSON, ToJSON)

data Config = Config
  { schemeType :: SchemeType
  , host :: Text' "Host"
  , port :: Word16' "Port"
  , path :: Text' "Path"
  } deriving (Eq, Ord, Show, Read, Generic, Hashable, FromJSON, ToJSON)

data T = T
  { schemeType :: SchemeType
  , host :: Text' "Host"
  , port :: Word16' "Port"
  , path :: Text' "Path"
  , manager :: Manager
  }

instance Resource T where
  type ResourceConfig T = Config
  make :: Config -> IO T
  make Config {schemeType, host, port, path} = do
    manager <-
      newManager
        (case schemeType of
           HttpType ->
             defaultManagerSettings
               {managerResponseTimeout = responseTimeoutNone}
           HttpsType ->
             tlsManagerSettings {managerResponseTimeout = responseTimeoutNone})
    -- ^ Override default timeout to none. Timeouts for HTTP requests should be explicitly
    -- specified in the code (and ideally at the handler level).
    return T {schemeType, host, port, path, manager}
  -- closeManager is apparently deprecated, since managers close on their own when they are no
  -- longer needed. Unclear how that works but okay.
  cleanup :: T -> IO ()
  cleanup _ = return ()

schemeFromType :: SchemeType -> Scheme
schemeFromType HttpType = Http
schemeFromType HttpsType = Https

call :: ClientM res -> T -> IO (Either ServantError res)
call requester T { schemeType
                 , host = Tagged host
                 , port = Tagged port
                 , path = Tagged path
                 , manager
                 } =
  runClientM
    requester
    (mkClientEnv
       manager
       (BaseUrl
          (schemeFromType schemeType)
          (unpack host)
          (fromIntegral port)
          (unpack path)))

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
