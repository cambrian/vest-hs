module Vest.Http
  ( module Vest.Http
  , module Reexports
  ) where

import qualified GHC.Base
import Network.HTTP.Client hiding (Proxy)
import Network.HTTP.Client.TLS
import Servant.API as Reexports hiding (Stream)
import Servant.Client
import Vest.Prelude.Core
import Vest.Prelude.Resource
import Vest.Prelude.Stream

data HttpClientException =
  HttpClientException Text
  deriving (Eq, Ord, Show, Read, Generic, Exception, Hashable, FromJSON, ToJSON)

-- Added because Scheme does not derive certain useful instances.
data SchemeType
  = HttpType
  | HttpsType
  deriving (Eq, Ord, Show, Read, Generic, Hashable, FromJSON, ToJSON)

data Config = Config
  { schemeType :: SchemeType
  , host :: Text' "Host"
  , port :: Int' "Port"
  , path :: Text' "Path"
  } deriving (Eq, Ord, Show, Read, Generic, Hashable, FromJSON, ToJSON)

data T = T
  { schemeType :: SchemeType
  , host :: Text' "Host"
  , port :: Int' "Port"
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
           HttpType -> defaultManagerSettings
           HttpsType -> tlsManagerSettings)
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
       (BaseUrl (schemeFromType schemeType) (unpack host) port (unpack path)))

direct :: ClientM result -> T -> IO result
direct requester t = do
  errorOrResult <- call requester t
  case errorOrResult of
    Left error -> throw error
    Right result -> return result

resultLoop ::
     ThreadId
  -> (result -> IO ())
  -> IO' "CloseStream" ()
  -> IO (Maybe (Either GHC.Base.String result))
  -> IO ()
resultLoop main push (Tagged close) pull = do
  resultMaybe <- pull
  case resultMaybe of
    Nothing -> close
    Just result ->
      case result of
        Left error ->
          close >> evilThrowTo main (HttpClientException (pack error))
        Right value -> push value >> resultLoop main push (Tagged close) pull

-- Only returns a stream when the first result has been received.
streaming :: ClientM (ResultStream result) -> T -> IO (Stream result)
streaming requester t = do
  (push, Tagged close, stream) <- pushStream
  receivedFirst <- newEmptyMVar
  main <- myThreadId
  let pushNotify x =
        void $ do
          push x
          tryPutMVar receivedFirst ()
  async $ do
    errorOrResult <- call requester t
    case errorOrResult of
      Left error -> close >> evilThrowTo main error
      Right (ResultStream results) ->
        results $ resultLoop main pushNotify (Tagged close)
  takeMVar receivedFirst
  return stream

-- Renaming to make things less opaque for the user.
request :: HasClient ClientM api => Proxy api -> Client ClientM api
request = client
