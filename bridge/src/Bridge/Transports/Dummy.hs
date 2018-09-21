module Bridge.Transports.Dummy
  ( T(..)
  , localConfig
  ) where

import Bridge.PubSub
import Bridge.Rpc
import qualified Data.HashTable.IO as HashTable
import VestPrelude

type HashTable k v = HashTable.BasicHashTable k v

type Config = ()

localConfig :: Config
localConfig = ()

data T = T
  { requestVars :: HashTable Route (MVar (Id, Text)) -- Route to request.
  , responseVars :: HashTable Id (MVar Text) -- Call ID to response.
  }

type instance ResourceConfig T = Config

instance Resource T where
  hold :: Config -> IO T
  hold _ = do
    requestVars <- HashTable.new
    responseVars <- HashTable.new
    return T {requestVars, responseVars}
  release :: T -> IO ()
  release _ = return ()

instance RpcTransport T where
  _serve ::
       ((Text -> IO ()) -> x -> IO ())
       -- (publish -> res/Stream res -> IO ())
       -- Generic publisher on intermediate result x. Should encapsulate serializing the
       -- intermediate results.
    -> (Text -> Maybe req)
    -> Route
       -- Should throw if Route is already being served.
    -> T
    -> (req -> IO x)
    -> IO ()
    -- Should mutate t to store the details necessary for cleanup.
  _serve publisher deserialize route T {requestVars, responseVars} handler = do
    requestVar <-
      HashTable.lookup requestVars route >>= \case
        Nothing -> do
          requestVar <- newEmptyMVar
          HashTable.insert requestVars route requestVar
          return requestVar
        Just _ -> throw $ AlreadyServing route
    let pub id x = do
          HashTable.lookup responseVars id >>= \case
            Nothing -> panic "impossible code path"
            Just responseVar -> putMVar responseVar x
    void . async . forever $ do
      (id, reqText) <- takeMVar requestVar
      case deserialize reqText of
        Nothing -> panic "dummy being used improperly"
        Just r -> handler r >>= publisher (pub id)
  _call ::
       IO (res -> IO (), IO x, IO ())
       -- IO (push, result, done)
       -- Generic response handler that takes an intermediate result x and defines how to push it
       -- to the caller.
    -> (req -> Text)
    -> (Text -> IO res)
    -> Route
    -> T
    -> Time Second
       -- Timeout
    -> req
    -> IO x
    -- Registers a handler in the waiting RPC call hash table, and deregisters it after done
    -- resolves.
    -- Timeouts occur if a result or stream result is not received after the specified time.
  _call handler serialize deserializeUnsafe route T {requestVars, responseVars} _timeout req = do
    id <- newUuid
    responseVar <- newEmptyMVar
    HashTable.insert responseVars id responseVar
    (push, result, waitForDone) <- handler
    renewTimeout <- timeoutThrow' waitForDone _timeout
    responseThread <-
      async . forever $ do
        response <- takeMVar responseVar
        renewTimeout >> deserializeUnsafe response >>= push
    HashTable.lookup requestVars route >>= \case
      Nothing -> panic "dummy being used improperly"
      Just requestVar -> putMVar requestVar (id, serialize req)
    async $ waitForDone >> cancel responseThread
    result
