-- Currently intended as an executable for mocked testing data.
module TezosStats
  ( module TezosStats
  ) where

-- import qualified AccessControl.Client as AccessControlClient
import qualified Data.HashMap.Strict as HashMap

-- import qualified Db
import qualified Tezos
import TezosStats.Api as TezosStats
import TezosStats.Internal as TezosStats

-- import qualified Transport.Amqp as Amqp
import Vest

type Api
   = OverviewEndpoint
     :<|> BakersEndpoint
     :<|> ImplicitEndpoint
     :<|> OperationEndpoint

overview :: T -> () -> IO TezosStats.OverviewResponse
overview T {rawStubData} _ = do
  stubData <- deserializeUnsafe @'JSON rawStubData
  return $ overviewResponse stubData

bakersFn :: T -> () -> IO TezosStats.BakersResponse
bakersFn T {rawStubData} _ = do
  stubData <- deserializeUnsafe @'JSON rawStubData
  return $ bakersResponse stubData

implicit :: T -> Tezos.ImplicitAddress -> IO TezosStats.ImplicitResponse
implicit T {rawStubData} implicitPkh = do
  stubData <- deserializeUnsafe @'JSON rawStubData
  case HashMap.lookup implicitPkh (implicitResponse stubData) of
    Nothing -> throw $ InvalidCallException "no such implicit PKH"
    Just response -> return response

operation ::
     T
  -> Tezos.OperationHash
  -> IO (Stream ValueBuffer TezosStats.OperationResponse)
operation T {rawStubData, streamDelayMillis} opHash = do
  stubData <- deserializeUnsafe @'JSON rawStubData
  case HashMap.lookup opHash (operationResponses stubData) of
    Nothing -> throw $ InvalidCallException "no such operation hash"
    Just streamOpList -> do
      (writer, stream) <- newStream
      async $
        mapM_
          (\x ->
             threadDelay (ms $ toRational streamDelayMillis) >>
             writeStream writer x)
          streamOpList >>
        closeStream writer
      return stream

type AuxiliaryTypes
   = Baker
     :<|> DelegateFraction
     :<|> DelegateInfo
     :<|> LedgerOperation
     :<|> LedgerOperationType
     :<|> OriginatedAddress
     :<|> TimeRate
     :<|> TimeSize

newtype RawStubData =
  RawStubData Text
  deriving newtype (IsString, FromJSON)

instance Loadable RawStubData where
  configFile = [relfile|stub-data.json|]

instance Service T where
  type ValueSpec T = ()
  type EventsProduced T = ()
  type EventsConsumed T = ()
  type RpcSpec T = Api
  summary = "tezos-stats v0.1.0"
  description = "Front-end stats server for Tezos."
  init configPaths f = do
    (RawStubData rawStubData) <- load configPaths
    withLoadable configPaths $ \(webSocket :<|> redis) ->
      f $ T {webSocket, redis, rawStubData, streamDelayMillis = 500}
  rpcHandlers t = overview t :<|> bakersFn t :<|> implicit t :<|> operation t
  valuesPublished _ = ()
  eventProducers _ = ()
  eventConsumers _ = ()
