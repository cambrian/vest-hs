-- Currently intended as an executable for mocked testing data.
module TezosPlatformStats
  ( module TezosPlatformStats
  ) where

import qualified Data.HashMap.Strict as HashMap
import qualified Tezos
import TezosPlatformStats.Api as TezosPlatformStats
import TezosPlatformStats.Internal as TezosPlatformStats
import Vest

overview :: T -> () -> IO TezosPlatformStats.OverviewResponse
overview T {rawStubData} _ = do
  stubData <- deserializeUnsafe @'JSON rawStubData
  return $ overviewResponse stubData

bakersFn :: T -> () -> IO TezosPlatformStats.BakersResponse
bakersFn T {rawStubData} _ = do
  stubData <- deserializeUnsafe @'JSON rawStubData
  return $ bakersResponse stubData

implicit :: T -> Tezos.ImplicitAddress -> IO TezosPlatformStats.ImplicitResponse
implicit T {rawStubData} implicitPkh = do
  stubData <- deserializeUnsafe @'JSON rawStubData
  case HashMap.lookup implicitPkh (implicitResponse stubData) of
    Nothing -> throw $ InvalidCallException "no such implicit PKH"
    Just response -> return response

operation ::
     T
  -> Tezos.OperationHash
  -> IO (Stream ValueBuffer TezosPlatformStats.OperationResponse)
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

newtype RawStubData =
  RawStubData Text
  deriving newtype (IsString, FromJSON)

instance Loadable RawStubData where
  configFile = [relfile|stub-data.json|]

instance Service T where
  type RpcSpec T = OverviewEndpoint
                   :<|> BakersEndpoint
                   :<|> ImplicitEndpoint
                   :<|> OperationEndpoint
  type ValueSpec T = ()
  type EventsProduced T = ()
  type EventsConsumed T = ()
  summary = "Tezos Platform Stats v0.1.0"
  description = "Front-end stats server for Tezos."
  init configPaths f = do
    (RawStubData rawStubData) <- load configPaths
    withLoadable configPaths $ \(webSocket :<|> redis) ->
      f $ T {webSocket, redis, rawStubData, streamDelayMillis = 500}
  rpcHandlers t = overview t :<|> bakersFn t :<|> implicit t :<|> operation t
  valuesPublished _ = ()
  eventProducers _ = ()
  eventConsumers _ = ()

type PublicApi = RpcSpec T

type AuxiliaryTypes
   = Baker
     :<|> DelegateFraction
     :<|> DelegateInfo
     :<|> LedgerOperation
     :<|> LedgerOperationType
     :<|> OriginatedAddress
     :<|> TimeRate
     :<|> TimeSize
