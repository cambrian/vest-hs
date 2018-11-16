module AccessControl
  ( module AccessControl
  ) where

import AccessControl.Api as AccessControl
import AccessControl.Internal as AccessControl
import qualified Data.HashMap.Strict as HashMap
import qualified Data.HashSet as HashSet
import Vest

accessToken :: T -> PublicKey -> IO SignedToken
accessToken T {subjects, secretKey} publicKey = do
  time <- now
  let Subject {name, permissions} =
        fromMaybe Subject {name = "unknown", permissions = HashSet.empty} $
        HashMap.lookup publicKey subjects
      token = Token {publicKey, name, permissions, time}
      signedToken = sign' secretKey (show' token)
  return signedToken

instance Service T where
  type RpcSpec T = TokenEndpoint
                   :<|> InvalidateAllExistingTokensEndpoint
  type ValueSpec T = TokenVersionValue
  type EventsProduced T = ()
  type EventsConsumed T = ()
  summary = "Access Control v0.1.0"
  description = "Internal role-based access control server."
  init configPaths f = do
    seed <- load configPaths
    subjects <- load configPaths
    -- (seed :<|> subjects) <- load configPaths
    let (publicKey, secretKey) = seedKeyPair seed
        acPublicKey = ACPublicKey publicKey
    (tokenTimeWriter, minTokenTime) <- newStream
    let bumpMinTokenTime = now >>= writeStream tokenTimeWriter
    bumpMinTokenTime
    withLoadable configPaths $ \(amqp :<|> redis) ->
      f $
      T
        { subjects
        , amqp
        , redis
        , publicKey = acPublicKey
        , secretKey
        , minTokenTime
        , bumpMinTokenTime
        }
  rpcHandlers t = accessToken t :<|> (\_claims () -> bumpMinTokenTime t)
  valuesPublished = minTokenTime
  eventProducers _ = ()
  eventConsumers _ = ()
