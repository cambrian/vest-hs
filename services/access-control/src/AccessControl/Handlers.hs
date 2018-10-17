module AccessControl.Handlers
  ( t
  ) where

import AccessControl
import qualified Data.HashMap.Strict as HashMap
import qualified Data.HashSet as HashSet
import Vest

accessToken :: T -> PublicKey -> IO Token
accessToken T {subjects, secretKey, tokenTTL} publicKey = do
  time <- now
  let Subject {name, permissions} =
        fromMaybe Subject {name = "unknown", permissions = HashSet.empty} $
        HashMap.lookup publicKey subjects
      access =
        Access
          {publicKey, name, permissions, expiration = timeAdd tokenTTL time}
      token = sign' secretKey (show' access)
  return token

t :: Handlers (RpcSpec T)
t = accessToken
