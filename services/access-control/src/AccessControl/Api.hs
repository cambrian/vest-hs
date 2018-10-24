module AccessControl.Api
  ( module AccessControl.Api
  ) where

import qualified AccessControl.Auth as Auth
import AccessControl.Internal
import qualified AccessControl.Permission as Permission
import qualified Transport.Amqp as Amqp
import Vest

type TokenEndpoint
   = Endpoint 'NoAuth T Amqp.T "token" PublicKey ('Direct SignedToken)

type InvalidateAllExistingTokensEndpoint
   = Endpoint ('Auth (Auth.T 'Permission.InvalidateAuthTokens)) T Amqp.T "invalidateAllExistingTokens" () ('Direct ())

type TokenVersionVariable = Variable T Amqp.T "minValidTokenTimestamp" Timestamp
